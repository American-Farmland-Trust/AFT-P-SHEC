# Use process_zipped_tifs.sh to convert all tifs to compressed tifs to save memory and disk during the job
# all processed compressed tifs are stored in one folder

## ----setup, include=FALSE--------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE)
# Load packages, specify fxn for finding the mode, create a list of files ----

library(tidyverse)
library(sf)
library(fasterize)
library(raster)
library(tigris)
library(fuzzyjoin)
library(parallel)
library(readr)
library(dplyr)
#library(terra)
library(data.table)
#library(gdalUtils)

# Point raster temp files to a custom folder
# Monitor/clean that directory if necessary as this may right intermediate files to disk
# without touching other system temp files
dir.create("/home/meng/Documents/updated_PSHEC/tmpdir_2", recursive = TRUE, showWarnings = FALSE)
rasterOptions(tmpdir = "/home/meng/Documents/updated_PSHEC/tmpdir_2")

# ----Define mode function----------####
mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- na.omit(unique(x))
  return(ux[which.max(tabulate(match(x, ux)))])
}

counties <- tigris::counties(year = 2022)


# Read crop county list file
crop_county <- read.csv(file='/home/meng/Documents/updated_PSHEC/gSSURGO/all_10_crops_county_list_2000_2023.csv')
crop_county$GEOID <- formatC(crop_county$GEOID, width = 5, format = 'd' ,flag = '0')
# Convert only the crop column names (columns after GEOID) to lowercase
colnames(crop_county)[3:ncol(crop_county)] <- tolower(colnames(crop_county)[3:ncol(crop_county)])

# Define the output directory ------####
output_dir <- '/home/meng/Documents/updated_PSHEC/gSSURGO/processed_soil/'

# Ensure the directory exists before the loop starts
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Define the crop soil processing function-----####

process_crop_1 <- function(crop, crop_county) {
  # Load required packages INSIDE the function
  require(dplyr)
  require(data.table)
  require(raster)
  require(rlang)
  
  # Define function that takes individual county gSSURGO vector, converts to raster, 
  # masks/resamples that raster by crop cells,
  # then computes summary stats
  soil.stats.gather <- function(i){
    
    # If i is NULL or empty, return an empty dataframe
    if (is.null(i) || nrow(i) == 0) {
      warning("Empty dataset encountered, skipping GEOID.")
      return(NULL) 
      #return(data.frame(GEOID = NA))  # Return a placeholder with expected structure
    }
    
    # Set GEOID variable for function environment
    temp.GEOID <- unique(i$GEOID)
    
    if (length(temp.GEOID) == 0) {
      warning("No GEOID found, returning empty dataframe.")
      return(data.frame(GEOID = NA))
    }
    
    # Set boundary object for the county
    ibound <- st_transform(subset(counties,GEOID %in% temp.GEOID), st_crs(crop.freq))
    # Create reclassification matrix for converting crop frequency data into binary raster
    # binary for had 2+ years of corn data or did not. to identify pixels from any field in
    # which crop grown with relative consistency but not opportunistically 
    # while also eliminating non-crop areas of counties
    
    # ML: the original code (from Dan Kane's paper) has a bug; where the no corn cell value =255 is also considered as corn cells; 
    # this can corrected with the following reclassification matrix
    rcl.m <- matrix(c(-Inf,2, NA,
                      2, 254,1,
                      254,Inf,NA), 
                    ncol=3, 
                    byrow=TRUE)
    
    # Create mask object
    imask <-
      reclassify(projectRaster(mask(crop(crop.freq, y = ibound), ibound), res = 30, crs = crs(ibound)), rcl.m)
    
    # Check if imask has no valid cells (all NA)
    
    if (all(is.na(values(imask)))) {
      warning("imask has no valid cells (all NA), returning NULL.")
      return(NULL)
    }
    
    # Convert dataframe to SF object
    i <- st_as_sf(i)
    
    # Create blank raster to rasterize sf to
    irast <- raster(imask)
    
    # ML: order needs to be a 'double' type, recode as numbers
    i <- i %>%
      mutate(order = dplyr::recode(order, 
                                   "Alfisol"= 1,
                                   "Entisols" = 2,
                                   "Histosols"=3,
                                   "Inceptisols"= 4,
                                   "Mollisols" = 5,
                                   "Spodosols"= 6,
                                   "Ultisols" = 7,
                                   "Vertisols" = 8,
                                   "Aridisols" = 9),# Set unmatched values to NA
             drainage_class = dplyr::recode(drainage_class,
                                            "Excessively drained" = 1,
                                            "Somewhat excessively drained" = 2,
                                            "Well drained" = 3,
                                            "Moderately well drained" = 4,
                                            "Somewhat poorly drained" = 5,
                                            "Poorly drained" = 6,
                                            "Very poorly drained" = 7))# Set unmatched values to NA
    
    
    # Convert the county-level data to a raster stack
    temp.raster <- raster::stack(fasterize(sf = i, raster = irast, field = "soc"),
                                 fasterize(sf = i, raster = irast, field = "clay"),
                                 fasterize(sf = i, raster = irast, field = "sand"),
                                 fasterize(sf = i, raster = irast, field = "silt"),
                                 fasterize(sf = i, raster = irast, field = "om"),
                                 fasterize(sf = i, raster = irast, field = "awc"),
                                 fasterize(sf = i, raster = irast, field = "aws"),
                                 fasterize(sf = i, raster = irast, field = "fifteenbar"),
                                 fasterize(sf = i, raster = irast, field = "cec"),
                                 fasterize(sf = i, raster = irast, field = "ph"),
                                 fasterize(sf = i, raster = irast, field = "slope"),
                                 fasterize(sf = i, raster = irast, field = "drainage_class"),
                                 fasterize(sf = i, raster = irast, field = "order"))
    
    
    # Rename layers in the raster stack
    names(temp.raster) <-
      c("soc",
        "clay",
        "sand",
        "silt",
        "om",
        "awc",
        "aws",
        "fifteenbar",
        "cec",
        "ph",
        "slope",
        "drainage_class",
        "order")
    
    # Mask temp.raster to just the crop cells in the county
    temp.raster <- mask(temp.raster, mask = imask)
    
    # check before computing statistics to ensure raster data exists
    if (all(is.na(values(temp.raster)))) {
      warning("All raster values are NA, returning NULL.")
      return(NULL)
    }
    
    # Then, we will mask the temp.raster to just the cells with <10% om
    om_mask <- reclassify(subset(temp.raster, 'om'), cbind(10, Inf, NA))
    temp.raster.om <- mask(temp.raster, mask = om_mask)
    
    # Summarize soils data for each county
    # cellStats does not provide median on very large raster objects, use getValues+apply instead if needed
    
    # Add checks to prevent issues with cellStats() returning empty values
    safe_cellStats <- function(raster, stat) {
      result <- tryCatch({
        as.data.frame(t(cellStats(raster, stat)))
      }, error = function(e) {
        warning("cellStats failed, returning NA")
        return(data.frame(NA))
      })
    }
    
    soil.stats.temp <- cbind(temp.GEOID,
                             safe_cellStats(subset(temp.raster, c(1:11)), 'mean') %>%
                               rename_all(~paste(.,"mean", sep = "_")),
                             safe_cellStats(subset(temp.raster, c(12:13)), mode) %>%
                               rename_all(~paste(.,"mode", sep = "_")),
                             safe_cellStats(subset(temp.raster.om, c(1:11)), 'mean') %>%
                               rename_all(~paste(.,"filter_mean", sep = "_")),
                             safe_cellStats(subset(temp.raster.om, c(12:13)), mode) %>%
                               rename_all(~paste(.,"filter_mode", sep = "_")),
                             area_ha = ifelse(length(temp.raster.om) > 0, dim(temp.raster.om[temp.raster.om$om > 0])[1] * 30 * 30 / 10000, NA),
                             cs_no_na = ifelse(length(temp.raster) > 0, length(which(!is.na(getValues(temp.raster$om)), TRUE)), NA),
                             cs_om10_no_na = ifelse(length(temp.raster.om) > 0, length(which(!is.na(getValues(temp.raster.om$om)), TRUE)), NA)
    )
    
    # Add logs in soil.stats.gather() to confirm it actually returns a dataframe:
    cat("soil.stats.gather: returning data frame with", nrow(soil.stats.temp), "rows\n")
    
    # remove objects to free memory
    rm(i, imask,om_mask, temp.raster,temp.raster.om, ibound, irast)
    
    gc()
    
    # Return
    return(soil.stats.temp)
    
  }
  
  
  # Ensure crop column exists in the dataset
  if (!crop %in% colnames(crop_county)) {
    stop(paste("Crop column", crop, "not found in the dataset!"))
  }
  
  # Filter counties where the crop is present (value == 1)
  filtered_counties <- crop_county %>%
    dplyr::filter(!!rlang::sym(crop) == 1) %>%
    dplyr::select(GEOID)  # Keep only GEOID column
  
  # Convert to a vector of GEOID values
  geoids_to_process <- filtered_counties$GEOID
  
  # Check if there are counties to process
  if (length(geoids_to_process) == 0) {
    warning(paste("No counties found for crop:", crop))
    return(NULL)
  }
  
  # load the corn frequency raster from CropScape - for corn, soybeans, wheat, and cotton
  crop.freq <- brick(paste0('/home/meng/Documents/updated_PSHEC/processed_tifs/',crop,'_compressed.tif'))
  
  ## ----soil------------------------------------------------------------------------------------------
  #list files in the county_gssurgo_gdbs folder
  #Use Sys.glob() for faster lookup:
  #all_counties_rds_list <- list.files("~/Documents/updated_PSHEC/gSSURGO/county_gssurgo_gdbs/", full.names = T, pattern = ".rds")
  all_counties_rds_list <- Sys.glob("/home/meng/Documents/updated_PSHEC/gSSURGO/county_gssurgo_gdbs/*.rds")
  
  # Filter RDS files based on GEOID
  rds_files_to_process <- all_counties_rds_list[
    grepl(paste(geoids_to_process, collapse = "|"), all_counties_rds_list)
  ]
  
  if (length(rds_files_to_process) == 0) {
    warning(paste("No matching RDS files found for crop:", crop))
    return(NULL)
  }
  
  # run soil.stats.gather function on each list;
  #gssurgo.soil.stats <- mclapply(mc.cores = 8, all_counties_rds_list, function(x){
  #      soil.stats.gather(i = read_rds(x))
  #  })
  
  # if mclapply does not work; divide the list to chunks and run by lapply
  chunks <- split(rds_files_to_process, ceiling(seq_along(rds_files_to_process)/20)) # divide list to run chunks size of 20
  
  # loop through the list and run the data gather function
  # define a empty dataframe
  #gssurgo.soil.stats <- list()
  
  for (n in seq_along(chunks)) {
    
    rds_files_to_process <- chunks[[n]]
    
    # Add Logging for Debugging & Monitoring
    cat("Processing chunk", n, "for crop:", crop, "(", length(rds_files_to_process), "files)\n")
    
    dat <- lapply(rds_files_to_process, function(x) {
      result <- tryCatch({
        # Print or log the file name to see if it's processed
        cat("  Processing file:", x, "\n")
        soil.stats.gather(i = read_rds(x))
      }, error = function(e) {
        warning(paste("Error processing file:", x, "Skipping..."))
        return(NULL)
      })
      return(result)
    })
    
    # Check how many valid data frames we got
    cat("Chunk", n, ": got", length(dat), "results before filtering NULL\n")
    
    # Remove NULL elements
    dat <- Filter(Negate(is.null), dat)
    
    cat("Chunk", n, ": got", length(dat), "valid data frames after filtering NULL\n")
    
    # This step may append many dat in memory 
    # Append results only if non-empty
    #if (length(dat) > 0) {
    #  gssurgo.soil.stats[[n]] <- dat
    #}
    
    # Try save intermediate files to disk
    if (length(dat) > 0) {
      # Ensure the temp_files folder exists
      # Construct folder path
      temp_folder <- paste0("/home/meng/Documents/updated_PSHEC/gSSURGO/", crop, "_temp_files")
      
      # Ensure the folder exists
      dir.create(temp_folder, recursive = TRUE, showWarnings = FALSE)
      
      # Save the data
      output_file <- file.path(temp_folder, paste0(crop, "_chunk_", n, ".rds"))
      cat("Saving chunk", n, "with", length(dat), "data frames to:", output_file, "\n")  # Log file save attempt
      
      saveRDS(dat, file = output_file)
      
      # remove automatically intermediate temp files to free disk
      # not sure if setting h =0 will cause the loop to remove some files that other cores are using 
      # leading to missing counties in crops; try set h = 0.5 and try again to be safe (remove files from 0.5 hour ago)
      removeTmpFiles(h=0.5)
      
    } else {
      cat("Chunk", n, "has no valid data; skipping save.\n")
    }
    
    #return(gssurgo.soil.stats)
    # remove objects to free memory
    rm(dat)
    
    gc()
    
  }
  
  # Merge only the saved files, avoiding unnecessary in-memory storage
  files <- list.files(
    paste0("/home/meng/Documents/updated_PSHEC/gSSURGO/",crop,"_temp_files/"), 
    pattern = paste0(crop, "_chunk_.*\\.rds$"), full.names = TRUE)
  
  # Read and flatten all saved chunk files
  all_data <- lapply(files, function(f) {
    df <- readRDS(f)  # Each `chunk` is a list of data frames
    if (!is.list(df)) {
      warning(paste("Skipping file:", f, "because it is not a list of data frames"))
      return(NULL)
    }
    return(df)  # Keep as a list of data frames
  })
  
  # Flatten the nested list
  all_data <- unlist(all_data, recursive = FALSE)
  
  # Merge all data frames into one
  gssurgo.soil.stats <- data.table::rbindlist(all_data, fill = TRUE)
  
  #gssurgo.soil.stats <- unlist(gssurgo.soil.stats, recursive = FALSE) # convert list to dataframe
  
  #Modify the renaming step to only rename if temp.GEOID exists:
  if ("temp.GEOID" %in% colnames(gssurgo.soil.stats)) {
    gssurgo.soil.stats <- gssurgo.soil.stats %>%
      rename(GEOID = temp.GEOID)
  } else {
    warning("⚠️ Column `temp.GEOID` not found. Skipping rename.")
  }
  
  # Merge stats dfs and write to the hard drive
  gssurgo.soil.stats <- gssurgo.soil.stats %>%
    #plyr::ldply(gssurgo.soil.stats) %>%
    #rename("GEOID" = temp.GEOID) %>%
    rename_at(.vars = vars(2:27), .funs = function(x) paste("ssurgo", x, sep = "_")) %>%
    mutate(ratio_om10toCS = cs_om10_no_na/cs_no_na) %>%
    mutate(cell_kept_perc90 = case_when(ratio_om10toCS >= 0.9 ~ 'Y',TRUE ~ 'N')) %>%
    mutate(ssurgo_order_filter_mode = as.character(ssurgo_order_filter_mode),
           ssurgo_order_filter_mode = dplyr::recode(ssurgo_order_filter_mode, 
                                                    '1' = "Alfisol",
                                                    '2' = "Entisols",
                                                    '3' = "Histosols",
                                                    '4' = "Inceptisols",
                                                    '5' = "Mollisols",
                                                    '6' = "Spodosols",
                                                    '7' = "Ultisols",
                                                    '8' = "Vertisols",
                                                    '9' = "Aridisols")) %>%
    mutate(ssurgo_drainage_class_filter_mode = as.character(ssurgo_drainage_class_filter_mode),
           ssurgo_drainage_class_filter_mode = dplyr::recode(ssurgo_drainage_class_filter_mode, 
                                                             '1' = "Excessively drained",
                                                             '2' = "Somewhat excessively drained",
                                                             '3' = "Well drained",
                                                             '4' = "Moderately well drained",
                                                             '5' = "Somewhat poorly drained",
                                                             '6' = "Poorly drained",
                                                             '7' = "Very poorly drained")) %>%
    dplyr::select(-cs_no_na,-cs_om10_no_na) 
  
  saveRDS(gssurgo.soil.stats, file = paste0("data/processed_soil/",crop,"_gssurgo.soil.stats.area_cdl_om10.rds"))
  
  
  cat("Processing complete for crop:", crop, "\n")
  
  gc()
  
}


# Loop over the crop files provided by cropscape, -------####
# calling the process_crop_1 function for each crop

crops <- c('barley','oats','peanuts','sorghum', 'tobacco','sugarbeets')

# Process each crop
# for loop is slow, but more memory efficient; if large RAM and multiple cores, use mclapply
#for (crop in crops) {
#  process_crop_1(crop, crop_county=crop_county)
#}

# Parallel processing, only work on Linux and Mac, not for Windows
#mclapply(X= crops, process_crop_1, mc.cores = 4, MoreArgs = list(crop_county = crop_county)) # adjust cores based on the system

#alternative options for parallel computing: for all systems
#install.packages("future.apply")  # If not already installed
library(future.apply)

# Use all available cores: multisession will create multiple R sessions, needs large memory 
# multicore works for linux and mac, more memory-efficient
plan(multicore, workers =8)  

# Parallel version of lapply
future_lapply(crops, function(crop) {
  process_crop_1(crop, crop_county = crop_county)
}, future.seed = TRUE)

# To manually clear the directory:
unlink("/home/meng/Documents/updated_PSHEC/tmpdir_2", recursive = TRUE)
