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
# Monitor/clean that directory if necessary as this may write intermediate files to disk
# without touching other system temp files
dir.create("/home/meng/Documents/updated_PSHEC/tmpdir", recursive = TRUE, showWarnings = FALSE)
rasterOptions(tmpdir = "/home/meng/Documents/updated_PSHEC/tmpdir")

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
crop_county <- read.csv(file='/home/meng/Documents/updated_PSHEC/bulk_density/all_10_crops_merged_data_county_list_ordered_2000_2023.csv')
crop_county$GEOID <- formatC(crop_county$GEOID, width = 5, format = 'd' ,flag = '0')
# Convert only the crop column names (columns after GEOID) to lowercase
colnames(crop_county)[3:(ncol(crop_county)-1)] <- tolower(colnames(crop_county)[3:(ncol(crop_county)-1)])

# Define the output directory ------####
output_dir <- '/home/meng/Documents/updated_PSHEC/bulk_density/'

# Ensure the directory exists before the loop starts
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# load the corn frequency raster from CropScape - for corn, soybeans, wheat, and cotton
crop.freq <- brick(paste0('bulk_density/2024_Cultivated_Layer/2024_cultivated_layer.tif'))

# Define the crop soil processing function-----####

#process_crop_1 <- function(crop_county) {
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
    #Categorization Code   Land Cover
    #"1"       Non-Cultivated
    #"2"       Cultivated
    
    # ML: the original code (from Dan Kane's paper) has a bug; where the no corn cell value =255 is also considered as corn cells; 
    # this can corrected with the following reclassification matrix
    rcl.m <- matrix(c(1, NA,
                      2, 1), 
                    ncol=2, 
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
    
    # Convert the county-level data to a raster stack
    temp.raster <- raster::stack(fasterize(sf = i, raster = irast, field = "bd"))
    
    
    # Rename layers in the raster stack
    names(temp.raster) <-
      c('bd')
    
    # Mask temp.raster to just the crop cells in the county
    temp.raster <- mask(temp.raster, mask = imask)
    
    # check before computing statistics to ensure raster data exists
    if (all(is.na(values(temp.raster)))) {
      warning("All raster values are NA, returning NULL.")
      return(NULL)
    }
    
    # Then, we will mask the temp.raster to just the cells with <10% om
    #om_mask <- reclassify(subset(temp.raster, 'om'), cbind(10, Inf, NA))
    #temp.raster.om <- mask(temp.raster, mask = om_mask)
    
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
    
    val <- safe_cellStats(subset(temp.raster, 1), 'mean')
    raster_name <- names(temp.raster)[1]
    colnames(val) <- paste0(raster_name, "_mean")
    soil.stats.temp <- cbind(temp.GEOID, val)
    
    # Add logs in soil.stats.gather() to confirm it actually returns a dataframe:
    cat("soil.stats.gather: returning data frame with", nrow(soil.stats.temp), "rows\n")
    
    # remove objects to free memory
    rm(i, imask, temp.raster,ibound, irast)
    
    gc()
    
    # Return
    return(soil.stats.temp)
    
  }
  
  
  ## ----soil------------------------------------------------------------------------------------------
  #list files in the county_gssurgo_gdbs folder
  #Use Sys.glob() for faster lookup:
  #all_counties_rds_list <- list.files("~/Documents/updated_PSHEC/gSSURGO/county_gssurgo_gdbs/", full.names = T, pattern = ".rds")
  all_counties_rds_list <- Sys.glob("/home/meng/Documents/updated_PSHEC/bulk_density/county_gssurgo_gdbs/*.rds")
  
  # run soil.stats.gather function on each list;
  #gssurgo.soil.stats <- mclapply(mc.cores = 8, all_counties_rds_list, function(x){
  #      soil.stats.gather(i = read_rds(x))
  #  })
  
  # if mclapply does not work; divide the list to chunks and run by lapply
  chunks <- split(all_counties_rds_list, ceiling(seq_along(all_counties_rds_list)/50)) # divide list to run chunks size of 20
  
  # loop through the list and run the data gather function
  # define a empty dataframe
  #gssurgo.soil.stats <- list()
  
  for (n in seq_along(chunks)) {
    
    all_counties_rds_list <- chunks[[n]]
    
    # Add Logging for Debugging & Monitoring
    #cat("Processing chunk", n, "for crop:", crop, "(", length(all_counties_rds_list), "files)\n")
    
    dat <- lapply(all_counties_rds_list, function(x) {
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
      temp_folder <- "/home/meng/Documents/updated_PSHEC/bulk_density/temp_files"
      
      # Ensure the directory exists before the loop starts
      if (!dir.exists(temp_folder)) {
        dir.create(temp_folder, recursive = TRUE)
      }
      
      # Ensure the folder exists
      #dir.create(temp_folder, recursive = TRUE, showWarnings = FALSE)
      
      # Save the data
      output_file <- file.path(temp_folder, paste0("chunk_", n, ".rds"))
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
    paste0("/home/meng/Documents/updated_PSHEC/bulk_density/temp_files/"), 
    pattern = paste0("chunk_.*\\.rds$"), full.names = TRUE)
  
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
  
  write.csv(gssurgo.soil.stats, file = "/home/meng/Documents/updated_PSHEC/bulk_density/bulk_density_by_county_gssurgo_2024.csv", row.names = FALSE)
  
  gc()
  
#}


#process_crop_1(crop_county)

# To manually clear the directory:
unlink("/home/meng/Documents/updated_PSHEC/tmpdir", recursive = TRUE)

# add state and county name
bd <- read.csv('bulk_density/bulk_density_by_county_gssurgo_2024.csv')
bd$GEOID <- formatC(bd$GEOID, width = 5, format = 'd' ,flag = '0')

data(fips_codes) #ML: this is a dataset from the package tigris
fips_codes #ML: 3247 observations
fips_codes <- fips_codes %>%
  mutate(CH.GEOID = paste(state,county_code, sep = ""), 
         GEOID = paste(state_code,county_code, sep = "")) %>%
  #filter(GEOID %in% unique(nass_15$GEOID)) %>%  
  mutate(county = str_remove(county, " County")) 

bd_1 <- bd %>% 
  left_join(fips_codes, by = 'GEOID') %>%
  dplyr::select(state,state_name, GEOID, county, bd_mean)

write.csv(bd_1, file = 'bulk_density/bulk_density_by_county_gssurgo_2024.csv', row.names = FALSE)
