#!/bin/bash
# Description: Batch process zipped TIFF files by unzipping, masking zeros, and compressing.
# Usage: ./process_zipped_tifs.sh <input_dir> <output_dir>

# Input arguments
INPUT_DIR="$1"
OUTPUT_DIR="$2"
LOG_FILE="${OUTPUT_DIR}/processing.log"

# Check input arguments
if [ $# -ne 2 ]; then
  echo "Usage: $0 <input_dir> <output_dir>"
  exit 1
fi

# Create output directory and log file
mkdir -p "$OUTPUT_DIR"
touch "$LOG_FILE"

# Function to log messages with timestamps
log() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Process each ZIP file in input directory
for ZIP_FILE in "$INPUT_DIR"/*.zip; do
  # Skip non-ZIP files
  if [ ! -f "$ZIP_FILE" ]; then continue; fi

  log "Processing: $ZIP_FILE"

  # Create a unique temp directory for this file
  TEMP_DIR="/home/meng/Documents/temp_$(basename "$ZIP_FILE" .zip)_$(date +%s)"
  mkdir -p "$TEMP_DIR"

  # Step 1: Unzip using 7z (supports ZIP64)
  log "Unzipping to: $TEMP_DIR"
  7z x -o"$TEMP_DIR" "$ZIP_FILE" >> "$LOG_FILE" 2>&1
  if [ $? -ne 0 ]; then
    log "ERROR: Failed to unzip $ZIP_FILE"
    rm -rf "$TEMP_DIR"
    continue
  fi

  # Step 2: Find the extracted TIFF file
  TIF_PATH=$(find "$TEMP_DIR" -type f \( -iname "*.tif" -o -iname "*.tiff" \) | head -n 1)
  if [ -z "$TIF_PATH" ]; then
    log "ERROR: No TIFF found in $ZIP_FILE"
    rm -rf "$TEMP_DIR"
    continue
  fi

  # Step 3: Process the TIFF in R
  OUTPUT_TIF="${OUTPUT_DIR}/$(basename "$ZIP_FILE" .zip)_compressed.tif"
  log "Downsizing and compressing to: $OUTPUT_TIF"

  Rscript --vanilla - <<EOF >> "$LOG_FILE" 2>&1
  library(terra)
  tryCatch({
    r <- rast("$TIF_PATH")
    r_nonzero <- classify(r, cbind(0, NA))
    writeRaster(
      r_nonzero,
      filename = "$OUTPUT_TIF",
      NAflag = 0, # set NoData to 0
      overwrite = TRUE,
      gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=2",   # Use PREDICTOR=3 for floating-point data
    "ZLEVEL=9",      # Max compression level
    "NUM_THREADS=ALL_CPUS",
    "BIGTIFF=YES"    # Required for large files
  )
    )
    cat("Successfully wrote:", "$OUTPUT_TIF\n")
  }, error = function(e) {
    cat("ERROR:", e\$message, "\n")
  })
EOF

  # Step 4: Cleanup temp directory
  log "Cleaning up: $TEMP_DIR"
  rm -rf "$TEMP_DIR"
done

log "All files processed!"