# Test SPSS Export Script
# ===============================================
# Simple test runner for spss_data_export.R

cat("Testing SPSS Data Export Script...\n\n")

# Test 1: Check if file exists
export_script <- "data/spss_data_export.R"
if (file.exists(export_script)) {
  cat("✓ Export script found:", export_script, "\n")
} else {
  cat("✗ Export script not found\n")
  stop("Cannot proceed without export script")
}

# Test 2: Check working directory
cat("✓ Current working directory:", getwd(), "\n")

# Test 3: Check data availability
data_files <- c("../../data/survey_data.rda", "../../data/longitudinal_data.rda")
for (file in data_files) {
  if (file.exists(file)) {
    cat("✓ Data file found:", basename(file), "\n")
  } else {
    cat("⚠ Data file missing:", basename(file), "\n")
  }
}

# Test 4: Check haven availability
haven_available <- requireNamespace("haven", quietly = TRUE)
cat("✓ Haven package available:", haven_available, "\n")

if (!haven_available) {
  cat("Installing haven...\n")
  tryCatch({
    install.packages("haven", repos = "https://cran.r-project.org/")
    haven_available <- requireNamespace("haven", quietly = TRUE)
    cat("✓ Haven installation successful:", haven_available, "\n")
  }, error = function(e) {
    cat("✗ Haven installation failed:", e$message, "\n")
  })
}

# Test 5: Source and run the export script
if (haven_available) {
  cat("\nRunning export script...\n")
  cat("==============================================\n")
  
  # Change to data directory for proper relative paths
  setwd("data")
  
  tryCatch({
    # Source the script
    source("spss_data_export.R", echo = FALSE)
    
    cat("\n==============================================\n")
    cat("Export script completed!\n")
    
    # Check for created files
    sav_files <- list.files(pattern = "\\.sav$")
    if (length(sav_files) > 0) {
      cat("\n✓ .sav files created:\n")
      for (file in sav_files) {
        size <- file.info(file)$size
        cat("  ", file, ":", round(size/1024, 2), "KB\n")
      }
    } else {
      cat("\n⚠ No .sav files found\n")
    }
    
  }, error = function(e) {
    cat("\n✗ Export script failed:", e$message, "\n")
  })
  
} else {
  cat("\n✗ Cannot run export without haven package\n")
}

cat("\nTest completed.\n")