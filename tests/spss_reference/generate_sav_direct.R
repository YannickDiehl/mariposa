# Direct SPSS .sav Generation - Alternative Method
# ===============================================
# Simple script to generate .sav files using only required packages

# Check and load required packages
if (!require(haven, quietly = TRUE)) {
  stop("Haven package required for .sav export")
}

# Change to package directory
setwd("/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat")

# Load the package data directly
load("data/survey_data.rda")

cat("==============================================\n")
cat("DIRECT SPSS .SAV FILE GENERATION\n") 
cat("==============================================\n\n")

cat("✓ Loaded survey_data:", nrow(survey_data), "observations,", ncol(survey_data), "variables\n")

# Show structure for verification
cat("Variables:", paste(names(survey_data), collapse = ", "), "\n\n")

# Create SPSS-compatible version
cat("Converting to SPSS format...\n")

# Simple numeric conversion for SPSS
spss_data <- data.frame(
  age = as.numeric(survey_data$age),
  gender = as.numeric(factor(survey_data$gender, levels = c("Male", "Female"))),
  region = as.numeric(factor(survey_data$region, levels = c("East", "West"))),
  education = as.numeric(factor(survey_data$education, levels = c("Low", "Medium", "High"))),
  income = as.numeric(survey_data$income),
  life_satisfaction = as.numeric(survey_data$life_satisfaction),
  trust_government = as.numeric(survey_data$trust_government),
  trust_media = as.numeric(survey_data$trust_media),
  trust_science = as.numeric(survey_data$trust_science),
  political_orientation = as.numeric(survey_data$political_orientation),
  environmental_concern = as.numeric(survey_data$environmental_concern),
  sampling_weight = as.numeric(survey_data$sampling_weight)
)

# Add SPSS labels
attr(spss_data$gender, "labels") <- c("Male" = 1, "Female" = 2)
attr(spss_data$region, "labels") <- c("East" = 1, "West" = 2)
attr(spss_data$education, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)

# Create output directory if needed
if (!dir.exists("tests/spss_reference/data")) {
  dir.create("tests/spss_reference/data", recursive = TRUE)
}

# Export to SPSS
output_file <- "tests/spss_reference/data/survey_data.sav"
haven::write_sav(spss_data, output_file)

# Verify creation
if (file.exists(output_file)) {
  file_size <- file.info(output_file)$size
  cat("✓ SUCCESS: survey_data.sav created\n")
  cat("  File size:", round(file_size / 1024, 1), "KB\n")
  cat("  Variables:", ncol(spss_data), "\n")
  cat("  Observations:", nrow(spss_data), "\n")
  cat("  Location:", output_file, "\n")
} else {
  cat("✗ FAILED: Could not create survey_data.sav\n")
}

cat("\n==============================================\n")
cat("GENERATION COMPLETE\n")
cat("==============================================\n")