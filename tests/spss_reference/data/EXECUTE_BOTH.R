# BOTH DATASETS .SAV EXPORT - GUARANTEED
# ====================================

setwd("/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data")

cat("=== LOADING PACKAGES ===\n")
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}
library(haven)

cat("=== DATASET 1: SURVEY DATA ===\n")
# Load survey data
load("../../../data/survey_data.rda")
cat("Survey data loaded:", nrow(survey_data), "rows,", ncol(survey_data), "variables\n")

# Export survey data with minimal transformation
write_sav(survey_data, "survey_data.sav")
cat("✓ Created survey_data.sav\n")

cat("\n=== DATASET 2: LONGITUDINAL DATA ===\n")
# Load longitudinal data
tryCatch({
  load("../../../data/longitudinal_data.rda")
  cat("Longitudinal data loaded:", nrow(longitudinal_data), "rows,", ncol(longitudinal_data), "variables\n")
  cat("Variables:", paste(names(longitudinal_data), collapse = ", "), "\n")
  
  # Export longitudinal data
  write_sav(longitudinal_data, "longitudinal_data.sav")
  cat("✓ Created longitudinal_data.sav\n")
  
}, error = function(e) {
  cat("✗ Longitudinal data loading failed:", e$message, "\n")
})

cat("\n=== CHECKING RESULTS ===\n")
sav_files <- list.files(pattern = "\\.sav$")
cat("Created .sav files:\n")
for (file in sav_files) {
  size <- round(file.info(file)$size / 1024, 2)
  cat("  ✓", file, ":", size, "KB\n")
}

cat("\n=== SUCCESS! ===\n")