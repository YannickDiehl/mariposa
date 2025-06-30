# Generate SPSS .sav Files - Direct Execution
# ===============================================
# Executes the export script to create actual .sav files

# Load required packages
library(haven)
library(dplyr)

# Load package data
devtools::load_all()

cat("==============================================\n")
cat("GENERATING SPSS .SAV FILES\n")
cat("==============================================\n\n")

# Load survey data
data(survey_data)
cat("✓ Loaded survey_data:", nrow(survey_data), "observations,", ncol(survey_data), "variables\n")

# Create SPSS-formatted version of survey_data
cat("Converting survey_data to SPSS format...\n")

spss_survey_data <- survey_data %>%
  # Ensure factor variables are properly labeled
  mutate(
    # Gender as numeric with value labels (SPSS standard)
    gender = as.numeric(factor(gender, levels = c("Male", "Female"))),
    
    # Region as numeric with value labels  
    region = as.numeric(factor(region, levels = c("East", "West"))),
    
    # Education as numeric with value labels
    education = as.numeric(factor(education, 
                                 levels = c("Low", "Medium", "High"),
                                 labels = c("Low", "Medium", "High"))),
    
    # All other variables remain numeric
    age = as.numeric(age),
    income = as.numeric(income),
    life_satisfaction = as.numeric(life_satisfaction),
    trust_government = as.numeric(trust_government),
    trust_media = as.numeric(trust_media),
    trust_science = as.numeric(trust_science),
    political_orientation = as.numeric(political_orientation),
    environmental_concern = as.numeric(environmental_concern),
    sampling_weight = as.numeric(sampling_weight)
  )

# Add SPSS value labels for categorical variables
attr(spss_survey_data$gender, "labels") <- c("Male" = 1, "Female" = 2)
attr(spss_survey_data$gender, "label") <- "Gender"

attr(spss_survey_data$region, "labels") <- c("East" = 1, "West" = 2)
attr(spss_survey_data$region, "label") <- "Region"

attr(spss_survey_data$education, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)
attr(spss_survey_data$education, "label") <- "Education Level"

# Add variable labels for all variables
attr(spss_survey_data$age, "label") <- "Age in years"
attr(spss_survey_data$income, "label") <- "Monthly income"
attr(spss_survey_data$life_satisfaction, "label") <- "Life satisfaction (1-7 scale)"
attr(spss_survey_data$trust_government, "label") <- "Trust in government (1-7 scale)"
attr(spss_survey_data$trust_media, "label") <- "Trust in media (1-7 scale)"
attr(spss_survey_data$trust_science, "label") <- "Trust in science (1-7 scale)"
attr(spss_survey_data$political_orientation, "label") <- "Political orientation (1-7 scale)"
attr(spss_survey_data$environmental_concern, "label") <- "Environmental concern (1-7 scale)"
attr(spss_survey_data$sampling_weight, "label") <- "Sampling weight"

# Export to SPSS format
output_file <- "tests/spss_reference/data/survey_data.sav"
haven::write_sav(spss_survey_data, output_file)

cat("✓ Survey data exported to:", output_file, "\n")
cat("  Variables:", ncol(spss_survey_data), "\n")
cat("  Observations:", nrow(spss_survey_data), "\n\n")

# Check if longitudinal data exists and export it
tryCatch({
  data(longitudinal_data)
  cat("Converting longitudinal_data to SPSS format...\n")
  
  spss_longitudinal_data <- longitudinal_data %>%
    mutate(
      # Subject ID as numeric
      subject_id = as.numeric(factor(subject_id)),
      
      # Group as numeric with labels
      group = as.numeric(factor(group, levels = c("Control", "Treatment"))),
      
      # Time as numeric
      time = as.numeric(time),
      
      # All score variables as numeric
      score = as.numeric(score)
    )
  
  # Add SPSS value labels
  attr(spss_longitudinal_data$group, "labels") <- c("Control" = 1, "Treatment" = 2)
  attr(spss_longitudinal_data$group, "label") <- "Treatment Group"
  
  # Add variable labels
  attr(spss_longitudinal_data$subject_id, "label") <- "Subject identifier"
  attr(spss_longitudinal_data$time, "label") <- "Time point"
  attr(spss_longitudinal_data$score, "label") <- "Outcome score"
  
  # Export to SPSS format
  longitudinal_output_file <- "tests/spss_reference/data/longitudinal_data.sav"
  haven::write_sav(spss_longitudinal_data, longitudinal_output_file)
  
  cat("✓ Longitudinal data exported to:", longitudinal_output_file, "\n")
  cat("  Variables:", ncol(spss_longitudinal_data), "\n")
  cat("  Observations:", nrow(spss_longitudinal_data), "\n\n")
  
}, error = function(e) {
  cat("✗ Longitudinal data not available or failed to export:", e$message, "\n\n")
})

# Verify files were created
cat("File verification:\n")
if (file.exists(output_file)) {
  file_info <- file.info(output_file)
  cat("  survey_data.sav: ✓ Created, size =", round(file_info$size / 1024, 1), "KB\n")
} else {
  cat("  survey_data.sav: ✗ Failed to create\n")
}

if (exists("longitudinal_output_file") && file.exists(longitudinal_output_file)) {
  file_info <- file.info(longitudinal_output_file)
  cat("  longitudinal_data.sav: ✓ Created, size =", round(file_info$size / 1024, 1), "KB\n")
}

cat("\n==============================================\n")
cat("SPSS .SAV FILE GENERATION COMPLETE\n")
cat("==============================================\n")
cat("Ready for SPSS validation!\n")
cat("Next steps:\n")
cat("1. Load .sav files in SPSS\n")
cat("2. Run syntax files from tests/spss_reference/syntax/\n")
cat("3. Export SPSS results as TXT files\n")
cat("4. Run validation tests in R\n")