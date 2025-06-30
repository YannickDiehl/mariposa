# 🔬 SPSS .sav Files Integrity Verification
# =========================================
# Comprehensive testing of generated .sav files

cat("🔬 SPSS .SAV FILES INTEGRITY VERIFICATION\n")
cat("=========================================\n\n")

# Set working directory
setwd("/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/tests/spss_reference/data")

# Load haven for .sav reading
if (!requireNamespace("haven", quietly = TRUE)) {
  cat("Installing haven...\n")
  install.packages("haven")
}
library(haven)

success_count <- 0
total_tests <- 0

# Helper function for test reporting
test_result <- function(test_name, condition, details = "") {
  total_tests <<- total_tests + 1
  if (condition) {
    cat("✅", test_name, "\n")
    if (details != "") cat("   ", details, "\n")
    success_count <<- success_count + 1
    return(TRUE)
  } else {
    cat("❌", test_name, "\n")
    if (details != "") cat("   ", details, "\n")
    return(FALSE)
  }
}

cat("📊 TEST 1: FILE EXISTENCE\n")
cat("-------------------------\n")

# Test 1: Check if .sav files exist
survey_exists <- file.exists("survey_data.sav")
test_result("Survey data file exists", survey_exists, 
           if(survey_exists) paste("Size:", round(file.info("survey_data.sav")$size/1024, 2), "KB") else "File not found")

longitudinal_exists <- file.exists("longitudinal_data.sav")
test_result("Longitudinal data file exists", longitudinal_exists,
           if(longitudinal_exists) paste("Size:", round(file.info("longitudinal_data.sav")$size/1024, 2), "KB") else "File not found")

cat("\n📖 TEST 2: HAVEN READ-BACK\n")
cat("---------------------------\n")

# Test 2: Haven read-back verification
if (survey_exists) {
  tryCatch({
    survey_readback <- read_sav("survey_data.sav")
    test_result("Survey data readable by haven", TRUE, 
               paste("Dimensions:", nrow(survey_readback), "x", ncol(survey_readback)))
    
    # Check specific variables
    has_gender <- "gender" %in% names(survey_readback)
    test_result("Gender variable present", has_gender)
    
    has_life_sat <- "life_satisfaction" %in% names(survey_readback)
    test_result("Life satisfaction variable present", has_life_sat)
    
    # Check for value labels (SPSS-specific)
    gender_labels <- attr(survey_readback$gender, "labels", exact = TRUE)
    has_labels <- !is.null(gender_labels)
    test_result("SPSS value labels preserved", has_labels,
               if(has_labels) paste("Labels:", paste(names(gender_labels), collapse = ", ")) else "No labels found")
    
  }, error = function(e) {
    test_result("Survey data readable by haven", FALSE, paste("Error:", e$message))
  })
} else {
  test_result("Survey data readable by haven", FALSE, "File does not exist")
}

if (longitudinal_exists) {
  tryCatch({
    longitudinal_readback <- read_sav("longitudinal_data.sav")
    test_result("Longitudinal data readable by haven", TRUE,
               paste("Dimensions:", nrow(longitudinal_readback), "x", ncol(longitudinal_readback)))
    
    # Check structure for repeated measures
    has_subject <- "subject_id" %in% names(longitudinal_readback) || any(grepl("subject", names(longitudinal_readback), ignore.case = TRUE))
    test_result("Subject identifier present", has_subject)
    
    has_time <- "time" %in% names(longitudinal_readback) || any(grepl("time", names(longitudinal_readback), ignore.case = TRUE))
    test_result("Time variable present", has_time)
    
  }, error = function(e) {
    test_result("Longitudinal data readable by haven", FALSE, paste("Error:", e$message))
  })
} else {
  test_result("Longitudinal data readable by haven", FALSE, "File does not exist")
}

cat("\n🔗 TEST 3: SPSS SYNTAX COMPATIBILITY\n")
cat("-------------------------------------\n")

# Test 3: Check compatibility with existing SPSS syntax files
syntax_dir <- "../syntax"
syntax_files <- list.files(syntax_dir, pattern = "\\.sps$", full.names = FALSE)

test_result("SPSS syntax directory exists", dir.exists(syntax_dir),
           paste("Found", length(syntax_files), "syntax files"))

if (length(syntax_files) > 0) {
  cat("   Available syntax files:\n")
  for (file in syntax_files) {
    cat("   -", file, "\n")
  }
  
  # Check if critical variables match syntax expectations
  if (exists("survey_readback")) {
    # Check for t-test variables (gender, life_satisfaction)
    t_test_ready <- has_gender && has_life_sat
    test_result("T-test syntax compatibility", t_test_ready,
               "Variables: gender, life_satisfaction available")
    
    # Check for descriptives variables
    desc_vars <- c("life_satisfaction", "age") %in% names(survey_readback)
    test_result("Descriptives syntax compatibility", all(desc_vars),
               paste("Available vars:", sum(desc_vars), "of", length(desc_vars)))
  }
}

cat("\n🎯 TEST 4: DATA INTEGRITY\n")
cat("--------------------------\n")

# Test 4: Data integrity checks
if (exists("survey_readback")) {
  # Check for missing values pattern
  missing_percent <- mean(is.na(survey_readback)) * 100
  reasonable_missing <- missing_percent < 50  # Less than 50% missing
  test_result("Reasonable missing values", reasonable_missing,
             paste("Missing data:", round(missing_percent, 1), "%"))
  
  # Check numeric ranges for key variables
  if (has_life_sat) {
    life_sat_range <- range(survey_readback$life_satisfaction, na.rm = TRUE)
    reasonable_range <- life_sat_range[1] >= 1 && life_sat_range[2] <= 7
    test_result("Life satisfaction in expected range (1-7)", reasonable_range,
               paste("Range:", paste(life_sat_range, collapse = " - ")))
  }
  
  # Check for duplicate rows (should be minimal)
  n_duplicates <- sum(duplicated(survey_readback))
  low_duplicates <- n_duplicates < (nrow(survey_readback) * 0.05)  # Less than 5%
  test_result("Low duplicate rate", low_duplicates,
             paste("Duplicates:", n_duplicates, "of", nrow(survey_readback)))
}

cat("\n📋 VERIFICATION SUMMARY\n")
cat("=======================\n")

success_rate <- round((success_count / total_tests) * 100, 1)
cat("Total tests:", total_tests, "\n")
cat("Successful:", success_count, "\n")
cat("Success rate:", success_rate, "%\n\n")

if (success_rate >= 80) {
  cat("🎉 VERIFICATION PASSED! .sav files are production-ready.\n")
  cat("✅ Files meet quality standards for SPSS validation.\n")
} else if (success_rate >= 60) {
  cat("⚠️  VERIFICATION PARTIAL. Some issues detected.\n")
  cat("🔧 Review failed tests and consider regeneration.\n")
} else {
  cat("❌ VERIFICATION FAILED. Major issues detected.\n")
  cat("🚨 .sav files require regeneration or investigation.\n")
}

cat("\n🚀 Next steps:\n")
cat("1. If PASSED: Run SPSS syntax files\n") 
cat("2. Export SPSS outputs as TXT\n")
cat("3. Test R parser functions\n")
cat("4. Integrate with testthat validation\n")

# Return success rate for scripting
success_rate