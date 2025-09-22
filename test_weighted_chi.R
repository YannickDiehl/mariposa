library(dplyr)

# Load data
load("/Users/yannickdiehl/Documents/SoftwareProjekte/RPakete/SurveyStat/data/survey_data.rda")

# Get weighted crosstab using xtabs (what R does)
weighted_table <- xtabs(sampling_weight ~ gender + region, data = survey_data)
cat("R's weighted table (decimal values):\n")
print(weighted_table)
cat("\n")

# Perform chi-squared test (R's approach)
test_r <- chisq.test(weighted_table, correct = FALSE)
cat("R chi-squared with decimal weights:", test_r$statistic, "\n")
cat("R p-value:", test_r$p.value, "\n\n")

# Round weighted table like SPSS does
rounded_table <- round(weighted_table)
cat("Rounded weighted table (like SPSS):\n")
print(rounded_table)
cat("\n")

# Test with rounded values
test_rounded <- chisq.test(rounded_table, correct = FALSE)
cat("Chi-squared with rounded weights:", test_rounded$statistic, "\n")
cat("P-value with rounded weights:", test_rounded$p.value, "\n\n")

# SPSS actual values from output
spss_table <- matrix(c(249, 945, 260, 1062), nrow = 2, byrow = TRUE,
                      dimnames = list(c("Male", "Female"), c("East", "West")))
cat("SPSS table from output:\n")
print(spss_table)
cat("\n")

test_spss <- chisq.test(spss_table, correct = FALSE)
cat("Chi-squared with SPSS values:", test_spss$statistic, "\n")
cat("P-value with SPSS values:", test_spss$p.value, "\n")
