# Generate Synthetic Longitudinal Data (Repeated Measures)
# ========================================================
# Creates a realistic repeated measures dataset suitable for
# testing rm_anova_test, rm_t_test, and related functions.

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(2024)

# Study design parameters
n_subjects <- 120  # Total number of subjects
n_timepoints <- 4  # Number of measurement occasions
n_groups <- 2      # Number of treatment groups

# Time point labels
timepoints <- paste0("T", 1:n_timepoints)

# Generate subject-level data
subjects <- tibble(
  subject_id = 1:n_subjects,
  
  # Treatment group assignment (balanced)
  group = factor(rep(c("Control", "Treatment"), each = n_subjects/2),
                levels = c("Control", "Treatment")),
  
  # Baseline characteristics
  age = round(rnorm(n_subjects, mean = 35, sd = 12)),
  gender = sample(c("Male", "Female"), n_subjects, replace = TRUE, prob = c(0.45, 0.55)),
  
  # Individual random effects (subject-specific intercepts)
  subject_intercept = rnorm(n_subjects, mean = 0, sd = 2.5),
  
  # Treatment effect size (Cohen's d ≈ 0.6)
  treatment_effect = ifelse(group == "Treatment", 3.0, 0),
  
  # Age effect
  age_effect = (age - mean(age)) * 0.1,
  
  # Gender effect  
  gender_effect = ifelse(gender == "Female", 1.2, 0)
)

# Generate time-varying effects
time_effects <- c(0, 0.8, 1.5, 2.0)  # Increasing over time
treatment_time_interaction <- c(0, 0.5, 1.8, 3.2)  # Treatment effect grows

# Create long format data
longitudinal_data <- subjects %>%
  # Expand to long format
  slice(rep(1:n(), each = n_timepoints)) %>%
  mutate(
    time = factor(rep(timepoints, n_subjects), levels = timepoints),
    time_numeric = as.numeric(time),
  ) %>%
  group_by(subject_id) %>%
  mutate(
    
    # Base outcome score with realistic correlations
    baseline_score = 25 + subject_intercept + age_effect + gender_effect,
    
    # Time effect
    time_effect = time_effects[time_numeric],
    
    # Treatment × Time interaction
    interaction_effect = ifelse(group == "Treatment", 
                               treatment_time_interaction[time_numeric], 
                               0),
    
    # Random measurement error (within-subject)
    measurement_error = rnorm(n(), mean = 0, sd = 1.8),
    
    # Autoregressive error component
    ar_error = case_when(
      time_numeric == 1 ~ rnorm(n(), 0, 1.0),
      time_numeric == 2 ~ 0.6 * lag(measurement_error) + rnorm(n(), 0, 0.8),
      time_numeric == 3 ~ 0.6 * lag(measurement_error) + rnorm(n(), 0, 0.8),
      time_numeric == 4 ~ 0.6 * lag(measurement_error) + rnorm(n(), 0, 0.8),
      TRUE ~ 0
    ),
    
    # Final outcome score
    outcome_score = baseline_score + time_effect + interaction_effect + 
                   measurement_error + ar_error,
    
    # Secondary outcome (correlated with primary)
    secondary_outcome = outcome_score * 0.7 + rnorm(n(), 0, 2.0),
    
    # Physiological measure (different scale)
    physio_measure = 40 + (outcome_score - 25) * 0.8 + rnorm(n(), 0, 3.5),
    
    # Questionnaire score (ordinal-like)
    questionnaire_score = pmax(1, pmin(7, 
      round(4 + (outcome_score - 25) * 0.15 + rnorm(n(), 0, 0.8)))),
    
    # Add some realistic missing data
    dropout = case_when(
      time_numeric == 1 ~ FALSE,  # No dropout at baseline
      time_numeric == 2 ~ runif(n()) < 0.05,  # 5% dropout by T2
      time_numeric == 3 ~ runif(n()) < 0.12,  # 12% by T3
      time_numeric == 4 ~ runif(n()) < 0.20,  # 20% by T4
      TRUE ~ FALSE
    )
  ) %>%
  
  # Handle dropout (cumulative)
  group_by(subject_id) %>%
  mutate(
    cumulative_dropout = cummax(dropout),
    
    # Set outcomes to NA after dropout
    outcome_score = ifelse(cumulative_dropout, NA, outcome_score),
    secondary_outcome = ifelse(cumulative_dropout, NA, secondary_outcome),
    physio_measure = ifelse(cumulative_dropout, NA, physio_measure),
    questionnaire_score = ifelse(cumulative_dropout, NA, questionnaire_score)
  ) %>%
  
  # Add occasional missing values (not due to dropout)
  mutate(
    outcome_score = ifelse(!cumulative_dropout & runif(n()) < 0.03, 
                          NA, outcome_score),
    secondary_outcome = ifelse(!cumulative_dropout & runif(n()) < 0.04, 
                              NA, secondary_outcome),
    physio_measure = ifelse(!cumulative_dropout & runif(n()) < 0.06, 
                           NA, physio_measure)
  ) %>%
  
  ungroup() %>%
  
  # Clean up temporary variables
  select(subject_id, group, age, gender, time, time_numeric,
         outcome_score, secondary_outcome, physio_measure, questionnaire_score) %>%
  
  # Convert to proper factor types
  mutate(
    subject_id = factor(subject_id),
    gender = factor(gender, levels = c("Male", "Female")),
    questionnaire_score = factor(questionnaire_score, levels = 1:7, ordered = TRUE)
  )

# Add variable labels
attr(longitudinal_data$subject_id, "label") <- "Subject ID"
attr(longitudinal_data$group, "label") <- "Treatment Group"
attr(longitudinal_data$age, "label") <- "Age in years"
attr(longitudinal_data$gender, "label") <- "Gender"
attr(longitudinal_data$time, "label") <- "Time point"
attr(longitudinal_data$time_numeric, "label") <- "Time point (numeric)"
attr(longitudinal_data$outcome_score, "label") <- "Primary outcome score"
attr(longitudinal_data$secondary_outcome, "label") <- "Secondary outcome score"
attr(longitudinal_data$physio_measure, "label") <- "Physiological measure"
attr(longitudinal_data$questionnaire_score, "label") <- "Questionnaire rating (1-7)"

# Create wide format version for some analyses
longitudinal_data_wide <- longitudinal_data %>%
  select(subject_id, group, age, gender, time, outcome_score) %>%
  tidyr::pivot_wider(
    names_from = time,
    values_from = outcome_score,
    names_prefix = "score_"
  )

# Save datasets
usethis::use_data(longitudinal_data, overwrite = TRUE)
usethis::use_data(longitudinal_data_wide, overwrite = TRUE)

# Print summary statistics
cat("Generated longitudinal_data with", nrow(longitudinal_data), "observations\n")
cat("Subjects:", length(unique(longitudinal_data$subject_id)), "\n")
cat("Time points:", length(unique(longitudinal_data$time)), "\n")
cat("Groups:", length(unique(longitudinal_data$group)), "\n")

cat("\nMissing data by time point:\n")
longitudinal_data %>%
  group_by(time) %>%
  summarise(
    n_total = n(),
    n_missing_primary = sum(is.na(outcome_score)),
    pct_missing = round(n_missing_primary / n_total * 100, 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nGroup means by time point:\n")
longitudinal_data %>%
  group_by(group, time) %>%
  summarise(
    n = sum(!is.na(outcome_score)),
    mean_score = round(mean(outcome_score, na.rm = TRUE), 2),
    sd_score = round(sd(outcome_score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()