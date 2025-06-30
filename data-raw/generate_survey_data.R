# Generate Synthetic Survey Data (ALLBUS-style)
# =============================================
# Creates a realistic German social survey dataset similar to ALLBUS
# with demographics, attitudes, and proper survey design features.

library(dplyr)
library(tibble)

# Set seed for reproducibility
set.seed(2024)

# Sample size
n <- 2500

# Generate synthetic survey data
survey_data <- tibble(
  # Unique identifier
  id = 1:n,
  
  # Demographics
  age = pmax(18, pmin(95, round(rnorm(n, mean = 50, sd = 18)))),
  gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)),
  
  # German regions (East/West)
  region = sample(c("East", "West"), n, replace = TRUE, prob = c(0.20, 0.80)),
  
  # Education levels (German system)
  education = sample(
    c("Hauptschule", "Realschule", "Gymnasium", "University"), 
    n, replace = TRUE, 
    prob = c(0.35, 0.25, 0.25, 0.15)
  ),
  
  # Monthly household income (EUR)
  income = pmax(800, pmin(8000, round(
    case_when(
      education == "Hauptschule" ~ rnorm(n, 2800, 800),
      education == "Realschule" ~ rnorm(n, 3500, 1000),
      education == "Gymnasium" ~ rnorm(n, 4200, 1200),
      education == "University" ~ rnorm(n, 5500, 1800),
      TRUE ~ rnorm(n, 3500, 1500)
    ), -2))),
  
  # Employment status
  employment = case_when(
    age >= 65 ~ "Retired",
    age <= 25 & education %in% c("Gymnasium", "University") ~ "Student",
    runif(n) < 0.85 ~ "Employed",
    runif(n) < 0.60 ~ "Unemployed",
    TRUE ~ "Other"
  ),
  
  # Attitudes and opinions (5-point Likert scales)
  
  # Political orientation (1 = very left, 5 = very right)
  political_orientation = sample(1:5, n, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.20, 0.05)),
  
  # Environmental concern (1 = not concerned, 5 = very concerned)
  environmental_concern = case_when(
    political_orientation <= 2 ~ sample(3:5, n, replace = TRUE, prob = c(0.2, 0.4, 0.4)),
    political_orientation == 3 ~ sample(2:5, n, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25)),
    political_orientation >= 4 ~ sample(1:4, n, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)),
    TRUE ~ sample(1:5, n, replace = TRUE)
  ),
  
  # Life satisfaction (1 = very dissatisfied, 5 = very satisfied)
  life_satisfaction = case_when(
    income >= 4000 ~ sample(3:5, n, replace = TRUE, prob = c(0.2, 0.4, 0.4)),
    income >= 2500 ~ sample(2:5, n, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.25)),
    income < 2500 ~ sample(1:4, n, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1)),
    TRUE ~ sample(1:5, n, replace = TRUE)
  ),
  
  # Trust in institutions (1 = no trust, 5 = complete trust)
  trust_government = sample(1:5, n, replace = TRUE, prob = c(0.20, 0.25, 0.30, 0.20, 0.05)),
  trust_media = sample(1:5, n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.15, 0.05)),
  trust_science = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.10, 0.25, 0.40, 0.20)),
  
  # Survey design variables
  
  # Sampling weights (realistic post-stratification weights)
  sampling_weight = case_when(
    region == "East" & age >= 60 ~ runif(n, 0.8, 1.4),  # Oversample older East Germans
    region == "West" & education == "University" ~ runif(n, 0.7, 1.2),  # Undersample West academics
    gender == "Female" & age <= 30 ~ runif(n, 0.9, 1.3),  # Slight adjustment for young women
    TRUE ~ runif(n, 0.85, 1.15)  # Base weight around 1.0
  ),
  
  # Stratification variables
  stratum = paste0(region, "_", 
                   case_when(age < 35 ~ "Young",
                            age < 55 ~ "Middle", 
                            TRUE ~ "Old")),
  
  # Interview mode
  interview_mode = sample(c("Face-to-face", "Telephone", "Online"), 
                         n, replace = TRUE, prob = c(0.60, 0.25, 0.15))
) %>%
  
  # Add realistic missing data patterns
  mutate(
    # Income has higher missingness
    income = ifelse(runif(n) < 0.12, NA, income),
    
    # Political orientation sensitive topic
    political_orientation = ifelse(runif(n) < 0.08, NA, political_orientation),
    
    # Some attitudinal items have missing data
    environmental_concern = ifelse(runif(n) < 0.04, NA, environmental_concern),
    life_satisfaction = ifelse(runif(n) < 0.03, NA, life_satisfaction),
    trust_government = ifelse(runif(n) < 0.06, NA, trust_government),
    trust_media = ifelse(runif(n) < 0.05, NA, trust_media),
    trust_science = ifelse(runif(n) < 0.04, NA, trust_science)
  ) %>%
  
  # Convert categorical variables to factors with proper labels
  mutate(
    gender = factor(gender, levels = c("Male", "Female")),
    region = factor(region, levels = c("East", "West")),
    education = factor(education, 
                      levels = c("Hauptschule", "Realschule", "Gymnasium", "University"),
                      ordered = TRUE),
    employment = factor(employment,
                       levels = c("Student", "Employed", "Unemployed", "Retired", "Other")),
    stratum = factor(stratum),
    interview_mode = factor(interview_mode,
                           levels = c("Face-to-face", "Telephone", "Online"))
  )

# Add variable labels (following German survey tradition)
attr(survey_data$age, "label") <- "Alter in Jahren"
attr(survey_data$gender, "label") <- "Geschlecht"
attr(survey_data$region, "label") <- "Region (Ost/West)"
attr(survey_data$education, "label") <- "Höchster Bildungsabschluss"
attr(survey_data$income, "label") <- "Monatliches Haushaltseinkommen (EUR)"
attr(survey_data$employment, "label") <- "Erwerbsstatus"
attr(survey_data$political_orientation, "label") <- "Politische Orientierung (1=links, 5=rechts)"
attr(survey_data$environmental_concern, "label") <- "Umweltsorge (1=gering, 5=hoch)"
attr(survey_data$life_satisfaction, "label") <- "Lebenszufriedenheit (1=unzufrieden, 5=zufrieden)"
attr(survey_data$trust_government, "label") <- "Vertrauen in Regierung (1=kein, 5=vollständig)"
attr(survey_data$trust_media, "label") <- "Vertrauen in Medien (1=kein, 5=vollständig)"
attr(survey_data$trust_science, "label") <- "Vertrauen in Wissenschaft (1=kein, 5=vollständig)"
attr(survey_data$sampling_weight, "label") <- "Gewichtungsfaktor"
attr(survey_data$stratum, "label") <- "Schichtungsvariable"
attr(survey_data$interview_mode, "label") <- "Befragungsmodus"

# Save dataset
usethis::use_data(survey_data, overwrite = TRUE)

# Print summary
cat("Generated survey_data with", nrow(survey_data), "observations and", ncol(survey_data), "variables\n")
cat("Missing data patterns:\n")
survey_data %>%
  summarise_all(~sum(is.na(.))) %>%
  select_if(~. > 0) %>%
  print()