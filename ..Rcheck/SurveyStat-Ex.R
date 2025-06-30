pkgname <- "SurveyStat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SurveyStat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("chi_squared_test")
### * chi_squared_test

flush(stderr()); flush(stdout())

### Name: chi_squared_test
### Title: Chi-Squared Test for Independence
### Aliases: chi_squared_test

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic chi-squared test for independence
survey_data %>% chi_squared_test(gender, region)

# With weights
survey_data %>% chi_squared_test(gender, education, weights = sampling_weight)

# Grouped analysis
survey_data %>% 
  group_by(region) %>% 
  chi_squared_test(gender, employment)

# With continuity correction
survey_data %>% chi_squared_test(gender, region, correct = TRUE)




cleanEx()
nameEx("describe")
### * describe

flush(stderr()); flush(stdout())

### Name: describe
### Title: Comprehensive Descriptive Statistics
### Aliases: describe

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic unweighted analysis
survey_data %>% describe(age)

# Weighted analysis
survey_data %>% describe(age, weights = sampling_weight)

# Multiple variables with custom statistics
survey_data %>% describe(age, income, life_satisfaction, 
                        weights = sampling_weight, 
                        show = c("mean", "sd", "skew"))

# Grouped analysis
survey_data %>% 
  group_by(region) %>% 
  describe(age, weights = sampling_weight)




cleanEx()
nameEx("emmeans")
### * emmeans

flush(stderr()); flush(stdout())

### Name: emmeans
### Title: Estimated Marginal Means
### Aliases: emmeans emmeans.oneway_anova_test_results

### ** Examples

## Not run: 
##D # Load data and perform repeated measures ANOVA
##D result <- data %>% 
##D   oneway_anova_test(var1, var2, group = group, repeated = TRUE, subject_id = id)
##D 
##D # Calculate all estimated marginal means
##D emmeans(result)
##D 
##D # Calculate specific effects
##D emmeans(result, show = "group")        # Between-subjects means
##D emmeans(result, show = "time")         # Within-subjects means
##D emmeans(result, show = "interaction")  # Interaction means
##D emmeans(result, show = c("group", "time"))  # Multiple effects
## End(Not run)




cleanEx()
nameEx("frequency")
### * frequency

flush(stderr()); flush(stdout())

### Name: frequency
### Title: Calculate frequency distributions with support for weights and
###   grouped data
### Aliases: frequency

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic categorical analysis
survey_data %>% frequency(gender)

# Multiple variables with weights
survey_data %>% frequency(gender, region, weights = sampling_weight)

# Grouped analysis by region
survey_data %>% 
  group_by(region) %>% 
  frequency(gender, weights = sampling_weight)

# Education levels with sorting
survey_data %>% frequency(education, sort.frq = "desc")

# Employment status with custom display options
survey_data %>% frequency(employment, weights = sampling_weight, 
                         show.na = TRUE, show.sum = TRUE)




cleanEx()
nameEx("levene_test")
### * levene_test

flush(stderr()); flush(stdout())

### Name: levene_test
### Title: Levene's Test for Homogeneity of Variance
### Aliases: levene_test levene_test.data.frame
###   levene_test.oneway_anova_test_results levene_test.t_test_results
###   levene_test.mann_whitney_test_results levene_test.grouped_df

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Standalone Levene test (test homogeneity of variances)
survey_data %>% levene_test(life_satisfaction, group = region)

# Multiple variables
survey_data %>% levene_test(life_satisfaction, trust_government, group = region)

# Weighted analysis
survey_data %>% levene_test(income, group = education, weights = sampling_weight)

# Piped after ANOVA (common workflow)
result <- survey_data %>%
  oneway_anova_test(life_satisfaction, group = education)
result %>% levene_test()

# Piped after t-test
survey_data %>%
  t_test(age, group = gender) %>%
  levene_test()

# Using mean instead of median as center
survey_data %>% levene_test(income, group = region, center = "mean")




cleanEx()
nameEx("longitudinal_data")
### * longitudinal_data

flush(stderr()); flush(stdout())

### Name: longitudinal_data
### Title: Longitudinal Study Data (Synthetic)
### Aliases: longitudinal_data
### Keywords: datasets

### ** Examples

# Load required packages and data
library(dplyr)
data(longitudinal_data)

# Simple repeated measures ANOVA (within-subjects only)
data(longitudinal_data_wide)
longitudinal_data_wide %>%
  rm_anova_test(score_T1, score_T2, score_T3, subject_id = subject_id)

# Repeated measures t-test
longitudinal_data_wide %>%
  rm_t_test(score_T1, score_T4)

# Descriptive statistics by group and time
longitudinal_data %>%
  group_by(group, time) %>%
  describe(outcome_score)




cleanEx()
nameEx("longitudinal_data_wide")
### * longitudinal_data_wide

flush(stderr()); flush(stdout())

### Name: longitudinal_data_wide
### Title: Longitudinal Study Data - Wide Format (Synthetic)
### Aliases: longitudinal_data_wide
### Keywords: datasets

### ** Examples

# Load required packages and data
library(dplyr)
data(longitudinal_data_wide)

# Paired t-test between time points
longitudinal_data_wide %>%
  rm_t_test(score_T1, score_T4)




cleanEx()
nameEx("mann_whitney_test")
### * mann_whitney_test

flush(stderr()); flush(stdout())

### Name: mann_whitney_test
### Title: Perform Mann-Whitney U tests with support for weights and
###   grouped data
### Aliases: mann_whitney_test

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic Mann-Whitney test (non-parametric comparison)
survey_data %>%
  mann_whitney_test(age, group = gender)

# Multiple variables
survey_data %>%
  mann_whitney_test(age, income, life_satisfaction, group = region)

# Using tidyselect helpers
survey_data %>%
  mann_whitney_test(starts_with("trust_"), group = gender)

# Weighted analysis
survey_data %>%
  mann_whitney_test(income, group = region, weights = sampling_weight)

# Grouped analysis (separate tests for each education level)
survey_data %>%
  group_by(education) %>%
  mann_whitney_test(life_satisfaction, group = gender)

# One-sided test
survey_data %>%
  mann_whitney_test(life_satisfaction, group = region, alternative = "greater")

# Store results for further analysis
result <- survey_data %>%
  mann_whitney_test(income, group = gender, weights = sampling_weight)
print(result)




cleanEx()
nameEx("mauchly_test")
### * mauchly_test

flush(stderr()); flush(stdout())

### Name: mauchly_test
### Title: Mauchly's Test of Sphericity
### Aliases: mauchly_test mauchly_test.oneway_anova_test_results

### ** Examples

## Not run: 
##D # Load data and perform repeated measures ANOVA
##D result <- data %>% 
##D   oneway_anova_test(var1, var2, group = group, repeated = TRUE, subject_id = id)
##D 
##D # Test sphericity assumption
##D mauchly_test(result)
## End(Not run)




cleanEx()
nameEx("oneway_anova_test")
### * oneway_anova_test

flush(stderr()); flush(stdout())

### Name: oneway_anova_test
### Title: Perform one-way ANOVA tests with support for weights and grouped
###   data
### Aliases: oneway_anova_test

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic one-way ANOVA (comparing across education levels)
survey_data %>%
  oneway_anova_test(life_satisfaction, group = education)

# Multiple dependent variables
survey_data %>%
  oneway_anova_test(life_satisfaction, trust_government, group = education)

# Using tidyselect helpers
survey_data %>%
  oneway_anova_test(starts_with("trust_"), group = education)

# Weighted analysis
survey_data %>%
  oneway_anova_test(income, group = education, weights = sampling_weight)

# Grouped analysis (separate ANOVA for each region)
survey_data %>%
  group_by(region) %>%
  oneway_anova_test(life_satisfaction, group = education)

# Unequal variances (Welch's ANOVA)
survey_data %>%
  oneway_anova_test(income, group = education, var.equal = FALSE)

# Store results for post-hoc analysis
result <- survey_data %>%
  oneway_anova_test(life_satisfaction, group = education)

# Follow up with post-hoc tests
result %>% tukey_test()
result %>% levene_test()  # Check homogeneity of variances

# Note: For repeated measures ANOVA, use rm_anova_test() function instead




cleanEx()
nameEx("parameter_estimates")
### * parameter_estimates

flush(stderr()); flush(stdout())

### Name: parameter_estimates
### Title: Parameter Estimates for Repeated Measures ANOVA
### Aliases: parameter_estimates
###   parameter_estimates.oneway_anova_test_results

### ** Examples

## Not run: 
##D # Load data and perform repeated measures ANOVA
##D result <- data %>% 
##D   oneway_anova_test(var1, var2, group = group, repeated = TRUE, subject_id = id)
##D 
##D # Calculate parameter estimates
##D parameter_estimates(result)
##D 
##D # Use different reference group
##D parameter_estimates(result, reference_group = "last")
##D parameter_estimates(result, reference_group = "Treatment")
## End(Not run)




cleanEx()
nameEx("rm_t_test")
### * rm_t_test

flush(stderr()); flush(stdout())

### Name: rm_t_test
### Title: Perform paired t-tests for repeated measures data
### Aliases: rm_t_test

### ** Examples

# Load required packages
library(dplyr)

# Create sample data
set.seed(123)
survey_data <- data.frame(
  id = 1:200,
  pre_test = rnorm(200, 50, 10),
  post_test = rnorm(200, 55, 12),
  condition = factor(rep(c("A", "B"), each = 100))
)

# Basic paired t-test
survey_data %>%
  rm_t_test(pre_test, post_test)

# Grouped paired t-test (paired comparison within each condition)
survey_data %>%
  group_by(condition) %>%
  rm_t_test(pre_test, post_test)

# One-sided test
survey_data %>%
  rm_t_test(pre_test, post_test, alternative = "greater")

# Test against different null hypothesis
survey_data %>%
  rm_t_test(pre_test, post_test, mu = 5)

# Weighted paired t-test
survey_data$weight <- runif(200, 0.5, 2.0)
survey_data %>%
  rm_t_test(pre_test, post_test, weights = weight)




cleanEx()
nameEx("survey_data")
### * survey_data

flush(stderr()); flush(stdout())

### Name: survey_data
### Title: German Social Survey Data (Synthetic)
### Aliases: survey_data
### Keywords: datasets

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic descriptive statistics
survey_data %>% describe(age, income, weights = sampling_weight)

# Frequency analysis
survey_data %>% frequency(education, region, weights = sampling_weight)

# Group comparisons
survey_data %>% 
  group_by(region) %>% 
  t_test(life_satisfaction, weights = sampling_weight)




cleanEx()
nameEx("t_test")
### * t_test

flush(stderr()); flush(stdout())

### Name: t_test
### Title: Perform independent sample t-tests with support for weights and
###   grouped data
### Aliases: t_test

### ** Examples

# Load required packages
library(dplyr)

# Create sample data
set.seed(123)
survey_data <- data.frame(
  id = 1:200,
  group = factor(rep(c("Treatment", "Control"), each = 100)),
  pre_test = rnorm(200, 50, 10),
  post_test = rnorm(200, 55, 12),
  outcome1 = c(rnorm(100, 5.2, 1.5), rnorm(100, 4.8, 1.3)),
  outcome2 = c(rnorm(100, 3.1, 1.2), rnorm(100, 3.5, 1.4)),
  weight = runif(200, 0.5, 2.0),
  gender = factor(sample(c("Male", "Female"), 200, replace = TRUE))
)

# Basic two-sample t-test
survey_data %>%
  t_test(outcome1, group = group)

# Multiple variables
survey_data %>%
  t_test(outcome1, outcome2, group = group)

# Using tidyselect helpers
survey_data %>%
  t_test(starts_with("outcome"), group = group)

# Weighted analysis
survey_data %>%
  t_test(outcome1, group = group, weights = weight)

# Grouped analysis (separate tests for each gender)
survey_data %>%
  group_by(gender) %>%
  t_test(outcome1, group = group)

# One-sample t-test (test against mu = 5)
survey_data %>%
  t_test(outcome1, mu = 5)

# One-sided test
survey_data %>%
  t_test(outcome1, group = group, alternative = "greater")

# Equal variance assumption
survey_data %>%
  t_test(outcome1, group = group, var.equal = TRUE)

# Store results for further analysis
result <- survey_data %>%
  t_test(outcome1, group = group, weights = weight)
print(result)




cleanEx()
nameEx("tukey_test")
### * tukey_test

flush(stderr()); flush(stdout())

### Name: tukey_test
### Title: Perform Tukey HSD post-hoc tests for ANOVA results
### Aliases: tukey_test

### ** Examples

# Load required packages
library(dplyr)

# Create sample data
set.seed(123)
survey_data <- data.frame(
  id = 1:300,
  treatment = factor(rep(c("Control", "Treatment1", "Treatment2"), each = 100)),
  outcome1 = c(rnorm(100, 5.0, 1.2), rnorm(100, 5.8, 1.4), rnorm(100, 6.2, 1.1)),
  outcome2 = c(rnorm(100, 3.2, 1.0), rnorm(100, 3.9, 1.3), rnorm(100, 4.1, 0.9)),
  weight = runif(300, 0.5, 2.0),
  gender = factor(sample(c("Male", "Female"), 300, replace = TRUE))
)

# Perform ANOVA followed by Tukey post-hoc test
anova_result <- survey_data %>%
  oneway_anova_test(outcome1, group = treatment)

# Tukey post-hoc comparisons
anova_result %>% tukey_test()

# Multiple variables
anova_result_multi <- survey_data %>%
  oneway_anova_test(outcome1, outcome2, group = treatment)

anova_result_multi %>% tukey_test()

# Weighted analysis
anova_weighted <- survey_data %>%
  oneway_anova_test(outcome1, group = treatment, weights = weight)

anova_weighted %>% tukey_test()

# Grouped analysis
anova_grouped <- survey_data %>%
  group_by(gender) %>%
  oneway_anova_test(outcome1, group = treatment)

anova_grouped %>% tukey_test()




cleanEx()
nameEx("w_iqr")
### * w_iqr

flush(stderr()); flush(stdout())

### Name: w_iqr
### Title: Weighted Interquartile Range (IQR)
### Aliases: w_iqr

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted interquartile range
survey_data %>% w_iqr(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_iqr(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_iqr(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(iqr_age = w_iqr(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_iqr(age)



cleanEx()
nameEx("w_kurtosis")
### * w_kurtosis

flush(stderr()); flush(stdout())

### Name: w_kurtosis
### Title: Weighted Kurtosis
### Aliases: w_kurtosis

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted kurtosis (excess kurtosis, default)
survey_data %>% w_kurtosis(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_kurtosis(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_kurtosis(age, weights = sampling_weight)

# Raw kurtosis (not excess)
survey_data %>% w_kurtosis(age, weights = sampling_weight, excess = FALSE)

# In summarise context
survey_data %>% summarise(kurt_age = w_kurtosis(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_kurtosis(age)



cleanEx()
nameEx("w_mean")
### * w_mean

flush(stderr()); flush(stdout())

### Name: w_mean
### Title: Weighted Mean
### Aliases: w_mean

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted usage
survey_data %>% w_mean(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_mean(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_mean(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(mean_age = w_mean(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_mean(age)




cleanEx()
nameEx("w_median")
### * w_median

flush(stderr()); flush(stdout())

### Name: w_median
### Title: Weighted Median
### Aliases: w_median

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted median
survey_data %>% w_median(age, weights = sampling_weight)

# Multiple variables  
survey_data %>% w_median(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_median(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(median_age = w_median(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_median(age)




cleanEx()
nameEx("w_modus")
### * w_modus

flush(stderr()); flush(stdout())

### Name: w_modus
### Title: Weighted Mode (Modus)
### Aliases: w_modus

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted mode (most frequent value)
survey_data %>% w_modus(gender, weights = sampling_weight)

# Multiple variables (works best with categorical/discrete data)
survey_data %>% w_modus(gender, region, education, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_modus(gender, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(mode_gender = w_modus(gender, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_modus(gender)



cleanEx()
nameEx("w_quantile")
### * w_quantile

flush(stderr()); flush(stdout())

### Name: w_quantile
### Title: Weighted Quantiles
### Aliases: w_quantile

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted quantiles (0%, 25%, 50%, 75%, 100%)
survey_data %>% w_quantile(age, weights = sampling_weight)

# Custom quantiles
survey_data %>% w_quantile(income, weights = sampling_weight, probs = c(0.1, 0.5, 0.9))

# Multiple variables
survey_data %>% w_quantile(age, income, weights = sampling_weight)

# Grouped data  
survey_data %>% group_by(region) %>% w_quantile(age, weights = sampling_weight)

# Unweighted (for comparison)
survey_data %>% w_quantile(age)




cleanEx()
nameEx("w_range")
### * w_range

flush(stderr()); flush(stdout())

### Name: w_range
### Title: Weighted Range
### Aliases: w_range

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted range (max - min)
survey_data %>% w_range(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_range(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_range(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(range_age = w_range(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_range(age)



cleanEx()
nameEx("w_sd")
### * w_sd

flush(stderr()); flush(stdout())

### Name: w_sd
### Title: Weighted Standard Deviation
### Aliases: w_sd

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted standard deviation
survey_data %>% w_sd(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_sd(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_sd(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(sd_age = w_sd(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_sd(age)




cleanEx()
nameEx("w_se")
### * w_se

flush(stderr()); flush(stdout())

### Name: w_se
### Title: Weighted Standard Error
### Aliases: w_se

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted standard error
survey_data %>% w_se(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_se(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_se(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(se_age = w_se(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_se(age)




cleanEx()
nameEx("w_skew")
### * w_skew

flush(stderr()); flush(stdout())

### Name: w_skew
### Title: Weighted Skewness
### Aliases: w_skew

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted skewness
survey_data %>% w_skew(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_skew(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_skew(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(skew_age = w_skew(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_skew(age)



cleanEx()
nameEx("w_var")
### * w_var

flush(stderr()); flush(stdout())

### Name: w_var
### Title: Weighted Variance
### Aliases: w_var

### ** Examples

# Load required packages and data
library(dplyr)
data(survey_data)

# Basic weighted variance
survey_data %>% w_var(age, weights = sampling_weight)

# Multiple variables
survey_data %>% w_var(age, income, life_satisfaction, weights = sampling_weight)

# Grouped data
survey_data %>% group_by(region) %>% w_var(age, weights = sampling_weight)

# In summarise context
survey_data %>% summarise(var_age = w_var(age, weights = sampling_weight))

# Unweighted (for comparison)
survey_data %>% w_var(age)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
