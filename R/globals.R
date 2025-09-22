# globals.R
# Declare global variables to avoid R CMD check NOTEs
# These variables are used in NSE (Non-Standard Evaluation) contexts within dplyr operations

# This file suppresses "no visible binding for global variable" NOTEs that occur
# when using column names in dplyr verbs and other tidyverse functions.
# These are false positives because the variables exist as column names in the data frames.

utils::globalVariables(c(
  # Column names used in frequency analysis
  "Variable",
  "value", 
  "label",
  "freq",
  "prc", 
  "valid_prc",
  "cum_freq",
  "cum_prc",
  
  # Column names that may be created/referenced in results
  "n_eff",
  "group_info",
  
  # Variables used in test result data frames
  "statistic",
  "p.value",
  "parameter",
  "estimate",
  "conf.int",
  
  # Variables potentially used in grouped operations
  "g",
  "w",
  "y",
  
  # Variables used in repeated measures and ANOVA analyses
  ".",
  ":=",
  "Response",
  "Time",
  "n",
  "subject_mean",
  "subject_weighted_mean"
))