#' Social Survey Data (Synthetic)
#'
#' A synthetic dataset modeled after large-scale social surveys 
#' with demographics, attitudes, and survey design features. Created for 
#' testing and demonstration purposes without licensing concerns.
#'
#' @format A data frame with 2,500 rows and 16 variables:
#' \describe{
#'   \item{id}{Unique identifier (1-2500)}
#'   \item{age}{Age in years (18-95)}
#'   \item{gender}{Gender (Male, Female)}
#'   \item{region}{Region (East, West)}
#'   \item{education}{Education level (Basic Secondary, Intermediate Secondary, Academic Secondary, University)}
#'   \item{income}{Monthly household income in EUR (800-15000)}
#'   \item{employment}{Employment status (Student, Employed, Unemployed, Retired, Other)}
#'   \item{political_orientation}{Political orientation (1=very left to 5=very right)}
#'   \item{environmental_concern}{Environmental concern (1=not concerned to 5=very concerned)}
#'   \item{life_satisfaction}{Life satisfaction (1=very dissatisfied to 5=very satisfied)}
#'   \item{trust_government}{Trust in government (1=no trust to 5=complete trust)}
#'   \item{trust_media}{Trust in media (1=no trust to 5=complete trust)}
#'   \item{trust_science}{Trust in science (1=no trust to 5=complete trust)}
#'   \item{sampling_weight}{Post-stratification sampling weight (0.7-1.4)}
#'   \item{stratum}{Stratification variable combining region and age group}
#'   \item{interview_mode}{Interview mode (Face-to-face, Telephone, Online)}
#' }
#'
#' @details
#' This dataset contains realistic patterns of correlation between variables:
#' \itemize{
#'   \item Education correlates with income and political attitudes
#'   \item Regional differences reflect East/West patterns
#'   \item Age effects on employment and attitudes
#'   \item Realistic missing data patterns (3-12% depending on sensitivity)
#'   \item Survey weights for post-stratification adjustment
#' }
#'
#' The data includes proper variable labels and follows social survey conventions
#' for coding and structure. Generated with set.seed(2024) for reproducibility.
#'
#' @source Generated synthetically using realistic demographic and 
#'   attitudinal patterns. No real survey data was used.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(survey_data)
#' 
#' # Basic descriptive statistics
#' survey_data %>% describe(age, income, weights = sampling_weight)
#' 
#' # Frequency analysis
#' survey_data %>% frequency(education, region, weights = sampling_weight)
#' 
#' # Group comparisons
#' survey_data %>% 
#'   group_by(region) %>% 
#'   t_test(life_satisfaction, group = gender, weights = sampling_weight)
#'
"survey_data"

#' Longitudinal Study Data (Synthetic)
#'
#' A synthetic repeated measures dataset suitable for testing longitudinal
#' analysis functions. Contains realistic treatment effects, missing data
#' patterns, and within-subject correlations.
#'
#' @format A data frame with 480 rows and 10 variables:
#' \describe{
#'   \item{subject_id}{Subject identifier (factor, 1-120)}
#'   \item{group}{Treatment group (Control, Treatment)}
#'   \item{age}{Age in years (numeric)}
#'   \item{gender}{Gender (Male, Female)}
#'   \item{time}{Time point (T1, T2, T3, T4)}
#'   \item{time_numeric}{Time point as numeric (1-4)}
#'   \item{outcome_score}{Primary outcome measure (continuous)}
#'   \item{secondary_outcome}{Secondary outcome measure (continuous)}
#'   \item{physio_measure}{Physiological measure (continuous)}
#'   \item{questionnaire_score}{Questionnaire rating (ordered factor, 1-7)}
#' }
#'
#' @details
#' Study design features:
#' \itemize{
#'   \item 120 subjects (60 per group)
#'   \item 4 measurement occasions
#'   \item Balanced treatment assignment
#'   \item Realistic dropout patterns (0% at T1, 35% by T4)
#'   \item Treatment effect grows over time (interaction effect)
#'   \item Autoregressive error structure
#'   \item Effect size approximately Cohen's d = 0.6
#' }
#'
#' Missing data patterns reflect realistic longitudinal study attrition with
#' higher dropout in later time points. The treatment effect demonstrates
#' a clear Group x Time interaction suitable for repeated measures testing.
#'
#' @source Generated synthetically with realistic longitudinal correlation 
#'   patterns and treatment effects. No real study data was used.
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(longitudinal_data)
#' 
#' # Simple example analysis with wide format data
#' # data(longitudinal_data_wide)
#' # (Repeated measures functions coming in future version)
#'
#' # Descriptive statistics by group and time
#' longitudinal_data %>%
#'   group_by(group, time) %>%
#'   describe(outcome_score)
#'
"longitudinal_data"

#' Longitudinal Study Data - Wide Format (Synthetic)
#'
#' Wide format version of the longitudinal_data dataset with one row per subject
#' and separate columns for each time point. Useful for certain repeated measures
#' analyses and data visualization.
#'
#' @format A data frame with 120 rows and 8 variables:
#' \describe{
#'   \item{subject_id}{Subject identifier (factor, 1-120)}
#'   \item{group}{Treatment group (Control, Treatment)}
#'   \item{age}{Age in years (numeric)}
#'   \item{gender}{Gender (Male, Female)}
#'   \item{score_T1}{Outcome score at Time 1}
#'   \item{score_T2}{Outcome score at Time 2}
#'   \item{score_T3}{Outcome score at Time 3}
#'   \item{score_T4}{Outcome score at Time 4}
#' }
#'
#' @details
#' This is the wide format version of \code{\link{longitudinal_data}} with
#' outcome scores spread across columns by time point. Missing values 
#' represent subject dropout or missed assessments.
#'
#' @source Generated synthetically from the long format longitudinal_data.
#'
#' @seealso \code{\link{longitudinal_data}} for the long format version
#'
#' @examples
#' # Load required packages and data
#' library(dplyr)
#' data(longitudinal_data_wide)
#' 
#' # Analysis of change scores
#' # (Repeated measures functions coming in future version)
#'
"longitudinal_data_wide"