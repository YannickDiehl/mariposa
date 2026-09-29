# Re-exported functions
# ---------------------
# The pipe is used throughout the documentation and vignettes
# (`survey_data %>% describe(age)`); re-exporting it makes those examples
# work after library(mariposa) alone, without attaching dplyr.

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`
