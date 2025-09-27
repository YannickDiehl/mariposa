# Reload package to get latest changes
devtools::load_all(".")
library(dplyr)

data(survey_data)

# Test describe
survey_data |>
  describe(political_orientation)

# Test t_test with levene
survey_data |>
  t_test(life_satisfaction, group = gender, weights = sampling_weight) |>
  levene_test()

# Test frequency
survey_data |>
  frequency(life_satisfaction)

# Test pearson correlation
survey_data |>
  pearson_cor(age, life_satisfaction)

survey_data |>
  pearson_cor(life_satisfaction, trust_government, trust_media)

# Test ANOVA with scheffe
survey_data |>
  oneway_anova_test(life_satisfaction, group = education, weights = sampling_weight) |>
  scheffe_test()


survey_data |>
  chi_squared_test(gender, region, weights = sampling_weight)

survey_data |>
  group_by(region) |> 
  kendall_tau(life_satisfaction, trust_government, trust_media)

