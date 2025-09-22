devtools::load_all()
library(tidyverse)

survey_data |> 
  describe(income, life_satisfaction)

survey_data |> 
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

survey_data |> 
  frequency(region)
1
 
