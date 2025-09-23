devtools::load_all()
library(tidyverse)

survey_data |> 
  describe(political_orientation)

survey_data |> 
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

survey_data |> 
  group_by(region) |> 
  frequency(political_orientation)


 survey_data |> 
   filter(region == "East") |> 
   select(political_orientation, trust_media, life_satisfaction)

survey_data |> 
  pearson_cor(trust_government, political_orientation, trust_science, na.rm = "pairwise", weights = sampling_weight)
