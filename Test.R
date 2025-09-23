survey_data |> 
  describe(political_orientation)

survey_data |> 
  t_test(life_satisfaction, group = gender, weights = sampling_weight)

survey_data |> 
  frequency(life_satisfaction)

 survey_data |> 
  pearson_cor(age, life_satisfaction)

survey_data |> 
  chi_squared_test(gender, region)

