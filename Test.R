survey_data |> 
  describe(political_orientation)

survey_data |> 
  t_test(life_satisfaction, group = gender, weights = sampling_weight) |> 
  levene_test()

survey_data |> 
  frequency(life_satisfaction)

 survey_data |> 
  pearson_cor(age, life_satisfaction)

survey_data |> 
  pearson_cor(life_satisfaction, trust_government, trust_media)

survey_data |> 
  oneway_anova_test(life_satisfaction, group = education, weights = sampling_weight) |> 
  scheffe_test()

