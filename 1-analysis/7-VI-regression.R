#########################################
# CRADLE depression and flooding analysis

# fit logistic regression model for top variables
# in variable importance analysis
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))

baseline$father_work_agr = as.factor(ifelse(baseline$father_work=="agriculture", 1, 0))

# indicators for above median
baseline$age_over_23 = as.factor(ifelse(baseline$mother_age>23, 1, 0))
baseline$hhsize_over_4 = as.factor(ifelse(baseline$hhsize>4, 1, 0))
baseline$edu_over_6 = as.factor(ifelse(baseline$mother_edu>6, 1, 0))
baseline$income_over_5 = as.factor(ifelse(baseline$income>5, 1, 0))
baseline$wealth_above_med = as.factor(ifelse(baseline$wealth_index>median(baseline$wealth_index), 1, 0))

# depression models ------------------------------------------------
dep_age_model <- fit_glm(data=baseline, Y_name="depression", A_name="mother_age",
                         family = "poisson")[1,]

dep_age_model <- fit_glm(data=baseline, Y_name="depression", A_name="age_over_23",
                         family = "poisson")[1,]

dep_income_model <- fit_glm(data=baseline, Y_name="depression", A_name="income_over_5",
                            covariates = c("month_b"),
                            family = "poisson")[1,]

dep_ownhouse_model <- fit_glm(data=baseline, Y_name="depression", A_name="own_house",
                              covariates = c("wealth_index"),
                              family = "poisson")[1,]

dep_hhsize_model <- fit_glm(data=baseline, Y_name="depression", A_name="hhsize_over_4",
                            covariates = "wealth_index", family = "poisson")[1,]

dep_latrine_flooded_model <- fit_glm(data=baseline, Y_name="depression", A_name="latrine_flooded",
                                     covariates = c("wealth_index","month_b","mother_edu",
                                                    "month_b", "mother_age","gestational_age"),
                                     family = "poisson")[1,]

dep_flood_prep_model <- fit_glm(data=baseline, Y_name="depression", A_name="flood_prepared",
                                covariates = c("wealth_index","month_b","mother_edu",
                                               "month_b", "mother_age","gestational_age"),
                             family = "poisson")[1,]

dep_wealth_model <- fit_glm(data=baseline, Y_name="depression", A_name="wealth_above_med",
                              covariates = c("month_b"),
                              family = "poisson")[1,]

# had to drop union for the model to converge 
dep_edu_model <- fit_glm(data=baseline, Y_name="depression", A_name="edu_over_6",
                         covariates = c("wealth_index"), family = "poisson")[1,]


dep_floodc_model <- fit_glm(data=baseline, Y_name="depression", A_name="flood_compound",
                            covariates = c("wealth_index","month_b","mother_edu",
                                           "month_b", "mother_age","gestational_age"),
                            family = "poisson")[1,]

# had to drop union for the model to converge 
dep_father_work_model <- fit_glm(data=baseline, Y_name="depression", 
                                 A_name="father_work_agr",
                                 covariates = c("wealth_index", "month_b"),
                                 family = "poisson")[1,]


depression_PRs <- bind_rows(
  dep_age_model, dep_income_model, dep_ownhouse_model,
  dep_hhsize_model, dep_latrine_flooded_model, dep_flood_prep_model,
  dep_wealth_model, dep_edu_model, dep_floodc_model, dep_father_work_model
) 

saveRDS(depression_PRs, paste0(results_path, "depression_PRs.RDS"))
