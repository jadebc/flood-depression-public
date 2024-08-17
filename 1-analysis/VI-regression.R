#########################################
# CRADLE depression and flooding analysis

# fit logistic regression model for top variables
# in variable importance analysis
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))
rf_depression <- readRDS(paste0(data_dir, "rf_resilience.RDS"))
rf_resilience <- readRDS(paste0(data_dir, "rf_resilience.RDS"))


# depression models ------------------------------------------------
dep_hhsize_model <- fit_glm(data=baseline, Y_name="depression", A_name="hhsize",
                            covariates = "wealth_index", family = "poisson")[1,]
dep_age_model <- fit_glm(data=baseline, Y_name="depression", A_name="mother_age",
                         family = "poisson")[1,]
dep_floodc_model <- fit_glm(data=baseline, Y_name="depression", A_name="flood_compound",
                            covariates = c("wealth_index", "month", "dist_to_perm_water", "dist_to_seasonal_water"),
                            family = "poisson")[1,]
dep_income_model <- fit_glm(data=baseline, Y_name="depression", A_name="flood_compound",
                            covariates = c("month"),
                            family = "poisson")[1,]
# ADD UNION: 
dep_edu_model <- fit_glm(data=baseline, Y_name="depression", A_name="mother_edu",
                         covariates = c("wealth_index"), family = "poisson")[1,]
dep_floodlat_model <- fit_glm(data=baseline, Y_name="depression", A_name="latrine_flooded",
                              covariates = c("wealth_index", "month", "dist_to_perm_water", "dist_to_seasonal_water"),
                              family = "poisson")[1,]

depression_ORs <- bind_rows(
  dep_hhsize_model, dep_age_model, dep_floodc_model, dep_edu_model, dep_floodlat_model
) 

# sort label by OR value
depression_ORs$label <- factor(depression_ORs$label, levels = depression_ORs$label[order(depression_ORs$pt_estimate)])

ggplot(depression_ORs, aes(x = label, y = pt_estimate, ymin = CI_lower, ymax = CI_upper)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Odds Ratios for Depression",
       x = "Covariate",
       y = "Odds Ratio",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(limits = c(0,6))+
  theme_minimal()


# resilience models ------------------------------------------------
res_hhsize_model <- fit_glm(data=baseline, Y_name="resilient", A_name="hhsize",
                            covariates = "wealth_index", family = "poisson")[1,]
res_age_model <- fit_glm(data=baseline, Y_name="depression", A_name="mother_age",
                         family = "poisson")[1,]
res_floodc_model <- fit_glm(data=baseline, Y_name="resilient", A_name="flood_compound",
                            covariates = c("wealth_index", "month", "dist_to_perm_water", "dist_to_seasonal_water"),
                            family = "poisson")[1,]
# ADD UNION: 
res_edu_model <- fit_glm(data=baseline, Y_name="resilient", A_name="mother_edu",
                         covariates = c("wealth_index"), family = "poisson")[1,]
res_floodlat_model <- fit_glm(data=baseline, Y_name="resilient", A_name="latrine_flooded",
                              covariates = c("wealth_index", "month", "dist_to_perm_water", "dist_to_seasonal_water"),
                              family = "poisson")[1,]

resilience_ORs <- bind_rows(
  dep_hhsize_model, dep_age_model, dep_floodc_model, dep_edu_model, dep_floodlat_model
) 

# sort label by OR value
depression_ORs$label <- factor(depression_ORs$label, levels = depression_ORs$label[order(depression_ORs$pt_estimate)])

ggplot(depression_ORs, aes(x = label, y = pt_estimate, ymin = CI_lower, ymax = CI_upper)) +
  geom_pointrange() +
  coord_flip() +
  labs(title = "Odds Ratios for Depression",
       x = "Covariate",
       y = "Odds Ratio",
       caption = "95% Confidence Intervals") +
  scale_y_continuous(limits = c(0,6))+
  theme_minimal()
