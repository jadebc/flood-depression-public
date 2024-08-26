#########################################
# CRADLE depression and flooding analysis

# fit logistic regression model for top variables
# in variable importance analysis
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))

baseline$father_work_agr = ifelse(baseline$father_work=="agriculture", 1, 0)


# depression models ------------------------------------------------
dep_age_model <- fit_glm(data=baseline, Y_name="depression", A_name="mother_age",
                         family = "poisson")[1,]

dep_hyg_lat_model <- fit_glm(data=baseline, Y_name="depression", A_name="hygienic_latrine",
                             covariates = c("wealth_index","hhsize","mother_edu"),
                             family = "poisson")[1,]

dep_income_model <- fit_glm(data=baseline, Y_name="depression", A_name="income",
                            covariates = c("month"),
                            family = "poisson")[1,]

dep_ownhouse_model <- fit_glm(data=baseline, Y_name="depression", A_name="own_house",
                              covariates = c("wealth_index"),
                              family = "poisson")[1,]

dep_hhsize_model <- fit_glm(data=baseline, Y_name="depression", A_name="hhsize",
                            covariates = "wealth_index", family = "poisson")[1,]

dep_latrine_flooded_model <- fit_glm(data=baseline, Y_name="depression", A_name="latrine_flooded",
                                covariates = c("wealth_index","month","mother_edu"),
                                family = "poisson")[1,]

dep_flood_prep_model <- fit_glm(data=baseline, Y_name="depression", A_name="flood_prepared",
                             covariates = c("wealth_index","month","mother_edu"),
                             family = "poisson")[1,]

dep_wealth_model <- fit_glm(data=baseline, Y_name="depression", A_name="wealth_index",
                              covariates = c("month"),
                              family = "poisson")[1,]

# had to drop union for the model to converge 
dep_edu_model <- fit_glm(data=baseline, Y_name="depression", A_name="mother_edu",
                         covariates = c("wealth_index"), family = "poisson")[1,]


dep_floodc_model <- fit_glm(data=baseline, Y_name="depression", A_name="flood_compound",
                            covariates = c("wealth_index", "month", "dist_to_perm_water", "dist_to_seasonal_water"),
                            family = "poisson")[1,]

# had to drop union for the model to converge 
dep_father_work_model <- fit_glm(data=baseline, Y_name="depression", 
                                 A_name="father_work_agr",
                                 covariates = c("wealth_index", "month"),
                                 family = "poisson")[1,]


depression_ORs <- bind_rows(
  dep_age_model, dep_hyg_lat_model, dep_income_model, dep_ownhouse_model,
  dep_hhsize_model, dep_latrine_flooded_model, dep_flood_prep_model,
  dep_wealth_model, dep_edu_model, dep_floodc_model, dep_father_work_model
) 

# sort label by OR value
depression_ORs$label <- factor(depression_ORs$label, levels = depression_ORs$label[order(depression_ORs$pt_estimate)])

depression_ORs <- depression_ORs %>% mutate(var_cat = case_when(
  label == "latrine_flooded" ~ "Flooding",
  label == "flood_compound" ~ "Flooding",
  label == "hygienic_latrine" ~ "Housing",
  label == "mother_age" ~ "Demographics",
  label == "hhsize" ~ "Demographics",
  label == "mother_edu" ~ "Demographics",
  label == "income" ~ "Economics",
  label == "flood_prepared" ~ "Flooding",
  label == "father_work_agr" ~ "Demographics",
  label == "wealth_index" ~ "Economics",
  label == "own_house" ~ "Housing"
))

palette <- c(
  "#9A6324",
  "#3CB44B",
  "#4363D8",
  "#4d4c4b"

)

dep_PR_plot <- ggplot(depression_ORs, aes(x = label, y = pt_estimate, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = 1, color = "grey") +
  geom_pointrange(aes(color = var_cat), size=0.25) +
  scale_color_manual(values= palette) +
  coord_flip() +
  labs(y = "Prevalence Ratio") +
  scale_y_continuous(trans= "log", breaks=seq(1,6,1),
                     labels =seq(1,6,1)) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank())


dep_PR_plot

ggsave(
  filename = paste0(figure_path, "rf_depression_PRs.png"),
  plot = dep_PR_plot,
  width = 4, height = 3, units = "in"
)
