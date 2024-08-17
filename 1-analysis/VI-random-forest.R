#########################################
# CRADLE depression and flooding analysis

# random forest analysis of drivers
# of flooding resilience
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))
library(randomForest)
library(caret)

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))

# Prepare the data -------------------------------------------------------
predictors <- c("flood_compound", "flood_union", "mother_age",
                "gestational_age",
                "dist_to_perm_water", "dist_to_seasonal_water",
                "wealth_index", "mother_edu", "father_edu", "hhsize",
                "elec", "bike","moto", "boat", "flood_prepared", "inside_hh_flooded",
                "latrine_flooded", "tubewell_flooded", "own_house", "income",
                "n_cow", "n_goat", "n_chicken", "fuel_wood", "fuel_grass",
                "fuel_dung", "private_toilet", "satisfied_house")

# TO DO: add union, hygienic latrine and flood preparedness


palette <- c(
  "#9A6324",
  "#A9A9A9",
  "#3CB44B",
  "#9F5F9F",
  "#4363D8"
)

# Depression model -------------------------------------------------------
set.seed(123) 
rf_depression <- run_random_forest(data = baseline, 
                  outcome = "depression", 
                  predictors = predictors)
  
saveRDS(rf_depression, paste0(data_dir, "rf_depression.RDS"))

# Print top 25% most important variables
print(rf_depression$top_vars)

# Print filtered important variables
print(rf_depression$filtered_importance)

# Plot variable importance
ggplot(rf_depression$var_importance_sorted,
       aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill=var_cat)) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Variables", y = "Importance", 
       title = "Random Forest Variable Importance") +
  theme_minimal()


# Resilience model -------------------------------------------------------
set.seed(123) 
rf_resilience <- run_random_forest(data = baseline %>% filter(flood_union==1), 
                                   outcome = "resilient", 
                                   predictors = predictors[!predictors %in% c("flood_union")])

saveRDS(rf_resilience, paste0(data_dir, "rf_resilience.RDS"))

# Print top 25% most important variables
print(rf_resilience$top_vars)

# Print filtered important variables
print(rf_resilience$filtered_importance)

# Plot variable importance
ggplot(rf_resilience$var_importance_sorted,
       aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill=var_cat)) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Variables", y = "Importance", 
       title = "Random Forest Variable Importance") +
  theme_minimal()


