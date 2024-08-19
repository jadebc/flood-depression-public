#########################################
# CRADLE depression and flooding analysis

# random forest analysis of drivers
# of flooding resilience
#########################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))
library(randomForest)
library(caret)
library(party)
#install.packages("party")

baseline = readRDS(paste0(data_dir, "baseline_clean.RDS"))
#baseline = readRDS("/Users/suhi/Downloads/baseline_clean.RDS")

view(baseline$union)

# Prepare the data -------------------------------------------------------
predictors <- c("union", "month", "flood_compound", "flood_union", "mother_age",
                "gestational_age",
                "dist_to_perm_water", "dist_to_seasonal_water",
                "wealth_index", "mother_edu", "father_edu", "hhsize", "father_work",
                "hygienic_latrine",
                "elec", "bike","moto", "boat", 
                "flood_prepared", 
                "inside_hh_flooded", "latrine_flooded", "tubewell_flooded", 
                "own_house", "income",
                "n_cow", "n_goat", "n_chicken", 
                "fuel_wood", "fuel_grass", "fuel_dung", 
                "private_toilet", "satisfied_house")

# suhi added union, flood preparedness, hygienic latrine, father_work


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
#saveRDS(rf_depression, "/Users/suhi/Downloads/rf_depression.RDS")

##############################################################################
# models with importance scores that are non-zero and greater
# than the absolute value of the largest negative importance value

# test_dep <- readRDS(paste0(data_dir, "rf_depression.RDS"))
# imp_dep <- test_dep$var_importance_sorted
# 
# imp_dep <- imp_dep %>%
#   filter(importance > abs(min(importance)))


##############################################################################


# Print top 25% most important variables
print(rf_depression$top_vars)

# Print filtered important variables
print(rf_depression$filtered_importance)

# Plot variable importance
depression_plot <- ggplot(rf_depression$var_importance_sorted,
                          aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill=var_cat)) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Variables", y = "Importance", 
       title = "Random Forest Variable Importance") +
  theme_minimal()

ggsave(
  filename = paste0(data_dir, "var_imp_figures/rf_depression_var_importance.png"),
  plot = depression_plot,
  width = 8, height = 6, units = "in"
)


# Resilience model -------------------------------------------------------
set.seed(123) 
rf_resilience <- run_random_forest(data = baseline %>% filter(flood_union==1), 
                                   outcome = "resilient", 
                                   predictors = predictors[!predictors %in% c("flood_union")])

saveRDS(rf_resilience, paste0(data_dir, "rf_resilience.RDS"))
#saveRDS(rf_resilience, "/Users/suhi/Downloads/rf_resilience.RDS")

##############################################################################
# models with importance scores that are non-zero and greater 
# than the absolute value of the largest negative importance value

test_res <- readRDS(paste0(data_dir, "rf_resilience.RDS"))
imp_res <- test_res$var_importance_sorted

imp_res <- imp_res %>%
  filter(importance > abs(min(importance)))

##############################################################################

# Print top 25% most important variables
print(rf_resilience$top_vars)

# Print filtered important variables
print(rf_resilience$filtered_importance)

# Plot variable importance
resilience_plot <- ggplot(rf_resilience$var_importance_sorted,
                          aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill=var_cat)) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Variables", y = "Importance", 
       title = "Random Forest Variable Importance") +
  theme_minimal()

ggsave(
  filename = paste0(data_dir, "var_imp_figures/rf_resilience_var_importance.png"),
  plot = resilience_plot,
  width = 8, height = 6, units = "in"
)

