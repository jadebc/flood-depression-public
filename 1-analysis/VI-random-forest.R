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

baseline$rainy_season = ifelse(baseline$month >= 6 & baseline$month<=9, 1, 0)

# Prepare the data -------------------------------------------------------
predictors <- c("union", "rainy_season",
                "flood_compound", "flood_union",
                "mother_age",
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

imp_dep <- rf_depression$var_importance_sorted

imp_dep <- imp_dep %>%
  filter(importance > abs(min(importance)))

##############################################################################


palette <- c(
  "#9A6324",
  "#3CB44B",
  "#4d4c4b",
  "#4363D8"
)

imp_dep <- imp_dep %>% mutate(var_cat = ifelse(variable=="hygienic_latrine", "Housing", var_cat))
imp_dep <- imp_dep %>% mutate(var_cat = ifelse(variable=="father_work", "Economic", var_cat))

# Plot variable importance
depression_plot <- ggplot(imp_dep,
                          aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", aes(fill=var_cat)) +
  coord_flip() +
  scale_fill_manual(values = palette) +
  labs(x = "Variables", y = "Importance") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())

depression_plot

ggsave(
  filename = paste0(figure_path, "rf_depression_var_importance.png"),
  plot = depression_plot,
  width = 5, height = 3, units = "in"
)
