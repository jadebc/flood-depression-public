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

