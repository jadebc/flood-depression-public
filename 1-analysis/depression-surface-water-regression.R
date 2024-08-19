#########################################
# CRADLE depression and flooding analysis

# regression analysis of surface water and depression
#########################################
rm(list=ls())
library(lubridate)
library(readstata13)
source(paste0(here::here(), '/0-config.R'))


d = readRDS(paste0(data_dir, "/baseline_clean.RDS"))
#d <- readRDS("/Users/suhi/Downloads/baseline_clean.RDS")

# define covariates for regression models
covariates <- c("month", "wealth_index", "mother_edu", "mother_age", "gestational_age")

# assess potential non-linear relationship ----------------------
ggplot(d, aes(x = dist_to_perm_water, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs distance to permanent water",
       x = "Distance to permanent water (m)",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = dist_to_seasonal_water, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs distance to seasonal water",
       x = "Distance to seasonal water (m)",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = dist_to_perm_water, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs distance to permanent water",
       x = "Distance to permanent water (m)",
       y = "EPDS score") +
  theme_minimal()

ggplot(d, aes(x = dist_to_seasonal_water, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs distance to seasonal water",
       x = "Distance to seasonal water (m)",
       y = "EPDS score") +
  theme_minimal()


# fit models for seasonal water ----------------------

## EPDS -------------
# unadjusted mean difference
res_epds_seas_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="dist_to_seasonal_water")

# adjusted mean difference
res_epds_seas_adj <- fit_glm(data=d, Y_name="EPDS", A_name="dist_to_seasonal_water",
                               covariates=covariates)


## depression -------------
# unadjusted PR
res_dep_seas_unadj <- fit_glm(data=d, Y_name="depression", A_name="dist_to_seasonal_water",
                                family = "binomial")

# adjusted PR
res_dep_seas_adj <- fit_glm(data=d, Y_name="depression", A_name="dist_to_seasonal_water",
                              covariates=covariates, family = "binomial")

## severe depression -------------
# unadjusted PR
res_sdep_seas_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="dist_to_seasonal_water",
                                 family = "binomial")

# adjusted PR
res_sdep_seas_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="dist_to_seasonal_water",
                               covariates=covariates, family = "binomial")

# fit models for permanent water ----------------------

## EPDS -------------
# unadjusted mean difference
res_epds_perm_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="dist_to_perm_water")

# adjusted mean difference
res_epds_perm_adj <- fit_glm(data=d, Y_name="EPDS", A_name="dist_to_perm_water",
                             covariates=covariates)


## depression -------------
# unadjusted PR
res_dep_perm_unadj <- fit_glm(data=d, Y_name="depression", A_name="dist_to_perm_water",
                              family = "binomial")

# adjusted PR
res_dep_perm_adj <- fit_glm(data=d, Y_name="depression", A_name="dist_to_perm_water",
                            covariates=covariates, family = "binomial")

## severe depression -------------
# unadjusted PR
res_sdep_perm_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="dist_to_perm_water",
                               family = "binomial")

# adjusted PR
res_sdep_perm_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="dist_to_perm_water",
                             covariates=covariates, family = "binomial")


# combine and save results ------------------------------------------------
res <- data.frame(
  bind_rows(res_epds_seas_unadj, res_epds_perm_unadj, 
            res_epds_seas_adj, res_epds_perm_adj, 
            
            res_dep_seas_unadj, res_dep_perm_unadj,
            res_dep_seas_adj, res_dep_perm_adj,
            
            res_sdep_seas_unadj, res_sdep_perm_unadj, 
            res_sdep_seas_adj, res_sdep_perm_adj)
)

saveRDS(res, paste0(data_dir, "/depression_surface_water_regression_results.RDS"))
#saveRDS(res, "/Users/suhi/Downloads/depression_surface_water_regression_results.RDS")



