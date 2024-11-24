#########################################################
# CRADLE depression and flooding analysis

# regression analysis of surface water and depression
#########################################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))


d = readRDS(paste0(data_dir, "/baseline_clean.RDS"))

# calculate the date of conception using edd and date
d = d %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         edd = as.Date(edd, format = "%Y-%m-%d"),
         conception_date = edd - 280) 

gov_data_raw = read.csv(paste0(box_path_cradle_data, "/Climate Data Sirajganj/climate_data_sirajganj.csv"))

gov_data = filter(gov_data_raw, !is.na(Sirajganj_WL)) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# merge in gov_data to d and calculate the 
# mean and max water level during the gestational period 
get_water_level <- function(date_conception, date_current){
  water_level_preg = gov_data %>% 
    filter(date >= date_conception & date <= date_current) %>% 
    summarise(mean_water_level_preg = mean(Sirajganj_WL, na.rm = TRUE),
              max_water_level_preg = max(Sirajganj_WL, na.rm = TRUE))
  
  water_level_6m = gov_data %>% 
    filter(date >= as.Date(date_current)-6*30.45 & date <= date_current) %>% 
    summarise(mean_water_level_6m = mean(Sirajganj_WL, na.rm = TRUE),
              max_water_level_6m = max(Sirajganj_WL, na.rm = TRUE))
  
  out = cbind(water_level_preg, water_level_6m)
  return(out)
}

# apply this function to d, do it so that the output is 
# bound as a column to d
water_level_summary <- apply(d, 1, function(x) get_water_level(x["conception_date"], x["date"])) %>% 
  bind_rows()

d <- bind_cols(d, water_level_summary)


# define covariates for regression models
covariates <- c("month_b", "wealth_index", "mother_edu",
                "father_edu", "mother_age", "gestational_age")

# assess potential non-linear relationship ----------------------
ggplot(d, aes(x = mean_water_level_preg, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs mean water level",
       x = "Mean water level",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = max_water_level_preg, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs max water level",
       x = "Max water level",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = mean_water_level_preg, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs mean water level",
       x = "Mean water level",
       y = "EPDS score") +
  theme_minimal()

ggplot(d, aes(x = max_water_level_preg, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs max water level",
       x = "Max water level",
       y = "Depression") +
  theme_minimal()

ggplot(d, aes(x = mean_water_level_6m, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs mean water level",
       x = "Mean water level",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = max_water_level_preg, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs max water level",
       x = "Max water level",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = mean_water_level_preg, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs mean water level",
       x = "Mean water level",
       y = "EPDS score") +
  theme_minimal()

ggplot(d, aes(x = max_water_level_preg, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs max water level",
       x = "Max water level",
       y = "Depression") +
  theme_minimal()


# fit models for mean water level (pregnancy) ----------------------

## EPDS -------------
# unadjusted mean difference
res_epds_mean_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="mean_water_level_preg")

# adjusted mean difference
res_epds_mean_adj <- fit_glm(data=d, Y_name="EPDS", A_name="mean_water_level_preg",
                             covariates=covariates)


## depression -------------
# unadjusted PR
res_dep_mean_unadj <- fit_glm(data=d, Y_name="depression", A_name="mean_water_level_preg",
                              family = "poisson")

# adjusted PR
res_dep_mean_adj <- fit_glm(data=d, Y_name="depression", A_name="mean_water_level_preg",
                            covariates=covariates, family = "poisson")

## severe depression -------------
# unadjusted PR
res_sdep_mean_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="mean_water_level_preg",
                               family = "poisson")

# adjusted PR
res_sdep_mean_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="mean_water_level_preg",
                             covariates=covariates, family = "poisson")

# fit models for max water level  (pregnancy) ----------------------

## EPDS -------------
# unadjusted mean difference
res_epds_max_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="max_water_level_preg")

# adjusted mean difference
res_epds_max_adj <- fit_glm(data=d, Y_name="EPDS", A_name="max_water_level_preg",
                             covariates=covariates)


## depression -------------
# unadjusted PR
res_dep_max_unadj <- fit_glm(data=d, Y_name="depression", A_name="max_water_level_preg",
                              family = "poisson")

# adjusted PR
res_dep_max_adj <- fit_glm(data=d, Y_name="depression", A_name="max_water_level_preg",
                            covariates=covariates, family = "poisson")

## severe depression -------------
# unadjusted PR
res_sdep_max_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="max_water_level_preg",
                               family = "poisson")

# adjusted PR
res_sdep_max_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="max_water_level_preg",
                             covariates=covariates, family = "poisson")



# fit models for mean water level (pregnancy) ----------------------

## EPDS -------------
# unadjusted mean difference
res_epds_mean_6m_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="mean_water_level_6m")

# adjusted mean difference
res_epds_mean_6m_adj <- fit_glm(data=d, Y_name="EPDS", A_name="mean_water_level_6m",
                                covariates=covariates)


## depression -------------
# unadjusted PR
res_dep_mean_6m_unadj <- fit_glm(data=d, Y_name="depression", A_name="mean_water_level_6m",
                                 family = "poisson")

# adjusted PR
res_dep_mean_6m_adj <- fit_glm(data=d, Y_name="depression", A_name="mean_water_level_6m",
                               covariates=covariates, family = "poisson")

## severe depression -------------
# unadjusted PR
res_sdep_mean_6m_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="mean_water_level_6m",
                                  family = "poisson")

# adjusted PR
res_sdep_mean_6m_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="mean_water_level_6m",
                                covariates=covariates, family = "poisson")

# fit models for max water level  (pregnancy) ----------------------

## EPDS -------------
# unadjusted mean difference
res_epds_max_6m_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="max_water_level_6m")

# adjusted mean difference
res_epds_max_6m_adj <- fit_glm(data=d, Y_name="EPDS", A_name="max_water_level_6m",
                               covariates=covariates)


## depression -------------
# unadjusted PR
res_dep_max_6m_unadj <- fit_glm(data=d, Y_name="depression", A_name="max_water_level_6m",
                                family = "poisson")

# adjusted PR
res_dep_max_6m_adj <- fit_glm(data=d, Y_name="depression", A_name="max_water_level_6m",
                              covariates=covariates, family = "poisson")

## severe depression -------------
# unadjusted PR
res_sdep_max_6m_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="max_water_level_6m",
                                 family = "poisson")

# adjusted PR
res_sdep_max_6m_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="max_water_level_6m",
                               covariates=covariates, family = "poisson")



# combine and save results ------------------------------------------------
res <- data.frame(
  bind_rows(res_epds_mean_unadj, res_epds_max_unadj, 
            res_epds_mean_adj, res_epds_max_adj, 
            
            res_dep_mean_unadj, res_dep_max_unadj,
            res_dep_mean_adj, res_dep_max_adj,
            
            res_sdep_mean_unadj, res_sdep_max_unadj, 
            res_sdep_mean_adj, res_sdep_max_adj,
            
            res_epds_mean_6m_unadj, res_epds_max_6m_unadj, 
            res_epds_mean_6m_adj, res_epds_max_6m_adj, 
            
            res_dep_mean_6m_unadj, res_dep_max_6m_unadj,
            res_dep_mean_6m_adj, res_dep_max_6m_adj,
            
            res_sdep_mean_6m_unadj, res_sdep_max_6m_unadj, 
            res_sdep_mean_6m_adj, res_sdep_max_6m_adj)
)
saveRDS(res, paste0(data_dir, "/depression_water_level_regression_results.RDS"))

