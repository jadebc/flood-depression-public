#################################################################
# CRADLE depression and flooding analysis

# regression analysis of proportion of surface water and depression
#################################################################
rm(list=ls())
source(paste0(here::here(), '/0-config.R'))


d = readRDS(paste0(data_dir, "/baseline_clean.RDS"))

# for all the columns that start with prop_ multiply times 100
# to get the percent of the area that is water
d <- d %>% 
  mutate_at(vars(starts_with("prop_")), funs(. * 100))

# covariates for regression
covariates <- c("month_b", "wealth_index", "mother_edu",
                "father_edu", "mother_age", "gestational_age")

# assess potential non-linear relationship ----------------------
ggplot(d, aes(x = prop_perm_water_100, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs proportion of permanent water",
       x = "Proportion of permanent water (m)",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = prop_seasonal_water_100, y = depression)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs proportion of seasonal water",
       x = "Proportion of seasonal water (m)",
       y = "Depression") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggplot(d, aes(x = prop_perm_water_100, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs proportion of permanent water",
       x = "Proportion of permanent water (m)",
       y = "EPDS score") +
  theme_minimal()

ggplot(d, aes(x = prop_seasonal_water_100, y = EPDS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs proportion of seasonal water",
       x = "Proportion of seasonal water (m)",
       y = "EPDS score") +
  theme_minimal()


# fit models for seasonal water ----------------------
seasonal_water_cols <- grep("prop_seasonal_water", colnames(d), value = TRUE) %>% as.list()

## EPDS -------------
# unadjusted mean difference
res_epds_seas_unadj <- lapply(seasonal_water_cols, function(x) 
  fit_glm(data=d, Y_name="EPDS", A_name=x)) %>% bind_rows()

# adjusted mean difference
res_epds_seas_adj <- lapply(seasonal_water_cols, function(x) 
  fit_glm(data=d, Y_name="EPDS", A_name=x,
                               covariates=covariates) %>% slice(1) ) %>% bind_rows()


## depression -------------
# unadjusted PR
res_dep_seas_unadj <- lapply(seasonal_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression", A_name=x, family = "binomial")) %>% bind_rows()

# adjusted PR
res_dep_seas_adj <-  lapply(seasonal_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression", A_name=x,
          covariates=covariates, family = "binomial") %>% slice(1) ) %>% bind_rows()


## severe depression -------------
# unadjusted PR
res_sdep_seas_unadj <- lapply(seasonal_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression_severe", A_name=x, family = "binomial")) %>% bind_rows()


# adjusted PR
res_sdep_seas_adj <-  lapply(seasonal_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression_severe", A_name=x,
          covariates=covariates, family = "binomial") %>% slice(1) ) %>% bind_rows()



# fit models for permanent water ----------------------
perm_water_cols <- grep("prop_perm_water", colnames(d), value = TRUE) %>% as.list()

## EPDS -------------
# unadjusted mean difference
res_epds_perm_unadj <- lapply(perm_water_cols, function(x) 
  fit_glm(data=d, Y_name="EPDS", A_name=x)) %>% bind_rows()

# adjusted mean difference
res_epds_perm_adj <- lapply(perm_water_cols, function(x) 
  fit_glm(data=d, Y_name="EPDS", A_name=x,
          covariates=covariates) %>% slice(1) ) %>% bind_rows()


## depression -------------
# unadjusted PR
res_dep_perm_unadj <- lapply(perm_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression", A_name=x, family = "binomial")) %>% bind_rows()

# adjusted PR
res_dep_perm_adj <-  lapply(perm_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression", A_name=x,
          covariates=covariates, family = "binomial") %>% slice(1) ) %>% bind_rows()


## severe depression -------------
# unadjusted PR
res_sdep_perm_unadj <- lapply(perm_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression_severe", A_name=x, family = "binomial")) %>% bind_rows()


# adjusted PR
res_sdep_perm_adj <-  lapply(perm_water_cols, function(x) 
  fit_glm(data=d, Y_name="depression_severe", A_name=x,
          covariates=covariates, family = "binomial") %>% slice(1) ) %>% bind_rows()



# combine and save results ------------------------------------------------
res <- data.frame(
  bind_rows(res_epds_seas_unadj, res_epds_perm_unadj, 
            res_epds_seas_adj, res_epds_perm_adj, 
            
            res_dep_seas_unadj, res_dep_perm_unadj,
            res_dep_seas_adj, res_dep_perm_adj,
            
            res_sdep_seas_unadj, res_sdep_perm_unadj, 
            res_sdep_seas_adj, res_sdep_perm_adj)
)

saveRDS(res, paste0(data_dir, "/depression_surface_water_prop_regression_results.RDS"))
write.csv(res, paste0(data_dir, "/depression_surface_water_prop_regression_results.csv"),
          row.names=F)



