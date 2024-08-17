#########################################
# CRADLE depression and flooding analysis

# regression analysis of flooding and depression
#########################################
rm(list=ls())
library(lubridate)
library(readstata13)
source(paste0(here::here(), '/0-config.R'))

#d = readRDS(paste0(data_dir, "/baseline_clean.RDS"))

d <- readRDS("/Users/suhi/Downloads/baseline_clean.RDS")


# covariates for regression
covariates <- c("month", "wealth_index", "mother_edu", "mother_age", "gestational_age", "dist_to_any_water")

# EPDS individual components and flooding in the compound association -----------------------------------

# create lists for storing results
mean_epds_component_floodc_exposed <- list()
mean_epds_component_floodc_unexposed <- list()
regression_results <- list()

# create binary variables for each question
process_variable <- function(variable) {
  binary_col <- paste0(variable, "_binary")
  
  # create binary column for each question
  # 1 if value is 1,2 or 3, 0 otherwise, NA if NA
  d <<- d %>%
    mutate(!!binary_col := case_when(
      get(variable) %in% c(2, 3) ~ 1,
      is.na(get(variable)) ~ NA_real_,
      TRUE ~ 0
    ))
  
  # calculate means
  mean_exposed <- mean(d[[binary_col]][d$flood_compound == 1], na.rm = TRUE)
  mean_unexposed <- mean(d[[binary_col]][d$flood_compound == 0], na.rm = TRUE)
  
  # store means 
  mean_epds_component_floodc_exposed[[binary_col]] <<- mean_exposed
  mean_epds_component_floodc_unexposed[[binary_col]] <<- mean_unexposed
  

  res <- fit_glm(data = d, Y_name = binary_col, A_name = "flood_compound",
                 covariates = c("mother_age", "month"),
                    family = "binomial")
  
  # store regression results 
  regression_results[[binary_col]] <<- res
  
}

variables <- paste0("q10_", 20:29)
lapply(variables, process_variable)


means_exposed_epds_comp <- data.frame(
  outcome = names(mean_epds_component_floodc_exposed),
  mean_exposed = unlist(mean_epds_component_floodc_exposed),
  row.names = NULL
)

means_unexposed_epds_comp <- data.frame(
  outcome = names(mean_epds_component_floodc_unexposed),
  mean_unexposed = unlist(mean_epds_component_floodc_unexposed),
  row.names = NULL
)

epds_components_reg <- bind_rows(regression_results) %>% 
  left_join(means_exposed_epds_comp, by = "outcome") %>% 
  left_join(means_unexposed_epds_comp, by = "outcome")

saveRDS(epds_components_reg, "/Users/suhi/Downloads/epds_individual_regression_results.RDS")
#saveRDS(epds_components_reg, paste0(data_dir, "epds_individual_regression_results.RDS"))

# flooding in the latrine -----------------------------------------------

## EPDS -------------
# mean in exposed, unexposed
mean_epds_floodl_exposed <- mean(d$EPDS[d$latrine_flooded==1])
mean_epds_floodl_unexposed <- mean(d$EPDS[d$latrine_flooded==0])


# unadjusted mean difference
res_epds_floodl_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="latrine_flooded")

# adjusted mean difference
res_epds_floodl_adj <- fit_glm(data=d, Y_name="EPDS", A_name="latrine_flooded",
                               covariates=covariates)

## depression -------------
# prevalence in exposed, unexposed
mean_dep_floodl_exposed <- mean(d$depression[d$latrine_flooded==1])
mean_dep_floodl_unexposed <- mean(d$depression[d$latrine_flooded==0])


# unadjusted PR moderate to severe depression
res_dep_floodl_unadj <- fit_glm(data=d, Y_name="depression", A_name="latrine_flooded",
                                family = "binomial")

# adjusted PR moderate to severe depression
res_dep_floodl_adj <- fit_glm(data=d, Y_name="depression", A_name="latrine_flooded",
                              covariates=covariates, family = "binomial")


## severe depression -------------
# prevalence in exposed, unexposed
mean_dep_sev_floodl_exposed <- mean(d$depression_severe[d$latrine_flooded==1])
mean_dep_sev_floodl_unexposed <- mean(d$depression_severe[d$latrine_flooded==0])


# unadjusted PR severe depression
res_dep_sev_floodl_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="latrine_flooded",
                                family = "binomial")

# adjusted PR severe depression
res_dep_sev_floodl_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="latrine_flooded",
                              covariates=covariates, family = "binomial")


# flooding in the compound -----------------------------------------------

## EPDS -------------
# mean in exposed, unexposed
mean_epds_floodc_exposed <- mean(d$EPDS[d$flood_compound==1])
mean_epds_floodc_unexposed <- mean(d$EPDS[d$flood_compound==0])

# unadjusted mean difference
res_epds_floodc_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_compound")

# adjusted mean difference
res_epds_floodc_adj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_compound",
                               covariates=covariates)

## depression -------------
# prevalence in exposed, unexposed
mean_dep_floodc_exposed <- mean(d$depression[d$flood_compound==1])
mean_dep_floodc_unexposed <- mean(d$depression[d$flood_compound==0])


# unadjusted PR moderate to severe depression
res_dep_floodc_unadj <- fit_glm(data=d, Y_name="depression", A_name="flood_compound",
                                family = "binomial")

# adjusted PR moderate to severe depression
res_dep_floodc_adj <- fit_glm(data=d, Y_name="depression", A_name="flood_compound",
                               covariates=covariates, family = "binomial")

## severe depression -------------
# prevalence in exposed, unexposed
mean(d$depression_severe[d$flood_compound==1])
mean(d$depression_severe[d$flood_compound==0])

# unadjusted PR
res_sdep_floodc_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_compound",
                                family = "binomial")

# adjusted PR
res_sdep_floodc_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_compound",
                              covariates=covariates, family = "binomial")



## severe depression -------------
# prevalence in exposed, unexposed
mean_dep_sev_floodc_exposed <- mean(d$depression_severe[d$flood_compound==1])
mean_dep_sev_floodc_unexposed <- mean(d$depression_severe[d$flood_compound==0])

# unadjusted PR severe depression
res_dep_sev_floodc_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_compound",
                                family = "binomial")

# adjusted PR severe depression
res_dep_sev_floodc_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_compound",
                              covariates=covariates, family = "binomial")


# flooding in the union -----------------------------------------------

## EPDS -------------
# mean in exposed, unexposed
mean_epds_floodu_exposed <- mean(d$EPDS[d$flood_union==1], na.rm=T)
mean_epds_floodu_unexposed <- mean(d$EPDS[d$flood_union==0], na.rm=T)

# unadjusted mean difference
res_epds_floodu_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_union")

# adjusted mean difference
res_epds_floodu_adj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_union",
                               covariates=covariates)

## depression -------------
# prevalence in exposed, unexposed
mean_dep_floodu_exposed <- mean(d$depression[d$flood_union==1], na.rm=T)
mean_dep_floodu_unexposed <- mean(d$depression[d$flood_union==0], na.rm=T)

# unadjusted PR
res_dep_floodu_unadj <- fit_glm(data=d, Y_name="depression", A_name="flood_union",
                                family = "binomial")

# adjusted PR
res_dep_floodu_adj <- fit_glm(data=d, Y_name="depression", A_name="flood_union",
                              covariates=covariates, family = "binomial")

## severe depression -------------
# prevalence in exposed, unexposed
mean_dep_sev_floodu_exposed <- mean(d$depression_severe[d$flood_union==1], na.rm=T)
mean_dep_sev_floodu_unexposed <- mean(d$depression_severe[d$flood_union==0], na.rm=T)

# unadjusted PR
res_dep_sev_floodu_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_union",
                                    family = "binomial")

# adjusted PR
res_dep_sev_floodu_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_union",
                                  covariates=covariates, family = "binomial")

# combine and save results ------------------------------------------------
# save regression results
# unadjusted
res_unadj <- data.frame(
  bind_rows(res_dep_floodl_unadj,
            res_dep_floodc_unadj,
            res_dep_floodu_unadj,
            
            res_dep_sev_floodl_unadj,
            res_dep_sev_floodc_unadj,
            res_dep_sev_floodu_unadj,
            
            res_epds_floodl_unadj,
            res_epds_floodc_unadj,
            res_epds_floodu_unadj)
)


saveRDS(res_unadj, "/Users/suhi/Downloads/flooding_regression_results_unadj.RDS")
#saveRDS(res_unadj, paste0(data_dir, "flooding_regression_results_unadj.RDS"))

# adjusted
res_adj <- data.frame(
  bind_rows(res_dep_floodl_adj,
            res_dep_floodc_adj,
            res_dep_floodu_adj,
            
            res_dep_sev_floodl_adj,
            res_dep_sev_floodc_adj,
            res_dep_sev_floodu_adj,
            
            res_epds_floodl_adj,
            res_epds_floodc_adj,
            res_epds_floodu_adj
  )
)

saveRDS(res_adj, "/Users/suhi/Downloads/flooding_regression_results_adj.RDS")
#saveRDS(res_adj, paste0(data_dir, "flooding_regression_results_adj.RDS"))

# save means

means <- data.frame(
  outcome = c(
    "depression",
    "depression",
    "depression",
    
    "depression_severe",
    "depression_severe",
    "depression_severe",
    
    "EPDS",
    "EPDS",
    "EPDS"
  ),
  
  label = c(
    "latrine_flooded",
    "flood_compound",
    "flood_union",
    
    "latrine_flooded",
    "flood_compound",
    "flood_union",
    
    "latrine_flooded",
    "flood_compound",
    "flood_union"
  ),
  
  mean_unexposed = c(mean_dep_floodl_unexposed,
                     mean_dep_floodc_unexposed,
                     mean_dep_floodu_unexposed,
                     
                     
                     mean_dep_sev_floodl_unexposed,
                     mean_dep_sev_floodc_unexposed,
                     mean_dep_sev_floodu_unexposed,
                     
                     mean_epds_floodl_unexposed,
                     mean_epds_floodc_unexposed,
                     mean_epds_floodu_unexposed
  ),
  
  
  mean_exposed = c(mean_dep_floodl_exposed,
                   mean_dep_floodc_exposed,
                   mean_dep_floodu_exposed,
                   
                   mean_dep_sev_floodl_exposed,
                   mean_dep_sev_floodc_exposed,
                   mean_dep_sev_floodu_exposed,
                   
                   mean_epds_floodl_exposed,
                   mean_epds_floodc_exposed,
                   mean_epds_floodu_exposed))


saveRDS(means, "/Users/suhi/Downloads/flooding_mean_results.RDS")
#saveRDS(means, paste0(data_dir, "flooding_mean_results.RDS"))
