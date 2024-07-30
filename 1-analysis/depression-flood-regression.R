#########################################
# CRADLE depression and flooding analysis

# regression analysis of flooding and depression
#########################################
rm(list=ls())
library(lubridate)
library(readstata13)
source(paste0(here::here(), '/0-config.R'))

d = readRDS(paste0(data_dir, "/baseline_clean.RDS"))

# define covariates for regression models
covariates <- c("month", "wealth_index", "mother_edu", "mother_age", "gestational_age")

# flooding in the compound -----------------------------------------------

## EPDS -------------
# mean in exposed, unexposed
mean(d$EPDS[d$flood_compound==1])
mean(d$EPDS[d$flood_compound==0])

# unadjusted mean difference
res_epds_floodc_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_compound")

# adjusted mean difference
res_epds_floodc_adj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_compound",
                               covariates=covariates)

## depression -------------
# prevalence in exposed, unexposed
mean(d$depression[d$flood_compound==1])
mean(d$depression[d$flood_compound==0])

# unadjusted PR
res_dep_floodc_unadj <- fit_glm(data=d, Y_name="depression", A_name="flood_compound",
                                family = "binomial")

# adjusted PR
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



# flooding in the union -----------------------------------------------

## EPDS -------------
# mean in exposed, unexposed
mean(d$EPDS[d$flood_union==1], na.rm=T)
mean(d$EPDS[d$flood_union==0], na.rm=T)

# unadjusted mean difference
res_epds_floodu_unadj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_union")

# adjusted mean difference
res_epds_floodu_adj <- fit_glm(data=d, Y_name="EPDS", A_name="flood_union",
                               covariates=covariates)

## depression -------------
# prevalence in exposed, unexposed
mean(d$depression[d$flood_union==1], na.rm=T)
mean(d$depression[d$flood_union==0], na.rm=T)

# unadjusted PR
res_dep_floodu_unadj <- fit_glm(data=d, Y_name="depression", A_name="flood_union",
                                family = "binomial")

# adjusted PR
res_dep_floodu_adj <- fit_glm(data=d, Y_name="depression", A_name="flood_union",
                              covariates=covariates, family = "binomial")

## severe depression -------------
# prevalence in exposed, unexposed
mean(d$depression_severe[d$flood_union==1], na.rm=T)
mean(d$depression_severe[d$flood_union==0], na.rm=T)

# unadjusted PR
res_sdep_floodu_unadj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_union",
                                family = "binomial")

# adjusted PR
res_sdep_floodu_adj <- fit_glm(data=d, Y_name="depression_severe", A_name="flood_union",
                              covariates=covariates, family = "binomial")


# combine and save results ------------------------------------------------
res <- data.frame(
  bind_rows(res_epds_floodc_unadj, res_epds_floodu_unadj, 
            res_epds_floodc_adj, res_epds_floodu_adj, 
            
            res_dep_floodc_unadj, res_dep_floodu_unadj,
            res_dep_floodc_adj, res_dep_floodu_adj,
            
            res_sdep_floodc_unadj, res_sdep_floodu_unadj, 
            res_sdep_floodc_adj, res_sdep_floodu_adj)
)

saveRDS(res, paste0(data_dir, "/depression_flood_regression_results.RDS"))

