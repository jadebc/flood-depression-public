#############################################
# CRADLE depression and flooding analysis

# Create tables S1-S2: 
#############################################

rm(list = ls())
source(paste0(here::here(), '/0-config.R'))

table <- readRDS("/Users/suhi/Downloads/baseline_clean.RDS")

