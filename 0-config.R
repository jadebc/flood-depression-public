#########################################
# CRADLE depression and flooding analysis

# configure data directories
# source base functions
# load libraries
#########################################

library(tidyverse)
library(here)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(assertthat)
library(boxr)

source(paste0(here::here(), "/0-base-functions.R"))


if(Sys.getenv("LOGNAME")=="jadebc"){
  box_path = "/Users/jadebc/Library/CloudStorage/Box-Box/Jade Benjamin-Chung's Externally Shareable Files/"
}

if(Sys.getenv("LOGNAME")=="gabriellabh"){
  box_path = "/Users/gabriellabh/Library/CloudStorage/Box-Box/"
}

if(Sys.getenv("LOGNAME")=="suhi"){
  box_path = "/Users/suhi/Library/CloudStorage/Box-Box/"
}

box_path_cradle_data = paste0(box_path, "CRADLE-Data/")
box_shapefile_path = paste0(box_path_cradle_data, "Shapefiles/")


# Define directories
data_dir = paste0(box_path, "CRADLE-Flooding-depression/")
figure_path = here::here("figures/")
table_path = here::here("tables/")
results_path = here::here("results/")



