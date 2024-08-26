
rm(list=ls())
#install.packages("psych")
library(lubridate)
library(readxl)
library(readstata13)
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
library(writexl)

source(paste0(here::here(), '/0-config.R'))

#baseline_raw=read.dta13("/Users/suhi/Downloads/CRADLE_Baseline_data.dta", convert.factors=F)
baseline_raw=read.dta13(paste0(box_path_cradle_data,"Baseline/CRADLE_Baseline_data.dta"), convert.factors=F)


#----------------------------------------
# rename variables and create new variables
#----------------------------------------
baseline <- baseline_raw %>% 
  rename(flood_compound = q21_1,
         flood_union = q21_10) %>% 
  mutate(date = as.Date(q1_2, format = "%Y-%m-%d")) %>% 
  mutate(month = month(date)) %>% 
  mutate(flood_union = as.factor(ifelse(flood_union==99,NA,flood_union)),
         flood_compound = as.factor(flood_compound)) %>% 
  mutate(dataid= as.character(dataid)) %>% 
  mutate(edd = as.Date(edd, format ="%Y-%m-%d" )) %>% 
  # calculate gestational age
  mutate(gestational_age = as.numeric(280 - difftime(edd, date, units = "days"))/7)  %>% 
  mutate(trimester = case_when(
    gestational_age < 13 ~ 1,
    gestational_age < 27 ~ 2,
    TRUE ~ 3
  )) %>% 
  rename(union = q1_3,
         mother_age = q2_2,
         mother_edu = q5_1,
         father_edu = q5_2, 
         hhsize = q4_1,
         father_work = q5_3,
         income = q19_4,
         n_cow = q13_1, 
         n_goat = q13_2,
         n_chicken = q13_3,
         elec = q19_21, 
         fridge = q19_211, 
         bike = q19_212,
         moto = q19_213,
         boat = q19_214,
         flood_prepared = q21_12, 
         inside_hh_flooded = q21_3,
         latrine_flooded = q21_5,
         tubewell_flooded = q21_7,
         toilet_hhs = q16_26,
         toilet_share = q16_25
  ) %>% 
  mutate(fuel_wood = ifelse(q19_3==1, 1, 0),
         fuel_grass = ifelse(q19_3==2, 1, 0),
         fuel_dung = ifelse(q19_3==3, 1, 0)) %>% 
  mutate(private_toilet = ifelse(q16_28 == 1, 1, 0),
         satisfied_house = ifelse(q14_30 <=2, 1, 0)) %>% 
  mutate(union = as.factor(union))

#----------------------------------------
# recode father's work 
#----------------------------------------

baseline <- baseline %>%
  mutate(father_work = factor(case_when(
    father_work %in% c(2, 15, 16, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 36, 77, 99) ~ "other",
    father_work %in% c(4, 8) ~ "non_agri_labor",
    father_work %in% c(1, 3) ~ "agriculture",
    father_work %in% c(6, 7, 9, 10, 11, 12, 13, 14, 19, 23) ~ "skilled_work",
    father_work %in% c(17, 18, 21, 22) ~ "business_trader",
    father_work %in% c(5, 20) ~ "salaried_job",
    father_work == 30 ~ "unemployed",
    father_work == 34 ~ "working_abroad"
  )))

#----------------------------------------
# add hygienic latrine variable 
#----------------------------------------

# baseline <- baseline %>%
#   mutate(hygienic_latrine = ifelse(q16_13 == 1 & q16_14 == 1 & q16_11 == 1, 1, 0))

# definition here: https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004256

baseline <- baseline %>% mutate(
  latrine_flush_sewer = ifelse(q16_13==1 & q16_15==1, 1, 0),
  latrine_septic_tank = ifelse(q16_13==1 & q16_15==2, 1, 0),
  latrine_pit_slab_seal = ifelse(q16_13==1 & q16_15==3 & q16_11==1 & q16_14==1, 1, 0),
  latrine_slab_lid_noseal = ifelse(q16_13==1 & q16_15==3 & q16_11==1 & q16_21==1, 1, 0),
  latrine_composting = ifelse(q16_24 ==1, 1, 0),
  latrine_canal_ditch = ifelse(q16_15==4, 1, 0),
  latrine_pit_no_seal = ifelse(q16_13==1 & q16_15==3 & q16_11==1 & (q16_14==2 | q16_14==3), 1, 0),
  latrine_hanging = ifelse(q16_17==1, 1, 0)
)

baseline <- baseline %>%
  mutate(hygienic_latrine = case_when(
    latrine_flush_sewer == 1 | latrine_septic_tank == 1 | latrine_pit_slab_seal == 1 | latrine_slab_lid_noseal == 1 | latrine_composting == 1 ~ 1,
    latrine_canal_ditch == 1 | latrine_pit_no_seal == 1 | latrine_hanging == 1 ~ 0,
    TRUE ~ NA
  ))


#----------------------------------------
# replace missing codes with NA
#----------------------------------------
# replace missing with NA for assets
baseline <- baseline %>% mutate(
  bike = ifelse(bike == 999, NA, bike),
  moto = ifelse(moto == 999, NA, moto),
  boat = ifelse(boat == 999, NA, boat),
  q19_218 = ifelse(q19_218 == 999, NA, q19_218),
  
  q19_6 = ifelse(q19_6 == 9999, NA, q19_6),
  q19_7 = ifelse(q19_7 == 9999, NA, q19_7),
  own_house = ifelse(q19_5==1, 1, 0),
  
  flood_prepared = ifelse(flood_prepared==99, NA, flood_prepared),
  inside_hh_flooded = ifelse(is.na(inside_hh_flooded), 0, inside_hh_flooded),
  latrine_flooded = ifelse(is.na(latrine_flooded), 0, latrine_flooded),
  tubewell_flooded = ifelse(is.na(tubewell_flooded), 0, tubewell_flooded)
  
) 

#----------------------------------------
# flooding variables
#----------------------------------------
# calculate month of year of flooding
baseline <- baseline %>% mutate(
  flood_compound_months = q21_2*30.5,
  flood_union_months = q21_11*30.5
) %>% 
  mutate(date_compound_flood = date - flood_compound_months,
         date_union_flood = date - flood_union_months) %>% 
  mutate(month_compound_flood = month(date_compound_flood),
         month_union_flood = month(date_union_flood),
         month_baseline = month(date)) 

# was flood before or after pregnancy 
baseline <- baseline %>% 
  # conception date 
  mutate(conception_date = date - gestational_age) %>%
  mutate(
    flood_compound_before_preg = ifelse(date_compound_flood < conception_date, 1, 0),
    flood_union_before_preg = ifelse(date_union_flood < conception_date, 1, 0)
  ) %>% 
  mutate(time_since_flood_compound = date - date_compound_flood,
         time_since_flood_union = date - date_union_flood, "weeks") %>% 
  mutate(trimester = case_when(
    gestational_age < 13 ~ 1,
    gestational_age < 27 ~ 2,
    TRUE ~ 3
  ))

# legnth of flooding 
baseline <- baseline %>% 
  rename(num_days_home_flooded = q21_4, 
         num_days_latrine_flooded = q21_6,
         num_days_tubewell_flooded = q21_8)


baseline <- baseline %>% #converting responses in hours to days 
  mutate(num_days_home_flooded = ifelse(q21_4a == 2, num_days_home_flooded / 24, num_days_home_flooded),
        num_days_latrine_flooded = ifelse(q21_6a == 2, num_days_latrine_flooded / 24, num_days_latrine_flooded),
        num_days_tubewell_flooded = ifelse(q21_8a == 2, num_days_tubewell_flooded / 24, num_days_tubewell_flooded))


#----------------------------------------
# water distance
#----------------------------------------

water <- read.csv(paste0(box_path_cradle_data, "Water-distance/Baseline_survey_water_dist.csv")) %>%
  mutate(dataid = as.character(dataid)) %>%
  dplyr::select(dataid, dist_to_perm_water, dist_to_seasonal_water)


# water <- read.csv("/Users/suhi/Downloads/Baseline_survey_water_dist.csv") %>%
#   mutate(dataid = as.character(dataid)) %>%
#   dplyr::select(dataid, dist_to_perm_water, dist_to_seasonal_water, dist_to_any_water)

# merge
baseline <- left_join(baseline, water, by = "dataid")


#----------------------------------------
# EPDS
#----------------------------------------
baseline <- baseline %>% 
  mutate(EPDS = q10_20 + q10_21 + q10_22 + q10_23 + q10_24 +
           q10_25 + q10_26 + q10_27 + q10_28 + q10_29) %>% 
  mutate(depression = ifelse(EPDS>9.5, 1,0),
         depression_severe = ifelse(EPDS>13, 1, 0))


#----------------------------------------
# wealth index
#----------------------------------------
library(psych)

# exclude due to lack of variation
# "q19_21", "q19_28","q19_215", "q19_216", "q19_217","q19_219","q19_220"

asset_cols <- c(
  "q19_22", "q19_23", "q19_24", "q19_25", "q19_26", "q19_27", 
  "q19_29", "q19_210", "q19_211", "q19_212", "q19_213", "q19_214",
  "q19_218"
)

asset_cols_both <- c(
  "q19_22n", "q19_23n", "q19_24n", "q19_25n", "q19_26n", "q19_27n", 
  "q19_29", "q19_210", "q19_211", "q19_212", "q19_213", "q19_214",
  "q19_218"
)


asset_cols_cont <- c(
  "q19_22n", "q19_23n", "q19_24n", "q19_25n", "q19_26n", "q19_27n"
)

baseline <- baseline %>% mutate(
  q19_22n = ifelse(is.na(q19_22n), 0, q19_22n),
  q19_23n = ifelse(is.na(q19_23n), 0, q19_23n),
  q19_24n = ifelse(is.na(q19_24n), 0, q19_24n),
  q19_25n = ifelse(is.na(q19_25n), 0, q19_25n),
  q19_26n = ifelse(is.na(q19_26n), 0, q19_26n),
  q19_27n = ifelse(is.na(q19_27n), 0, q19_27n)
)

prn<-psych::principal(baseline[,asset_cols_cont], rotate="varimax",
                      nfactors=3,covar=T, scores=TRUE)
index=prn$scores[,1]
nlab<-c(1,2,3,4,5)

baseline <- baseline %>% mutate(wealth_index = index,
                                wealth_quintile = as.factor(cut(index,
                                                                breaks=quantile(index, 
                                                                                probs=c(0,0.25, 0.5, 0.75,1)),include.lowest = T)))

baseline <- baseline %>%
  mutate(resilient = ifelse(flood_union == 1 & depression==0, 1, 0))



#----------------------------------------
# flood preparedness
#----------------------------------------

#flood_prep <- read_excel("/Users/suhi/Downloads/flood_preparedness_2024_08_16.xlsx")
flood_prep <- read_excel(paste0(data_dir, "flood_preparedness_2024_08_16.xlsx"))

baseline <- bind_cols(baseline, flood_prep)


#----------------------------------------
# percent of surface water 
#----------------------------------------

#sw_df <- readRDS("/Users/suhi/Downloads/analysis_prop_surface_water.RDS")
sw_df <- readRDS(paste0(data_dir, "analysis_prop_surface_water.RDS")) %>% 
  dplyr::select(-union) %>% 
  mutate(dataid = as.character(dataid))


baseline <- baseline %>%
  left_join(sw_df, by = c("dataid"))


saveRDS(baseline, paste0(data_dir, "baseline_clean.RDS"))
#saveRDS(baseline, "/Users/suhi/Downloads/baseline_clean.RDS")



