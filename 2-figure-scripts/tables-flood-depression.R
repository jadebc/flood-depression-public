#############################################
# CRADLE depression and flooding analysis

# Create tables 1-5: 
#############################################

rm(list = ls())
source(paste0(here::here(), '/0-config.R'))

tabdata <- readRDS("/Users/suhi/Downloads/baseline_clean.RDS")

# test = tabdata %>% dplyr::select(depression, depression_severe)
# sum(test$depression) #164
# sum(test$depression_severe) #63

# Table 1 -----------------------------------------------------------------------------------

table1 <- tabdata %>%
  dplyr::select(mother_edu, mother_age, gestational_age, hhsize, income, flood_union, flood_compound, time_since_flood_compound, 
                inside_hh_flooded, num_days_home_flooded, latrine_flooded, num_days_latrine_flooded,
                tubewell_flooded, num_days_tubewell_flooded, flood_prepared) %>% 
  # Create variable for income less than 12,001  
  mutate(income_less_12 = ifelse(income < 6, 1, 0)) %>% 
  dplyr::select(-income) %>% 
  # Move income_less_12 after hhsize
  relocate(income_less_12, .after = hhsize)  

# Convert the factor variable flood_union to numeric 
table1 <- table1 %>%
  mutate(flood_union = case_when(
    flood_union == "0" ~ 0,
    flood_union == "1" ~ 1))

# Convert the factor variable flood_compound to numeric 
table1 <- table1 %>%
  mutate(flood_compound = case_when(
    flood_compound == "0" ~ 0,
    flood_compound == "1" ~ 1))

# Convert difftime variable time_since_flood_compound to numeric in months
table1 <- table1 %>%
  mutate(time_since_flood_compound = as.numeric(time_since_flood_compound, units = "days") / 30.5)

# Calculate N 
summary_stats_N <- table1 %>%
  summarize(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "N")

# Calculate mean
summary_stats_Mean <- table1 %>%
  summarize(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean/%")

# Combine N and mean
comb_summary_stats <- summary_stats_N %>%
  left_join(summary_stats_Mean, by = "Variable")

# Variables that should be percentages 
perc_vars <- c("income_less_12", "flood_union", "flood_compound", "inside_hh_flooded", "latrine_flooded", "tubewell_flooded", "flood_prepared")

# Convert Mean/% column values to percentage where applicable
comb_summary_stats <- comb_summary_stats %>% 
  mutate(`Mean/%` = ifelse(Variable %in% perc_vars, 
                           paste0(sprintf("%.2f", `Mean/%` * 100), "%"), 
                           sprintf("%.2f", `Mean/%`)))

labels <- c(
  "Mother's years of education", "Mother's age in years", "Gestational age in weeks", "Number of household members", "Monthly income < 12,001 taka", "Union flooded for at least one day in past 6 months",
  "Compound flooded for at least one day in past 6 months", "Number of months ago that the compound flooded", "Inside of the home flooded in the past 6 months", "Number of days the home was flooded",
  "Latrine flooded in past 6 months", "Number of days latrine was flooded", "Tubewell flooded in past 6 months", "Number of days the tubewell was flooded", "Respondent feels prepared to handle a flood if it happened tomorrow"
)

comb_summary_stats <- comb_summary_stats %>%
  mutate(Label = labels) %>% 
  relocate(Label, .after = Variable) %>% 
  dplyr::select(-Variable)

write.csv(comb_summary_stats, file = paste0(table_path, "table1_flood_dep.csv"), row.names = FALSE)
#write.csv(comb_summary_stats, "/Users/suhi/Downloads/table1_flood_dep.csv", row.names = FALSE)



# Table 2 -----------------------------------------------------------------------------------

table2 <- tabdata 

#table(table2$prep_1)
#table(table2$prep_2)
#table(table2$prep_3)
#table(table2$prep_4)


# data cleaning
table2 <- table2 %>%
  mutate(across(c(prep_1, prep_2, prep_3, prep_4), 
                ~ifelse(. == "kive on suspended platform", 
                        "live on suspended platform", .)))
table2 <- table2 %>% 
  mutate(across(c(prep_1, prep_2, prep_3, prep_4),
                ~ifelse(. %in% c("move to higher ground/ dry area/ dry area location",
                                 "live on higher ground",
                                 "move to dry location",
                                 "move to high"),
                        "move to higher ground/ dry area", .)))

table2 <- table2 %>%
  mutate(across(c(prep_1, prep_2, prep_3, prep_4), 
                ~ifelse(. == "move to road", 
                        "live on road", .)))

table2 <-table2 %>% 
  mutate(across(c(prep_1, prep_2, prep_3, prep_4), 
                ~ifelse(. == "prepapre stove", 
                        "prepare stove", .)))


# Add dummy column for relocating to suspended platform 
table2 <- table2 %>%
  mutate(relocate_suspended_platform = replace_na(ifelse(
    prep_1 == "live on suspended platform" | 
      prep_2 == "live on suspended platform" | 
      prep_3 == "live on suspended platform" | 
      prep_4 == "live on suspended platform", 
    1, 0), 0))

# Add dummy column for relocating to boat/raft 
table2 <- table2 %>%
  mutate(relocate_boat_raft = replace_na(ifelse(
    prep_1 == "live in boat/ prepare boat" | 
      prep_2 == "live in boat/ prepare boat" | 
      prep_3 == "live in boat/ prepare boat" | 
      prep_4 == "live in boat/ prepare boat", 
    1, 0), 0)) %>%
  mutate(relocate_boat_raft = coalesce(ifelse(
    prep_1 == "prepare raft" | 
      prep_2 == "prepare raft" | 
      prep_3 == "prepare raft" | 
      prep_4 == "prepare raft", 
    1, relocate_boat_raft), relocate_boat_raft))

# Add dummy column for relocating to road
table2 <- table2 %>%
  mutate(relocate_road = replace_na(ifelse(
    prep_1 == "live on road" | 
      prep_2 == "live on road" | 
      prep_3 == "live on road" | 
      prep_4 == "live on road", 
    1, 0), 0))

# Add dummy column for relocating to roof
table2 <- table2 %>%
  mutate(relocate_roof = replace_na(ifelse(
    prep_1 == "live on roof" | 
      prep_2 == "live on roof" | 
      prep_3 == "live on roof" | 
      prep_4 == "live on roof", 
    1, 0), 0))

# Add dummy column for relocating to higher ground or dry area
table2 <- table2 %>%
  mutate(relocate_high_ground_dry_area = replace_na(ifelse(
    prep_1 == "move to higher ground/ dry area"  | 
      prep_2 == "move to higher ground/ dry area" | 
      prep_3 == "move to higher ground/ dry area" | 
      prep_4 == "move to higher ground/ dry area", 
    1, 0), 0))

# Add dummy column for relocating to shelter
table2 <- table2 %>%
  mutate(relocate_shelter = replace_na(ifelse(
    prep_1 == "move to shelter center"  | 
      prep_2 == "move to shelter center" | 
      prep_3 == "move to shelter center" | 
      prep_4 == "move to shelter center", 
    1, 0), 0))

# Add dummy column for relocating to another home
table2 <- table2 %>%
  mutate(relocate_another_home = replace_na(ifelse(
    prep_1 == "move to someone else's home"  | 
      prep_2 == "move to someone else's home" | 
      prep_3 == "move to someone else's home" | 
      prep_4 == "move to someone else's home", 
    1, 0), 0))

# Add dummy column for raise bed
table2 <- table2 %>%
  mutate(raise_bed = replace_na(ifelse(
    prep_1 == "raise bed"  | 
      prep_2 == "raise bed" | 
      prep_3 == "raise bed" | 
      prep_4 == "raise bed", 
    1, 0), 0))

# Add dummy column for raise home
table2 <- table2 %>%
  mutate(raise_home = replace_na(ifelse(
    prep_1 == "raise home"  | 
      prep_2 == "raise home" | 
      prep_3 == "raise home" | 
      prep_4 == "raise home", 
    1, 0), 0))

# Add dummy column for collect dry food
table2 <- table2 %>%
  mutate(collect_dry_food = replace_na(ifelse(
    prep_1 == "collect dry food"  | 
      prep_2 == "collect dry food" | 
      prep_3 == "collect dry food" | 
      prep_4 == "collect dry food", 
    1, 0), 0))

# Add dummy column for prepare cash
table2 <- table2 %>%
  mutate(prepare_cash = replace_na(ifelse(
    prep_1 == "prepare cash"  | 
      prep_2 == "prepare cash"  | 
      prep_3 == "prepare cash"  | 
      prep_4 == "prepare cash" , 
    1, 0), 0))

# Add dummy column for prepare stove
table2 <- table2 %>%
  mutate(prepare_stove = replace_na(ifelse(
    prep_1 == "prepare stove"  | 
      prep_2 == "prepare stove"  | 
      prep_3 == "prepare stove"  | 
      prep_4 == "prepare stove" , 
    1, 0), 0))

# Add dummy column for no preparation
table2 <- table2 %>%
  mutate(no_prep = replace_na(ifelse(
    prep_1 == "no preparation"  | 
      prep_2 == "no preparation"  | 
      prep_3 == "no preparation" | 
      prep_4 == "no preparation" , 
    1, 0), 0))

# Add dummy column for other
table2 <- table2 %>%
  mutate(other = replace_na(ifelse(
    prep_1 == "move safe location" | 
      prep_1 == "move to new char" |
      prep_1 == "purify drinking water" |
      prep_2 == "keep children away from flood water" | 
      prep_3 == "climb tree" | 
      prep_4 == "purify drinking water" , 
    1, 0), 0))

table2 <- table2 %>% dplyr::select(relocate_suspended_platform, relocate_boat_raft, relocate_road, relocate_roof,
                                   relocate_high_ground_dry_area, relocate_shelter, relocate_another_home,
                                   raise_bed, raise_home, collect_dry_food, prepare_cash, prepare_stove, no_prep, other)

# Calculate percentage
tab2_perc <- table2 %>%
  summarize(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "%") %>% 
  mutate(`%` = paste0(sprintf("%.2f", `%` * 100), "%"))

tab2_lab <- c(
  "Temporarily relocate to suspended platform", "Temporarily relocate to boat or raft", "Temporarily relocate to road", 
  "Temporarily relocate to roof", "Temporarily relocate to higher ground or dry area", "Temporarily relocate to shelter", 
  "Temporarily relocate to another home", "Raise bed", "Raise home", "Collect dry food", "Prepare cash", "Prepare stove",
  "No preparation", "Other" 
)

tab2_perc <- tab2_perc %>%
  mutate(Label = tab2_lab) %>% 
  relocate(Label, .after = Variable) %>% 
  dplyr::select(-Variable)

write.csv(tab2_perc, file= paste0(table_path, "tab2_flood_dep.csv"), row.names = FALSE)
#write.csv(tab2_perc, "/Users/suhi/Downloads/table2_flood_dep.csv", row.names = FALSE)

# Table 3 -----------------------------------------------------------------------------------

# reg_unadj <- readRDS("/Users/suhi/Downloads/flooding_regression_results_unadj.RDS") %>% 
#   dplyr::select(-covariates, -model, -parameter_type) 

reg_unadj <- readRDS(paste0(data_dir, "flooding_regression_results_adj.RDS")) %>% 
  dplyr::select(-covariates, -model, -parameter_type) 

# reg_adj <- readRDS("/Users/suhi/Downloads/flooding_regression_results_adj.RDS") %>%
#   filter(label == "latrine_flooded" | label == "flood_compound" | label == "flood_union") %>% 
#   dplyr::select(outcome, label, pt_estimate, CI_lower, CI_upper) %>% 
#   rename(pt_estimate_adj = pt_estimate, CI_lower_adj = CI_lower, CI_upper_adj = CI_upper)
  
reg_adj <- readRDS(paste0(data_dir, "flooding_regression_results_adj.RDS")) %>% 
  filter(label == "latrine_flooded" | label == "flood_compound" | label == "flood_union") %>% 
  dplyr::select(outcome, label, pt_estimate, CI_lower, CI_upper) %>% 
  rename(pt_estimate_adj = pt_estimate, CI_lower_adj = CI_lower, CI_upper_adj = CI_upper)

reg <- reg_unadj %>% left_join(reg_adj, by = c("outcome", "label")) %>%
  mutate(CI = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper),
      CI_adj = sprintf("%.2f (%.2f, %.2f)", pt_estimate_adj, CI_lower_adj, CI_upper_adj)) %>% 
      dplyr::select(-pt_estimate, -CI_lower, -CI_upper, -pt_estimate_adj, -CI_lower_adj, -CI_upper_adj) %>% 
  filter(label == "latrine_flooded" | label == "flood_compound" | label == "flood_union") 

mean <- readRDS(paste0(data_dir, "flooding_mean_results.RDS"))
#mean <- readRDS("/Users/suhi/Downloads/flooding_mean_results.RDS")

reg <- reg %>%
  left_join(mean, by = c("outcome", "label")) 

tab3 <- reg %>% 
  filter(outcome != "EPDS") %>% 
  mutate(mean_unexposed = sprintf("%.2f%%", mean_unexposed * 100),
         mean_exposed = sprintf("%.2f%%", mean_exposed * 100)) %>% 
  relocate(mean_exposed, .after = N) %>%
  relocate(mean_unexposed, .after = mean_exposed) %>% 
  rename("Prevalence among exposed" = mean_exposed, "Prevalence among unexposed" = mean_unexposed,
         "Crude prevalence ratio (95% CI)" = CI, "Adjusted * prevalence ratio (95% CI)" = CI_adj) %>% 
  mutate(Flooding = c("Flooded latrine", "Flooded compound", "Flooded union",
                      "Flooded latrine", "Flooded compound", "Flooded union"),
         Outcome = c("Moderate or severe depression","Moderate or severe depression", "Moderate or severe depression",
                     "Severe depression", "Severe depression", "Severe depression")) %>% 
  relocate(Flooding, .after = outcome) %>% 
  relocate(Outcome, .after = outcome) %>% 
  dplyr::select(-label, - outcome)
  
write.csv(tab3, file = paste0(table_path, "table3_flood_dep.csv"), row.names = FALSE)
#write.csv(tab3, "/Users/suhi/Downloads/table3_flood_dep.csv", row.names = FALSE)

# Table 4 -----------------------------------------------------------------------------------  

tab4 <- reg %>% 
  filter(outcome == "EPDS") %>% 
  relocate(mean_exposed, .after = N) %>% 
  relocate(mean_unexposed, .after = mean_exposed) %>% 
  mutate(mean_exposed = sprintf("%.2f", mean_exposed),
         mean_unexposed = sprintf("%.2f", mean_unexposed)) %>% 
  rename("Mean among exposed" = mean_exposed, "Mean among unexposed" = mean_unexposed,
         "Crude mean difference (95% CI)" = CI, "Adjusted * mean difference (95% CI)" = CI_adj) %>% 
  mutate(Variable = c("Flooded latrine", "Flooded compound", "Flooded union")) %>% 
  relocate(Variable, .before = N) %>% 
  dplyr::select(-outcome, -label)

write.csv(tab4, file = paste0(table_path, "table4_flood_dep.csv"), row.names = FALSE)
#write.csv(tab4, "/Users/suhi/Downloads/table4_flood_dep.csv", row.names = FALSE)

# Table 5 -----------------------------------------------------------------------------------  

#tab5 <- readRDS("/Users/suhi/Downloads/epds_individual_regression_results.RDS")
tab5 <- readRDS(paste0(data_dir,"epds_individual_regression_results.RDS"))

tab5 <- tab5 %>% 
  filter(label == "flood_compound") %>% 
  mutate("Adjusted prevalence ratio (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>% 
  mutate("Prevalence among exposed" = sprintf("%.2f%%", mean_exposed * 100),
         "Prevalence among unexposed" = sprintf("%.2f%%", mean_unexposed * 100)) %>% 
  mutate(Variable = c("Difficult to see funny side of things",
                      "Difficult to look forward to enjoyment",
                      "Blamed self unnecessarily when things went wrong",
                      "Felt anxious or worried for no good reason",
                      "Felt scared or panicky for no good reason",
                      "Things getting on top of self",
                      "Difficulty sleeping due to unhappiness",
                      "Felt sad or miserable",
                      "Cried due to unhappiness",
                      "Thoughts of self harm"
                      )) %>% 
  dplyr::select(Variable, "Prevalence among exposed", "Prevalence among unexposed",
                "Adjusted prevalence ratio (95% CI)")

#write.csv(tab5, "/Users/suhi/Downloads/table5_flood_dep.csv", row.names = FALSE)  
write.csv(tab5, file = paste0(table_path, "table5_flood_dep.csv"), row.names = FALSE)