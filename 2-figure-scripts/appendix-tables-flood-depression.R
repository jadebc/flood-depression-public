#############################################
# CRADLE depression and flooding analysis

# Create tables S1-S2: 
#############################################

rm(list = ls())
source(paste0(here::here(), '/0-config.R'))

d <- readRDS(paste0(data_dir, "depression_surface_water_regression_results.RDS"))

# Table S1 -----------------------------------------------------------------------------------

s1 <- d %>% 
  filter(outcome != "EPDS") %>% 
  filter(label == "dist_to_seasonal_water" | label == "dist_to_perm_water") 

s1_unadj <- s1 %>% 
  filter(is.na(covariates)) %>% 
  mutate("Crude prevalence ratio (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>% 
  dplyr::select(outcome, label, N, "Crude prevalence ratio (95% CI)")

s1_adj <- s1 %>% 
  filter(!is.na(covariates)) %>% 
  mutate("Adjusted * prevalence ratio (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>% 
  dplyr::select(outcome, label, "Adjusted * prevalence ratio (95% CI)")

s1 <- s1_unadj %>% 
  left_join(s1_adj, by = c("outcome", "label")) %>% 
  mutate(Outcome = case_when(
    outcome == "depression" ~ "Moderate or severe depression",
    outcome == "depression_severe" ~ "Severe depression"),
    Variable = case_when(
      label == "dist_to_seasonal_water" ~ "Seasonal water",
      label == "dist_to_perm_water" ~ "Permanent water"
    )) %>% 
  relocate(Variable, .before = N) %>% 
  relocate(Outcome, .before = Variable) %>% 
  dplyr::select(-outcome, -label)

write.csv(s1, file = paste0(table_path, "s1_dist_to_sw_dep.csv"), row.names = FALSE)
#write.csv(s1, "/Users/suhi/Downloads/s1_dist_to_sw_dep.csv", row.names = FALSE)


# Table S2 -----------------------------------------------------------------------------------

s2 <- d %>% 
  filter(outcome == "EPDS") %>% 
  filter(label == "dist_to_seasonal_water" | label == "dist_to_perm_water") 

s2_unadj <- s2 %>% 
  filter(is.na(covariates)) %>% 
  mutate("Crude mean difference (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>% 
  dplyr::select(outcome, label, N, "Crude mean difference (95% CI)")

s2_adj <- s2 %>% 
  filter(!is.na(covariates)) %>% 
  mutate("Adjusted * mean difference (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>% 
  dplyr::select(outcome, label, "Adjusted * mean difference (95% CI)")

s2 <- s2_unadj %>% 
  left_join(s2_adj, by = c("outcome", "label")) %>% 
  rename(Outcome = outcome) %>% 
  mutate(Variable = case_when(
    label == "dist_to_seasonal_water" ~ "Seasonal water",
    label == "dist_to_perm_water" ~ "Permanent water"
  )) %>% 
  relocate(Variable, .before = N) %>% 
  dplyr::select(-label)

write.csv(s2, file = paste0(table_path, "s2_dist_to_sw_epds.csv"), row.names = FALSE)
#write.csv(s2, "/Users/suhi/Downloads/s2_dist_to_sw_epds.csv", row.names = FALSE)

  
