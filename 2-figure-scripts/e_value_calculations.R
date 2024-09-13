#############################################
# CRADLE depression and flooding analysis

# Calculate e values for tables 3-5 
#############################################

rm(list = ls())
source(paste0(here::here(), '/0-config.R'))

epds <- readRDS(paste0(data_dir,"epds_individual_regression_results.RDS")) %>% 
  filter(label == "flood_compound")

# function to calculate E-value 
calculate_e_value <- function(RR) {
  if (RR > 1) {
    # Apply formula for RR > 1
    return(RR + sqrt(RR * (RR - 1)))
  } else if (RR < 1) {
    # Apply formula for RR < 1 (use RR* = 1/RR)
    RR_star <- 1 / RR
    return(RR_star + sqrt(RR_star * (RR_star - 1)))
  } else {
    # For RR == 1, E-value is 1
    return(1)
  }
}

# function to calculate E-value for CI lower limit (LL)
calculate_e_value_CI_LL <- function(RR, LL) {
  if (RR <= 1) {
    # If RR < 1, set the E-value for CI lower limit to NA
    return(NA)
  } else {
    # If LL <= 1, E-value is 1
    if (LL <= 1) {
      return(1)
    } else {
      # Apply formula for E-value for CI lower limit when LL > 1
      return(LL + sqrt(LL * (LL - 1)))
    }
  }
}

# Function to calculate E-value for CI upper limit (UL)
calculate_e_value_CI_UL <- function(RR, UL) {
  if (RR >= 1) {
    # If RR <= 1, set the E-value for CI upper limit to NA
    return(NA)
  } else {
    # If UL <= 1, E-value is 1
    if (UL >= 1) {
      return(1)
    } else {
      # Apply formula for E-value for CI upper limit when UL > 1
      return((1/UL) + sqrt((1/UL) * ((1/UL) - 1)))
    }
  }
}


############################################################
# Flooding and moderate or severe depression
###########################################################

reg <- readRDS(paste0(data_dir, "flooding_regression_results_adj.RDS")) %>% 
  filter(label == "latrine_flooded" | label == "flood_compound" | label == "flood_union") %>% 
  dplyr::select(outcome, label, pt_estimate, CI_lower, CI_upper) %>% 
  filter(outcome != "EPDS")

# Apply the E-value calculation to the pt_estimate column and create a new column 'e_value'
reg <- reg %>% 
  mutate(e_value = sapply(pt_estimate, calculate_e_value))

# Apply the E-value CI calculation to the pt_estimate and CI_lower columns
reg <- reg %>%
  mutate(e_value_CI_lower = mapply(calculate_e_value_CI_LL, pt_estimate, CI_lower))


# Apply the E-value CI calculation to the pt_estimate and CI_upper columns
reg <- reg %>%
  mutate(e_value_CI_upper = mapply(calculate_e_value_CI_UL, pt_estimate, CI_upper)) %>% 
  mutate("Adjusted prevalence ratio (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>%
  mutate(e_value = sprintf("%.2f", e_value),
         e_value_CI_lower = sprintf("%.2f", e_value_CI_lower),
         e_value_CI_upper = sprintf("%.2f", e_value_CI_upper)) %>% 
  dplyr::select(outcome, label, "Adjusted prevalence ratio (95% CI)", e_value, e_value_CI_lower, e_value_CI_upper) %>%
  rename("E value" = e_value, "E value LL" = e_value_CI_lower, "E value UL" = e_value_CI_upper, Outcome = outcome, Label = label)

write.csv(reg, file = paste0(table_path, "moderate_severe_depression_e_value.csv"), row.names = FALSE)

############################################################
# Flooding and EPDS score
###########################################################

reg_epds <- readRDS(paste0(data_dir, "flooding_regression_results_adj.RDS")) %>% 
  filter(label == "latrine_flooded" | label == "flood_compound" | label == "flood_union") %>% 
  dplyr::select(outcome, label, pt_estimate, CI_lower, CI_upper) %>% 
  filter(outcome == "EPDS")

# Apply the E-value calculation to the pt_estimate column and create a new column 'e_value'
reg_epds <- reg_epds %>% 
  mutate(e_value = sapply(pt_estimate, calculate_e_value))

# Apply the E-value CI calculation to the pt_estimate and CI_lower columns
reg_epds <- reg_epds %>%
  mutate(e_value_CI_lower = mapply(calculate_e_value_CI_LL, pt_estimate, CI_lower))


# Apply the E-value CI calculation to the pt_estimate and CI_upper columns
reg_epds <- reg_epds %>%
  mutate(e_value_CI_upper = mapply(calculate_e_value_CI_UL, pt_estimate, CI_upper)) %>% 
  mutate("Adjusted prevalence ratio (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>%
  mutate(e_value = sprintf("%.2f", e_value),
         e_value_CI_lower = sprintf("%.2f", e_value_CI_lower),
         e_value_CI_upper = sprintf("%.2f", e_value_CI_upper)) %>% 
  dplyr::select(outcome, label, "Adjusted prevalence ratio (95% CI)", e_value, e_value_CI_lower, e_value_CI_upper) %>%
  rename("E value" = e_value, "E value LL" = e_value_CI_lower, "E value UL" = e_value_CI_upper, Outcome = outcome, Label = label)

write.csv(reg_epds, file = paste0(table_path, "epds_score_e_value.csv"), row.names = FALSE)



############################################################
# Compound flooding and EPDS individual questions
###########################################################

# Apply the E-value calculation to the pt_estimate column and create a new column 'e_value'
epds <- epds %>% 
  mutate(e_value = sapply(pt_estimate, calculate_e_value))


# Apply the E-value CI calculation to the pt_estimate and CI_lower columns
epds <- epds %>%
  mutate(e_value_CI_lower = mapply(calculate_e_value_CI_LL, pt_estimate, CI_lower))


# Apply the E-value CI calculation to the pt_estimate and CI_upper columns
epds <- epds %>%
  mutate(e_value_CI_upper = mapply(calculate_e_value_CI_UL, pt_estimate, CI_upper)) %>% 
  mutate("Adjusted prevalence ratio (95% CI)" = sprintf("%.2f (%.2f, %.2f)", pt_estimate, CI_lower, CI_upper)) %>%
  mutate(e_value = sprintf("%.2f", e_value),
         e_value_CI_lower = sprintf("%.2f", e_value_CI_lower),
         e_value_CI_upper = sprintf("%.2f", e_value_CI_upper)) %>%
  mutate(Variable = c("Difficult to see funny side of things",
                      "Difficult to look forward to enjoyment",
                      "Blamed self unnecessarily when things went wrong",
                      "Felt anxious or worried for no good reason",
                      "Felt scared or panicky for no good reason",
                      "Things getting on top of self",
                      "Difficulty sleeping due to unhappiness",
                      "Felt sad or miserable",
                      "Cried due to unhappiness",
                      "Thoughts of self harm")) %>%
  dplyr::select(Variable, "Adjusted prevalence ratio (95% CI)", e_value, e_value_CI_lower, e_value_CI_upper) %>%
  rename("E value" = e_value, "E value LL" = e_value_CI_lower, "E value UL" = e_value_CI_upper)

write.csv(epds, file = paste0(table_path, "epds_individual_e_value.csv"), row.names = FALSE)
