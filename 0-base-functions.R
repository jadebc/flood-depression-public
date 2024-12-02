#############################################################################################
# CRADLE depression and flooding analysis

# functions for regression analysis, random forest analysis and calculating wash outcomes 
#############################################################################################

##############################################
##############################################
# Documentation: screen_covariates
# Usage: screen_covariates(Y, Ws, family = "gaussian", pval = 0.2, print = TRUE)
# Description: Screens covariates for association with the outcome using likelihood ratio tests

# Args/Options:
# Y: the outcome variable
# Ws: a data frame or matrix of potential covariates
# family: the family to be used in the glm function (DEFAULT: "gaussian")
# pval: the p-value threshold for selecting covariates (DEFAULT: 0.2)
# print: a boolean specifying whether to print the results (DEFAULT: TRUE)

# Returns: a vector of names of selected covariates
# Output: prints the selected covariates and their p-values if print is TRUE

screen_covariates <- function (Y, Ws, family = "gaussian", pval = 0.2, print = TRUE){
  require(lmtest)
  Ws <- as.data.frame(Ws)
  dat <- bind_cols(Y, Ws)
  colnames(dat)[1] = "Y"
  dat <- dat[complete.cases(dat), ]
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  rownames(LRp) <- names(Ws)
  colnames(LRp) <- "P-value"
  for (i in 1:nW) {
    dat$W <- dat[, i]
    if (class(dat$W) == "factor" & dim(table(dat$W)) == 
        1) {
      fit1 <- fit0 <- glm(Y ~ 1, data = dat, family = family)
    }
    else {
      fit1 <- glm(Y ~ W, data = dat, family = family)
      fit0 <- glm(Y ~ 1, data = dat, family = family)
    }
    LRp[i] <- lrtest(fit1, fit0)[2, 5]
  }
  
  
  p20 <- ifelse(LRp < pval, 1, 0)
  if (print == TRUE) {
     
    if (sum(p20) > 0) {
      LRps <- matrix(LRp[p20 == 1, ], ncol = 1)
      rownames(LRps) <- names(Ws)[p20 == 1]
      colnames(LRps) <- "P-value"
      cat(paste("\n\nCovariates selected (P<", pval, "):\n", 
                sep = ""))
      print(LRps)
    }
    else {
      cat(paste("\nNo covariates were associated with the outcome at P<", 
                pval))
    }
  }
  return(names(Ws)[p20 == 1])
}

##############################################
##############################################
# Documentation: fit_glm
# Usage: fit_glm(data, Y_name, A_name, family = "gaussian", covariates = NULL)
# Description: Fits a generalized linear model (GLM) with optional covariate screening and adjustment

# Args/Options:
# data: a data frame containing the outcome, treatment, and covariate variables
# Y_name: a string specifying the name of the outcome variable in the data frame
# A_name: a string specifying the name of the treatment variable in the data frame
# family: the family to be used in the glm function (DEFAULT: "gaussian")
# covariates: a vector of covariate names to be considered for adjustment (DEFAULT: NULL)

# Returns: a data frame with model estimates, confidence intervals, and other relevant information
# Output: prints the results data frame

fit_glm <- function(data, Y_name, A_name, family = "gaussian", covariates = NULL){
  Y <- data[,Y_name]
  
  if(family!="gaussian") Y <- as.factor(Y)
    
  if(!is.null(covariates)){
    # covariate screening
    Ws <- data %>% dplyr::select(all_of(covariates))
    screen_family = ifelse(family=="gaussian","gaussian","binomial")
    covs <- screen_covariates(Y, Ws, family = screen_family)
    if(length(covs)>1){
      # check for collinearity
      cormatrix <- cor(data[,c(covs)])
      diag(cormatrix) <- 0
      assert_that(max(cormatrix) <= 0.6 , msg="Covariates are highly correlated")
    } 
    
  }

  # fit glm
  if(!is.null(covariates)){
    glm_formula <- as.formula(paste(Y_name, "~", A_name, "+", paste(covs, collapse = "+")))
  }else{
    glm_formula <- as.formula(paste(Y_name, "~", A_name))
  }
  glm_fit <-glm(glm_formula, data = data, family = family)
  
  estimates <- get_estimate(glm_fit)
  
  if(!is.null(covariates)){
    estimates =estimates %>% mutate(
      covariates = paste0(covs, collapse=", "),
      model = "adjusted"
      )
  } 
  if(is.null(covariates)){
    estimates =estimates %>% mutate(
      covariates = NA,
      model = "unadjusted")
  } 
  
  estimates <- estimates %>% mutate(parameter_type = ifelse(family=="binomial","odds ratio","mean difference"))
  estimates <- estimates %>% mutate(parameter_type = ifelse(family=="poisson","prevalence ratio","mean difference"))
  
  estimates <- estimates %>% mutate(label = rownames(estimates),
                                    outcome = Y_name)
  estimates <- estimates %>% dplyr::select(outcome, label, everything()) %>% 
    rename(CI_lower = `2.5 %`,
           CI_upper = `97.5 %`)
  rownames(estimates) <- NULL
  estimates$label[1] = A_name
  
  print(estimates)
  
  return(estimates)

}


##############################################
##############################################
# Documentation: get_estimate
# Usage: get_estimate(model)
# Description: Extracts point estimates and confidence intervals from a GLM object

# Args/Options:
# model: a GLM object from which to extract estimates

# Returns: a data frame containing the number of observations, point estimates, and confidence intervals
# Output: none (returns the data frame silently)

get_estimate <- function(model){
  if(family(model)$family =="gaussian"){
    out = cbind(pt_estimate = coef(model), confint(model))
  }
  if(family(model)$family %in% c("binomial", "poisson")){
    out = exp(cbind(pt_estimate = coef(model), confint(model)))
  }
  # remove intercept
  out = as.data.frame(out)
  out = out[-1,]
  
  # include N in output
  out = cbind(N = nobs(model), out)
  
  return(out)
}


##############################################
##############################################
# Documentation: run_random_forest
# Usage: run_random_forest(data, outcome, predictors)
# Description: Runs a random forest model using cforest with preprocessing, hyperparameter tuning, and variable importance analysis
#
# Args/Options:
# data: a data frame containing the outcome and predictor variables
# outcome: a string specifying the name of the outcome variable
# predictors: a vector of strings specifying the names of the predictor variables
#
# Returns: a list containing:
#   - rf_model: the final random forest model (cforest object)
#   - tuned_model: the tuned model object from caret
#   - best_mtry: the best mtry value found during tuning
#   - oob_error: out-of-bag error rate
#   - oob_accuracy: out-of-bag accuracy
#   - var_importance_sorted: data frame of sorted variable importance with categories
#   - top_vars: vector of variables in the top 25% of importance
#
# Output: 
#   - prints the best mtry value
#   - prints the sorted variable importance
#
# Note: This function uses the cforest method from the party package and tunes only the mtry parameter.
#       Binary variables are converted to factors, and numeric variables are standardized.
##############################################
##############################################

run_random_forest <- function(data, outcome, predictors) {
  library(party)
  library(caret)
  
  # Restrict to necessary columns and complete cases
  vi_data <- data %>% dplyr::select(all_of(c(outcome, predictors))) 
  vi_data <- vi_data[complete.cases(vi_data),]
  
  # Recode binary variables as factors
  binary_vars <- colnames(vi_data)[apply(vi_data, 2, function(x) length(unique(x))==2)]
  convert_binary <- function(x) {
    as.factor(ifelse(x == 0, "no", "yes"))
  }
  vi_data <- vi_data %>% mutate_at(binary_vars, convert_binary)
  
  # Create a formula for modeling
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  
  # Set up cross-validation
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  # Tune mtry
  set.seed(123)  # for reproducibility
  mtry_grid <- expand.grid(mtry = seq(2, length(predictors), by = 2))
  
  tuned_model <- train(
    formula,
    data = vi_data,
    method = "cforest",
    trControl = ctrl,
    tuneGrid = mtry_grid,
    controls = cforest_unbiased(ntree = 500),
    metric = "ROC"
  )
  
  best_mtry <- tuned_model$bestTune$mtry
  
  # Print best mtry
  cat("Best mtry:", best_mtry, "\n")
  
  # Train final model with best mtry and a fixed mincriterion
  final_ctrl <- cforest_unbiased(ntree = 500, mtry = best_mtry)  # You can adjust this value if needed
  rf_model <- cforest(formula, data = vi_data, controls = final_ctrl)
  
  # Get variable importance
  var_importance <- varimp(rf_model)
  
  # Sort importance and convert to dataframe
  var_importance_sorted <- data.frame(
    variable = names(var_importance),
    importance = var_importance
  ) %>%
    arrange(desc(importance))
  
  # Print sorted importance
  print(var_importance_sorted)
  
  # Add variable categories
  var_importance_sorted <- var_importance_sorted %>% mutate(
    var_cat = case_when(
      variable %in% c("bike", "boat", "elec","moto", "fuel_dung", "fuel_grass", "fuel_wood",
                      "income","hhsize","wealth_index") ~ "Economic",
      variable %in% c("mother_age", "mother_edu", "father_edu","gestational_age") ~ "Demographic",
      variable %in% c("dist_to_perm_water", "dist_to_seasonal_water","flood_prepared", "inside_hh_flooded", "latrine_flooded", "tubewell_flooded",
                      "flood_compound", "flood_union") ~ "Water and flooding",
      variable %in% c("n_cow", "n_goat", "n_chicken") ~ "Animal ownership",
      variable %in% c("own_house", "private_toilet", "satisfied_house") ~ "Housing"
    )
  )
  
  # List variables in top 25% of importance
  top_vars <- var_importance_sorted %>%
    filter(importance >= quantile(importance, 0.75)) %>%
    pull(variable)
  
  # Calculate OOB error
  oob_prediction <- predict(rf_model, OOB = TRUE, type = "response")
  oob_error <- mean(oob_prediction != vi_data[[outcome]])
  oob_accuracy <- 1 - oob_error
  
  return(list(
    rf_model = rf_model,
    tuned_model = tuned_model,
    best_mtry = best_mtry,
    oob_error = oob_error, 
    oob_accuracy = oob_accuracy, 
    var_importance_sorted = var_importance_sorted, 
    top_vars = top_vars
  ))
}

##############################################
# Function: calc_wash_outcomes
# Usage: calc_wash_outcomes(df)
# Description: Calculates various Water, Sanitation, and Hygiene (WASH) outcomes based on survey responses and adds new variables to the dataframe. These outcomes include indicators for improved sanitation, sanitation ladder, open defecation, improved water, water ladder, and handwashing with soap and water.
# Args/Options:
#   df: A dataframe containing survey data, with columns for various WASH-related questions.
# Returns: The input dataframe with additional columns representing WASH outcomes:
#   - improved_san: Binary indicator (1 or 0) for improved sanitation, based on criteria for flush type, slab, and composting toilets.
# - san_ladder: Categorical variable representing the sanitation ladder, with levels "Safely managed," "Basic," "Limited," or "Unimproved."
# - open_defecation: Binary indicator (1 or 0) for open defecation.
# - improved_water: Binary indicator (1 or 0) for improved water sources, based on accepted types.
# - water_ladder: Categorical variable representing the water ladder, with levels "Basic," "Limited," or "Unimproved," based on the time to collect water.
# - handwash_soap_water: Binary indicator (1 or 0) for the presence of both soap and water at handwashing facilities.
# Note: Improved sanitation facilities are those designed to hygienically separate excreta from human contact, and include: flush/pour flush toilets connected to piped sewer systems, septic tanks or pit latrines; pit latrines with slabs (including ventilated pit latrines), and composting toilets (https://washdata.org/monitoring/sanitation).
# Improved drinking water sources are those that have the potential to deliver safe water by nature of their design and construction, and include: piped water, boreholes or tubewells, protected dug wells, protected springs, rainwater, and packaged or delivered water (https://washdata.org/monitoring/drinking-water).
##############################################
calc_wash_outcomes <- function(df) {
  df <- df %>%
    mutate(q15_91 = ifelse(is.na(q15_91), 0, q15_91),
           q15_92 = ifelse(is.na(q15_92), 0, q15_92)) %>% 
    mutate(improved_san = ifelse((q16_13 == 1 & q16_15 %in% c(1:3) & q16_14 == 1 & q16_11==1) |# has slab, pour flush flushes to approved source & has functional water seal
                                   q16_24 == 1 , # or latrine is composting toilet 
                                 1, 0),
           san_ladder = case_when(
             q16_25 == 0 & improved_san == 1 ~ "Basic", # Unshared improved
             q16_25 == 1 & improved_san == 1 ~ "Limited", # Shared improved
             improved_san == 0 ~ "Unimproved",
             is.na(q16_25) & is.na(improved_san) ~ "Don't know/Missing"
           ),
           open_defecation_child = ifelse(q16_1 %in% c(1, 2), 1, 0),
           improved_water = ifelse(q17_1 %in% c(1:8), 1, 0), # tubewells, boreholes, taps, piped, dug wells with concrete reinforcement
           water_ladder = case_when(
             q17_1b <= 30 & improved_water == 1 ~ "Basic", # <=30 min improved
             q17_1b > 30 & improved_water == 1 ~ "Limited", # >30min improved
             improved_water == 0 ~ "Unimproved",
             TRUE ~ "Uncategorized"),
           improved_handwash_latrine = if_else(q15_8 == 2 & q15_91 == 1 & q15_92 == 1, 1, 0),
           improved_handwash_kitchen = if_else(q15_8 == 3 & q15_91 == 1 & q15_92 == 1, 1, 0),
           handwash_soap_water = ifelse(q15_91 == 1 & q15_92 == 1, 1, 0)
           
    )
  return(df)
}