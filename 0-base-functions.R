#########################################
# CRADLE depression and flooding analysis

# regression analysis of flooding and depression
#########################################

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
  
  if(family=="binomial") Y <- as.factor(Y)
    
  if(!is.null(covariates)){
    # covariate screening
    Ws <- data %>% dplyr::select(all_of(covariates))
    covs <- screen_covariates(Y, Ws, family = family)
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
  if(family(model)$family =="binomial"){
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
# Description: Runs a random forest model with preprocessing, hyperparameter tuning, and variable importance analysis

# Args/Options:
# data: a data frame containing the outcome and predictor variables
# outcome: a string specifying the name of the outcome variable
# predictors: a vector of strings specifying the names of the predictor variables

# Returns: a list containing:
#   - rf_model: the final random forest model
#   - oob_error: out-of-bag error rate
#   - oob_accuracy: out-of-bag accuracy
#   - var_importance_sorted: data frame of sorted variable importance
#   - top_vars: vector of variables in the top 20% of importance
# Output: prints the best model from tuning and the sorted variable importance

run_random_forest <- function(data, outcome, predictors){
  # restrict to necessary columns and compete cases
  vi_data <- data %>% dplyr::select(all_of(c(outcome, predictors))) 
  vi_data <- vi_data[complete.cases(vi_data),]

  # recode binary variables as factors
  binary_vars <- colnames(vi_data)[apply(vi_data, 2, function(x) length(unique(x))==2)]

  # write a function to convert a numeric 0/1 to a factor "no"/"yes"
  convert_binary <- function(x) {
    as.factor(ifelse(x == 0, "no", "yes"))
  }
  
  vi_data <- vi_data %>% mutate_at(binary_vars, convert_binary)
  
  # Standardize numeric features ------------------------------------------------
  numeric_vars <- sapply(vi_data, is.numeric)
  data_scaled <- vi_data
  data_scaled[, numeric_vars] <- scale(data_scaled[, numeric_vars])
  
  # Create a formula for modeling -----------------------------------------
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  
  # Define tuning grid ----------------------------------------------------
  tuning_grid <- expand.grid(
    mtry = seq(2, ncol(data_scaled) - 1, by = 2),
    min.node.size = c(1, 3, 5),
    splitrule = "gini"
  )
  
  # Set up cross-validation
  # use 10-fold cv
  ctrl <- trainControl(method = "cv", 
                       number = 10, 
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
  
  # Train model
  rf_model <- train(formula, data = data_scaled,
                    method = "ranger",
                    tuneGrid = tuning_grid,
                    trControl = ctrl,
                    metric = "ROC")
  
  # View best model
  print(rf_model)
  
  # Random Forest Method
  rf_model <- randomForest(formula, 
                           data = data_scaled, 
                           min.node.size = 3,
                           mtry = 2,
                           splitrule = "gini",
                           importance = TRUE, 
                           ntree = 500)
  
  # Get OOB error rate
  oob_error <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  
  # Calculate OOB accuracy
  oob_accuracy <- 1 - oob_error
  
  # Get variable importance
  var_importance <- importance(rf_model, type = 1)
  
  # Sort importance and convert to dataframe
  var_importance_sorted <- data.frame(
    variable = rownames(var_importance),
    importance = var_importance[, "MeanDecreaseAccuracy"]
  ) %>%
    arrange(desc(importance))
  
  # Print sorted importance
  print(var_importance_sorted)
  
  var_importance_sorted = var_importance_sorted %>% mutate(
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

  # List variables in top 20% of importance
  top_vars <- var_importance_sorted %>%
    filter(importance >= quantile(importance, 0.8)) %>%
    pull(variable)
  
  return(list(
    rf_model = rf_model,
    oob_error = oob_error, 
    oob_accuracy = oob_accuracy, 
    var_importance_sorted = var_importance_sorted, 
    top_vars = top_vars
  ))
}



