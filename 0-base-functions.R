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
  dat <- data.frame(Ws, Y)
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
    covs <- screen_covariates(Y, data[,covariates], family = family)
    cormatrix <- cor(data[,c(covs)])
    
    # check for collinearity
    diag(cormatrix) <- 0
    assert_that(max(cormatrix) <= 0.6 , msg="Covariates are highly correlated")
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
