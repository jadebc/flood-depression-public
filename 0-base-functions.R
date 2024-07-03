#########################################
# CRADLE depression and flooding analysis

# regression analysis of flooding and depression
#########################################

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


# write an R function that obtains the odds ratio and 95% confidence interval for a glm model
# or the mean difference and confidence interval 
# for a t-test
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
