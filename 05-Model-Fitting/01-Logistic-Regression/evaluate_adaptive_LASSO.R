# Evaluate Adaptive LASSO Fits




# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable", "glmnet", "doParallel", "parallel", "pROC")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract recoded datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})

imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})

the.data <- clean.data[[2]]
the.data <- imputed.data[[2]]
the.validation.data <- imputed.data[[3]]




the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]


the.data.complete <- the.data[complete.cases(the.data),]
the.data.complete <- the.validation.data[complete.cases(the.validation.data),]


the.predictors <- the.data.complete[,the.vars]


the.interactions <- t(combn(the.vars, 2))
the.interactions <- apply(the.interactions, MARGIN = 1, paste0, collapse="*")



# the.predictors.formula <- as.formula(paste0("~", paste0(the.interactions, collapse = "+")))
the.predictors.formula <- as.formula(paste0("~", paste0(the.vars, collapse = "+")))

x <- model.matrix(the.predictors.formula, data = the.predictors)[,-1]

y <- the.data.complete$Default_ind












# formulating the cost function 
## use population averages, could use individuals, but would be very involved

## averages of the average monthly card debt
## harmonic.mean.avg.card.debt <- tapply(the.data.complete$avg_card_debt, the.data.complete$Default_ind, function(x){length(x) / sum(1/x)})

geometric.mean.avg.card.debt <- tapply(the.data.complete$avg_card_debt, the.data.complete$Default_ind, function(x){exp(mean(log(x)))})

## average of the number of non mortgage deliquencies in past 12 months , divided by 12 to get monthly
quadratic.mean.n.delinq <- tapply(the.data.complete$non_mtg_acc_past_due_12_months_num, the.data.complete$Default_ind, function(x){sqrt(mean(x^2))/12})

## proportion of avg monthly debt for bank profit 
### conservative? small to account for avg debt being spread across all cards (unknown number, best we have is # cards opened in last 36 months, many zeros)
profit.merchant <- 1/100
profit.interest <- 1/10


## actual function
calculate.Cost <- function(TP, FP, TN, FN){
  
  
  cost <-  TN * (geometric.mean.avg.card.debt["Did Not Default"]*(profit.merchant+profit.interest*quadratic.mean.n.delinq["Did Not Default"])) - FN * geometric.mean.avg.card.debt["Defaulted"]
  
  
  return(cost)
  
}


# works as designed
## calculate.Cost(TP = 0, FP = 0, TN = 0, FN = 1)














gamma <- c(0.25, 0.5, 1, 2, 4) #0.5 | 1 | 2


load("./05-Model-Fitting/01-Logistic-Regression/alasso_fits.rds")

# 
# the.fit <- the.models[[1]]
# 





evaluate.Thresholds <- function(.index, .other){
  
  
  the.params <- data.frame(gamma = .other[[1]],
                           lambda = .other[[2]][.index],
                           auc = as.numeric(.other[[3]][.index]))
  
  
  the.probs <- .other[[4]][,.index]
  
  
  the.metrics <- data.frame(threshold = .other[[5]][[.index]]$thresholds, 
                            sensitivity = .other[[5]][[.index]]$sensitivities, 
                            specificity = .other[[5]][[.index]]$specificities) %>%
                  mutate(youden = sensitivity + specificity - 1, 
                         d2c = sqrt((1-sensitivity)^2+(1-specificity)^2))
  
  the.truth <- .other[[6]]
  
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl) 
  
  clusterExport(cl, varlist = c("the.probs", "the.truth"), envir = environment())
  
  the.predictions <- parLapply(cl, the.metrics$threshold, function(.threshold){
    require(dplyr)
    these.predictions <- factor(ifelse(the.probs>.threshold, "Defaulted", "Did Not Default"), levels = c("Defaulted", "Did Not Default")) %>% 
      relevel(., ref = "Did Not Default")
    
    the.df <- data.frame(truth = the.truth, prediction = these.predictions) %>% 
      summarise(
        TP = sum(truth == "Defaulted" & prediction == "Defaulted"), 
        FP = sum(truth == "Did Not Default" & prediction == "Defaulted"), 
        TN = sum(truth == "Did Not Default" & prediction == "Did Not Default"), 
        FN = sum(truth == "Defaulted" & prediction == "Did Not Default"), 
        n = n()
      )
    return(the.df)
  })
  
  stopCluster(cl)
  
  
  
  the.metrics <- bind_cols(the.metrics, bind_rows(the.predictions)) %>%
    mutate(profit = calculate.Cost(TP, FP, TN, FN))
  
  # the most profitable threshold
  .index.profit <- which(the.metrics$profit==the.metrics$profit[which.max(the.metrics$profit)])
  # the most accurate threshold
  .index.accurate <- which(the.metrics$d2c==the.metrics$d2c[which.min(the.metrics$d2c)])
  
  
  the.best.thresholds <- bind_rows(the.metrics[.index.profit,], the.metrics[.index.accurate,]) %>% 
    bind_cols(the.params, .) %>% 
    mutate(Model = .index) %>%
    select(Model, everything())
  
  return(the.best.thresholds)
  
  
}











evaluate.Adaptive.LASSO <- function(.index){
  
  
  the.fit <- the.models[[.index]]
  
  the.gamma <- gamma[.index]
  
  fit.lambdas <- the.fit$lambda
  fit.probs <- predict(the.fit, x, type = "response")
  
  
  ### estimate ROC curves for each fit with different lambda
  fit.rocs <- apply(fit.probs, MARGIN = 2, function(x){roc(y, x, levels = c("Did Not Default", "Defaulted"), direction = "<")})
  #### extract auc from each
  fit.rocs.auc <- sapply(fit.rocs, function(x){pluck(x, "auc")})
  

  
  the.truth <- the.data.complete$Default_ind
  
 
  
  
  
  total1 <- ncol(fit.probs)
  pb1 <- txtProgressBar(min = 0, max = total1, style = 2)
  
  model.evaluations <- lapply(1:ncol(fit.probs), function(.index, .other){
    setTxtProgressBar(pb1, .index);
    evaluate.Thresholds(.index, .other)
    }, 
    .other = list(the.gamma, fit.lambdas, fit.rocs.auc, fit.probs, fit.rocs, the.truth))
  
  return(model.evaluations)
}









total <- sum(unlist(lapply(the.models, function(x){length(x$lambda)})))
pb <- txtProgressBar(min = 0, max = total, style = 3)


# 
# all.model.evaluations <- lapply(1:length(the.models), function(.index){
#   setTxtProgressBar(pb, .index);
#   evaluate.Adaptive.LASSO(.index)
# })
# 






k <- 0


all.model.evaluations <- lapply(1:length(the.models), function(.index){
  # setTxtProgressBar(pb, .index);
  
  
  
  the.fit <- the.models[[.index]]
  
  the.gamma <- gamma[.index]
  
  fit.lambdas <- the.fit$lambda
  fit.probs <- predict(the.fit, x, type = "response")
  
  
  ### estimate ROC curves for each fit with different lambda
  fit.rocs <- apply(fit.probs, MARGIN = 2, function(x){roc(y, x, levels = c("Did Not Default", "Defaulted"), direction = "<")})
  #### extract auc from each
  fit.rocs.auc <- sapply(fit.rocs, function(x){pluck(x, "auc")})
  
  
  
  the.truth <- the.data.complete$Default_ind
  
  
  
  
  
  # total1 <- ncol(fit.probs)
  # pb1 <- txtProgressBar(min = 0, max = total1, style = 2)
  
  model.evaluations <- lapply(1:ncol(fit.probs), function(.index, .other){
    k <<- k+1
    setTxtProgressBar(pb, k);
    evaluate.Thresholds(.index, .other)
  }, 
  .other = list(the.gamma, fit.lambdas, fit.rocs.auc, fit.probs, fit.rocs, the.truth))
  
  return(model.evaluations)

})










save(all.model.evaluations, file = "./05-Model-Fitting/01-Logistic-Regression/model_metrics_validation.rds")






# 
# 
# 
# 
# 
# 
# 
# 
# fit.lambdas <- the.fit$lambda
# fit.probs <- predict(the.fit, x, type = "response")
# 
# 
# ### estimate ROC curves for each fit with different lambda
# fit.rocs <- apply(fit.probs, MARGIN = 2, function(x){roc(y, x, levels = c("Did Not Default", "Defaulted"), direction = "<")})
# #### extract auc from each
# fit.rocs.auc <- sapply(fit.rocs, function(x){pluck(x, "auc")})
# 
# #### find ROC with highest auc
# fit.rocs[[names(fit.rocs.auc[which.max(fit.rocs.auc)])]]
# 
# ### what is index for best lambda used
# best.fit <- which(names(fit.rocs)==names(fit.rocs.auc[which.max(fit.rocs.auc)]))
# 
# ### actual lambda value
# lambda <- the.fit$lambda[best.fit]
# 
# 
# 
# 
# 
# 
# 
# 
# 
















# Former code that used the repeated cross-validation, we should hard-code to incorporate other validation dataset and other fit criteria
# fit2 <- cv.glmnet(x=x, y=y, alpha=1, family="binomial", penalty.factor=adaptive.weights)
#
# plot(fit2)
# #fit2$lambda.min
# #fit2$lambda.1se
# coef(fit2, s=fit2$lambda.min)
# coef(fit2, s=fit2$lambda.1se)


































