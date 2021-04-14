# Adaptive LASSO
##
## Load data and prepare for model fitting
##
## Fit ridge regression
##
## Calculate adaptive weights
##
## Fit LASSO regression
##
## Evaluate model fit, feature selection, ...
##
## Manually iterate to investigate accuracy, thresholds, cost, auc, ...
###
### Hyperparameters 
####
#### lambda for regularization 
#### gamma for adaptive weights)







# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable", "glmnet", "doParallel", "parallel")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract recoded datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})

imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})

the.data <- clean.data[[2]]
the.data <- imputed.data[[2]]



the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]


the.data.complete <- the.data[complete.cases(the.data),]

the.predictors <- the.data.complete[,the.vars]


the.interactions <- t(combn(the.vars, 2))
the.interactions <- apply(the.interactions, MARGIN = 1, paste0, collapse="*")



# the.predictors.formula <- as.formula(paste0("~", paste0(the.interactions, collapse = "+")))
the.predictors.formula <- as.formula(paste0("~", paste0(the.vars, collapse = "+")))

x <- model.matrix(the.predictors.formula, data = the.predictors)[,-1]

y <- the.data.complete$Default_ind












# Adaptive LASSO
set.seed(74)


## Ridge regression
# 
# ### can manually provide sequence of lambdas
# the.lambdas <- NULL
# fit1 <- glmnet(x=x, y=y, family = "binomial", alpha = 0, lambda = the.lambdas)
# 
# ### use model fits for all lambdas to predict on training data (x)
# fit1.preds <- predict(fit1, x, type = "class")
# fit1.probs <- predict(fit1, x, type = "response")
# 
# 
# 
# #HERE is where we can use different fit criteria to select hyperparameter
# #NEED to predict on the VALIDATION dataset too
# 
# 
# ### estimate ROC curves for each fit with different lambda
# library(pROC)
# fit1.rocs <- apply(fit1.probs, MARGIN = 2, function(x){roc(y, x)})
# #### extract auc from each
# fit1.rocs.auc <- sapply(fit1.rocs, function(x){pluck(x, "auc")})
# 
# #### find ROC with highest auc
# fit1.rocs[[names(fit1.rocs.auc[which.max(fit1.rocs.auc)])]]
# 
# ### what is index for best lambda used
# best.fit1 <- which(names(fit1.rocs)==names(fit1.rocs.auc[which.max(fit1.rocs.auc)]))
# 
# ### actual lambda value
# lambda <- fit1$lambda[best.fit1]




# Former code that used the repeated cross-validation, we should hard-code to incorporate other validation dataset and other fit criteria

# Repeated CV for ridge
fit1 <- cv.glmnet(x=x, y=y, alpha=0, family="binomial")

###Examine tuning
plot(fit1)
###fit1$lambda.min
###fit1$lambda.1se

lambda <- fit1$lambda.1se








## Calculation of adaptive weights

ridge.coefs <- as.matrix(coef(fit1, s=lambda)[-1])

gamma <- c(0.25, 0.5, 1, 2, 4) #0.5 | 1 | 2



fit.Adaptive.LASSO <- function(.gamma){

        require(glmnet)
        adaptive.weights <- 1/abs(ridge.coefs)^.gamma
        
        
        ### Correction for extreme weights
        # table(is.infinite(adaptive.weights))
        adaptive.weights[is.infinite(adaptive.weights)] <- max(adaptive.weights[adaptive.weights!=Inf]) # 999999
        
        #### force in variable by setting weight to zero
        # adaptive.weights[c(1:3, 4:7)] <- 0
        
        
        
        
        
        ## LASSO fit with adaptive weights
        
        ### can manually provide sequence of lambdas
        the.lambdas <- NULL
        fit2 <- glmnet(x=x, y=y, family = "binomial", alpha = 1, lambda = the.lambdas, penalty.factor=adaptive.weights)
        
        
        
        return(fit2)
}






cl <- makePSOCKcluster(4)
registerDoParallel(cl) 

clusterExport(cl, varlist = c("ridge.coefs", "x", "y"))

the.models <- parLapply(cl, gamma, fit.Adaptive.LASSO)

stopCluster(cl)


# dir("./05-Model-Fitting/01-Logistic-Regression")
save(the.models, file = "./05-Model-Fitting/01-Logistic-Regression/alasso_fits.rds")










# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### use model fits for all lambdas to predict on training data (x)
# fit2.preds <- predict(fit2, x, type = "class")
# fit2.probs <- predict(fit2, x, type = "response")
# 
# 
# 
# #HERE is where we can use different fit criteria to select hyperparameter
# #NEED to predict on the VALIDATION dataset too
# 
# 
# 
# ### estimate ROC curves for each fit with different lambda
# library(pROC)
# fit2.rocs <- apply(fit2.probs, MARGIN = 2, function(x){roc(y, x, levels = c("Did Not Default", "Defaulted"), direction = "<")})
# #### extract auc from each
# fit2.rocs.auc <- sapply(fit2.rocs, function(x){pluck(x, "auc")})
# 
# #### find ROC with highest auc
# fit2.rocs[[names(fit2.rocs.auc[which.max(fit2.rocs.auc)])]]
# 
# ### what is index for best lambda used
# best.fit2 <- which(names(fit2.rocs)==names(fit2.rocs.auc[which.max(fit2.rocs.auc)]))
# 
# ### actual lambda value
# lambda <- fit2$lambda[best.fit2]
# 
# 
# 
# 
# # Former code that used the repeated cross-validation, we should hard-code to incorporate other validation dataset and other fit criteria
# # fit2 <- cv.glmnet(x=x, y=y, alpha=1, family="binomial", penalty.factor=adaptive.weights)
# # 
# # plot(fit2)
# # #fit2$lambda.min
# # #fit2$lambda.1se
# # coef(fit2, s=fit2$lambda.min)
# # coef(fit2, s=fit2$lambda.1se)
# 
# 
# 
# 




