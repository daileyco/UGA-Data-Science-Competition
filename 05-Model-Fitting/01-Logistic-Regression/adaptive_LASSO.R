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
clean.data <- lapply(all.data, function(x){pluck(x, "imputed")})

imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})

the.data <- clean.data[[2]]
the.data <- imputed.data[[2]]



the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]


the.data.complete <- the.data[complete.cases(the.data),]

the.predictors <- the.data.complete[,the.vars]


the.interactions <- t(combn(the.vars, 2))
the.interactions <- apply(the.interactions, MARGIN = 1, paste0, collapse="*")



the.predictors.formula <- as.formula(paste0("~", paste0(the.vars, collapse = "+")))

x <- model.matrix(the.predictors.formula, data = the.predictors)[,-1]

y <- the.data.complete$Default_ind












# Adaptive LASSO
set.seed(74)

library(glmnet) 

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

### can manually provide sequence of lambdas
the.lambdas <- c(fit1$lambda, seq(0.02, 0.005, -0.001))
fit1 <- glmnet(x=x, y=y, family = "binomial", alpha = 0, lambda = NULL)
the.lambdas <- c(fit1$lambda, seq(0.02, 0.005, -0.001))
fit1$lambda
### use model fits for all lambdas to predict on training data (x)
## x can be validation setth

fit1.preds <- predict(fit1, x, type = "class")
fit1.probs <- predict(fit1, x, type = "response")

#HERE is where we can use different fit criteria to select hyperparameter
#NEED to predict on the VALIDATION dataset too


### estimate ROC curves for each fit with different lambda
library(pROC)
fit1.rocs <- apply(fit1.probs, MARGIN = 2, function(x){roc(y, x, levels = c("Did Not Default", "Defaulted"), direction = "<" )})
#### extract auc from each
fit1.rocs.auc <- sapply(fit1.rocs, function(x){pluck(x, "auc")})

#### find ROC with highest auc
fit1.rocs[[names(fit1.rocs.auc[which.max(fit1.rocs.auc)])]]

### what is index for best lambda used
best.fit1 <- which(names(fit1.rocs)==names(fit1.rocs.auc[which.max(fit1.rocs.auc)]))

### actual lambda value
lambda <- fit1a$lambda[best.fit1]




# Former code that used the repeated cross-validation, we should hard-code to incorporate other validation dataset and other fit criteria
fit1a <- cv.glmnet(x=x, y=y, alpha=0, family="binomial")

# Examine tuning
plot(fit1a)
fit1a$lambda.min
fit1a$lambda.1se

lambda <- fit1$lambda.1se








## Calculation of adaptive weights
# Everything  up to here is "rigid" ha
ridge.coefs <- as.matrix(coef(fit1a, s=lambda)[-1])






gamma <-1 #0.5 | 1 | 2

# shitFXN = function (gammas) {
# for every gamma, fit model
# output model, 
# save in list
# }

adaptive.weights <- 1/abs(ridge.coefs)^gamma


fit.Adaptive.LASSO <- function(.gamma){

#### force in variable by setting weight to zero
# adaptive.weights[c(1:20)] <- 0






### can manually provide sequence of lambdas
the.lambdas <- c(fit1$lambda, seq(0.02, 0.005, -0.005))
fit2 <- glmnet(x=x, y=y, family = "binomial", alpha = 1, lambda = NULL, penalty.factor=adaptive.weights, keep = T)

clusterExport(cl, varlist = c("ridge.coefs", "x", "y"))

the.models <- parLapply(cl, gamma, fit.Adaptive.LASSO)

stopCluster(cl)


# dir("./05-Model-Fitting/01-Logistic-Regression")
save(the.models, file = "./05-Model-Fitting/01-Logistic-Regression/alasso_fits.rds")










###### FOR VALIDATION SET 

# Former code that used the repeated cross-validation, we should hard-code to incorporate other validation dataset and other fit criteria
fit2a <- cv.glmnet(x=x, y=y, alpha=1, type.measure = "deviance", family="binomial", penalty.factor=adaptive.weights, keep = T)
# 
plot(fit2a)
fit2a$lambda.min
fit2a$lambda.1se
coef(fit2a, s=fit2a$lambda.min)
coef(fit2a, s=fit2a$lambda.1se)



fit2a.preds <- predict(fit2a, x, type = "class")
fit2a.probs <- predict(fit2a, x, type = "response")



### estimate ROC curves for each fit with different lambda
library(pROC)
fit2a.rocs <- apply(fit2a.probs, MARGIN = 2, function(x){roc(y, x, levels = c("Did Not Default", "Defaulted"), direction = "<")})
#### extract auc from each
fit2a.rocs.auc <- sapply(fit2a.rocs, function(x){pluck(x, "auc")})

#### find ROC with highest auc
fit2a.rocs[[names(fit2a.rocs.auc[which.max(fit2a.rocs.auc)])]]

### what is index for best lambda used
best.fit2a <- which(names(fit2a.rocs)==names(fit2a.rocs.auc[which.max(fit2a.rocs.auc)]))

### actual lambda value
lambda <- fit2a$lambda[best.fit2a]




standForLogistic = the.data.complete
standForLogistic
for (var in c(1:5,9,14:17,19)){
  standForLogistic[, var] = scale(standForLogistic[,var])
}
logisticFit = glm(Default_ind ~ . ,data = standForLogistic, family = "binomial" )
tab_model(logisticFit, df.method = "wald") # df.method = "wald" for speed no other 
                                            # reason since we don't care about CI's

tab_model(coef(fit2a))

lassoCoefs = matrix((coef(fit2a)))
logisticCoefs = (matrix((coef(logisticFit))))

pct_change = abs((logisticCoefs - lassoCoefs) / logisticCoefs)*100
cbind(logisticCoefs, lassoCoefs, pct_change)  
  
