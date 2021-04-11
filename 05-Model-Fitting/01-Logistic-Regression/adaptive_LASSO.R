





vars.considered <- vars.considered[!vars.considered%in%c("S1.moustique_derange_nuit", "Count.enf", "Count.lt5", "Count.5a15")]

data.complete <- data[complete.cases(data[,c("Count.GE", "Count.enf", vars.considered)]),]

predictors <- data.complete[, c("Quartier", "trt", "trt.simple", vars.considered)]

as.formula(paste("GE~age.cat+Quartier+distance_to_cours_eau+any_animal", paste(vars.considered, collapse = " + "), sep = " + "))
as.formula(paste("~age.cat+Quartier+trt+trt.simple", paste(vars.considered, collapse = " + "), sep = " + "))

x <- model.matrix(as.formula(paste("~Quartier+trt+trt.simple", paste(vars.considered, collapse = " + "), sep = " + ")), data=predictors)[,-1]





# Adaptive LASSO
set.seed(74)

y <- as.matrix(cbind(data.complete$Count.GE, data.complete$Count.enf))

library(glmnet)

## Ridge regression
fit1 <- cv.glmnet(x=x, y=y, alpha=0, family="binomial")

###Examine tuning
###plot(fit1)
###fit1$lambda.min
###fit1$lambda.1se

lambda <- fit1$lambda.min
## Calculation of adaptive weights
# ridge.coefs <- as.matrix(cbind(coef(fit1, s=lambda)[[1]], coef(fit1, s=lambda)[[2]], coef(fit1, s=lambda)[[3]], coef(fit1, s=lambda)[[4]]))
ridge.coefs <- as.matrix(coef(fit1, s=lambda)[-1])
gamma <- 0.5 #0.5 | 1 | 2
adaptive.weights <- 1/abs(ridge.coefs)^gamma
### Correction for extreme weights
table(is.infinite(adaptive.weights))
adaptive.weights[is.infinite(adaptive.weights)] <- max(adaptive.weights[adaptive.weights!=Inf]) # 999999
adaptive.weights[c(1:3, 4:7)] <- 0
adaptive.weights[c(1:3, 8:9)] <- 0

## Adaptive LASSO model fit
fit2 <- cv.glmnet(x=x, y=y, alpha=1, family="binomial", penalty.factor=adaptive.weights)

plot(fit2)
#fit2$lambda.min
#fit2$lambda.1se
coef(fit2, s=fit2$lambda.min)
coef(fit2, s=fit2$lambda.1se)








