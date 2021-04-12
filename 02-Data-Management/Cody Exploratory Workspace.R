## CODY'S EXPLORATORY WORKSPACE

train <- read.csv("./01-Data/00-Raw/Simulated_Data_Train.csv")


names(train)


lapply(1:ncol(train), function(x){try(hist(train[,x], main=names(train)[x]))})

# 212 99999 values skewing dist
hist(train$avg_card_debt[-which(train$avg_card_debt==train$avg_card_debt[which.max(train$avg_card_debt)])])
# bunch of zeros
hist(train$credit_past_due_amount[which(train$credit_past_due_amount!=0)])




hist(train$non_mtg_acc_past_due_12_months_num)
hist(train$non_mtg_acc_past_due_6_months_num)
hist(train$mortgages_past_due_6_months_num)


table(train$Default_ind, train$non_mtg_acc_past_due_12_months_num)
prop.table(table(train$Default_ind, train$non_mtg_acc_past_due_12_months_num), margin = 2)


table(train$Default_ind, train$non_mtg_acc_past_due_6_months_num)
prop.table(table(train$Default_ind, train$non_mtg_acc_past_due_6_months_num), margin = 2)


table(train$Default_ind, train$mortgages_past_due_6_months_num)
prop.table(table(train$Default_ind, train$mortgages_past_due_6_months_num), margin = 2)


table(train$non_mtg_acc_past_due_12_months_num, train$non_mtg_acc_past_due_6_months_num)

table(train$non_mtg_acc_past_due_12_months_num, train$mortgages_past_due_6_months_num)



library(dplyr)
library(magrittr)

train %<>% mutate(delinquent = ifelse(non_mtg_acc_past_due_12_months_num>0 | non_mtg_acc_past_due_6_months_num > 0 | mortgages_past_due_6_months_num>0, 1, 0))


table(train$Default_ind, train$delinquent)

tapply(train$tot_credit_debt, list(train$Default_ind, train$delinquent), mean)
tapply(train$tot_credit_debt, list(train$Default_ind, train$delinquent), median)
tapply(train$tot_credit_debt, list(train$Default_ind, train$delinquent), function(x){exp(mean(log(x)))})


tapply(train$avg_card_debt, list(train$Default_ind, train$delinquent), mean)
tapply(train$avg_card_debt, list(train$Default_ind, train$delinquent), median)
tapply(train$avg_card_debt, list(def=train$Default_ind, del=train$delinquent), function(x){exp(mean(log(x)))})






















# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract recoded datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})

the.data <- clean.data[[2]]



the.vars <- names(the.data)[-which(names(the.data)%in%c("Default_ind", "States"))]


the.data.matrix <- sapply(the.data[,the.vars], as.numeric) %>% as.matrix()
the.data.matrix <- the.data.matrix[complete.cases(the.data.matrix),]



fit.Factor.analysis <- function(the.data.matrix, n.factors){
  the.factor.analysis <- factanal(the.data.matrix, factors = n.factors)
  
  the.evaluation.criteria <- cbind(
    "N Factors" = the.factor.analysis$factors, 
    "Chi-square Statistic" = the.factor.analysis$STATISTIC, 
    "Degrees of Freedom" = the.factor.analysis$dof, 
    p = the.factor.analysis$PVAL
  ) %>%
    as.data.frame()
  
  return(the.evaluation.criteria)
}



explore.fa <- lapply(1:4, function(x){
  fit.Factor.analysis(the.data.matrix, x)
}) 



explore.fa.table <- bind_rows(explore.fa)


fit.Factor.analysis(the.data.matrix, 5)




the.cor.mat <- cor(the.data.matrix, use = "pairwise.complete.obs")


factanal(covmat = the.cor.mat, factors = 1)
factanal(covmat = the.cor.mat, factors = 2)
factanal(covmat = the.cor.mat, factors = 3)
factanal(covmat = the.cor.mat, factors = 4)
factanal(covmat = the.cor.mat, factors = 5)





train.data <- clean.data[[2]]
validation.data <- clean.data[[3]]



dir("./01-Data/01-Clean")
train.data.impute <- rfImpute(x = train.data[,-which(names(train.data)=="Default_ind")],
                              y = train.data[,"Default_ind"])

save(train.data.impute, file = "./01-Data/01-Clean/train_impute.rds")

set.seed(74)
rf.fit <- randomForest(x = train.data.impute[,-1], 
                       y = train.data.impute[,1], 
                       
                       # xtest = validation.data[,-which(names(validation.data)=="Default_ind")], 
                       # ytest = validation.data[,"Default_ind"], 
                       
                       ntree = 10, 
                       mtry = 4,
                       
                       replace=TRUE, 
                       classwt=c(10,1), 
                       # cutoff, 
                       # strata = train.data.impute[,1],
                       # sampsize = rep(sum(train.data$Default_ind=="Defaulted"), 2),
                       # nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                       # maxnodes = NULL,
                       importance=FALSE, 
                       localImp=TRUE, 
                       # nPerm=1,
                       proximity = TRUE, 
                       # oob.prox=proximity,
                       # norm.votes=TRUE, 
                       do.trace=TRUE,
                       # keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
                       # keep.inbag=FALSE, ...
                       # na.action = na.roughfix
                       )



rf.timing <- microbenchmark::microbenchmark()

library(randomForest)
rf.fit <- randomForest(data = the.data, ntree = 50, mtry = floor(sqrt(ncol(the.data)-1)), replace = TRUE, importance = TRUE)

rf.fit

varImpPlot(rf.fit)



preds <- predict(rf.fit)

the.data.preds <- the.data

the.data.preds$prediction[which(complete.cases(the.data.preds))] <- predict(rf.fit, type = "response")

the.data.preds$prediction.probs.def <- NA
the.data.preds$prediction.probs.dnd <- NA

the.data.preds[which(complete.cases(the.data.preds)), c("prediction.probs.def", "prediction.probs.dnd")] <- predict(rf.fit, type = "prob")


pROC::roc(as.numeric(the.data.preds$Default_ind=="Defaulted"), the.data.preds$prediction)


pROC::plot.roc(as.numeric(the.data.preds$Default_ind=="Defaulted"), the.data.preds$prediction)


predict(rf.fit, type = "prob")


names(preds[1:10])





library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl) 


res <- microbenchmark::microbenchmark({
  
  
  foreach(ntree=rep(1,100), .combine = randomForest::combine, 
          .multicombine = TRUE, .packages = c("randomForest", "doParallel")) %dopar% {
            randomForest(x = the.predictors, 
                         y = the.data.complete[,"Default_ind"], 
                         
                         xtest = the.validation.predictors, 
                         ytest = the.validation.data.complete[,"Default_ind"], 
                         
                         ntree = ntree, 
                         mtry = 4,
                         
                         replace=TRUE, 
                         # classwt=c(10,1), 
                         # cutoff, 
                         strata = the.data.complete[,"Default_ind"],
                         sampsize = rep(sum(the.data.complete$Default_ind=="Defaulted"), 2),
                         # nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                         # maxnodes = NULL,
                         importance=TRUE, 
                         localImp=TRUE, 
                         # nPerm=1,
                         proximity = FALSE, 
                         # oob.prox=proximity,
                         # norm.votes=TRUE, 
                         do.trace=TRUE,
                         keep.forest=TRUE, 
                         # corr.bias=FALSE,
                         # keep.inbag=FALSE, ...
                         # na.action = na.roughfix
            )
            
          }
  
}, times = 10, unit = "s")
stopCluster(cl)

print(res)



