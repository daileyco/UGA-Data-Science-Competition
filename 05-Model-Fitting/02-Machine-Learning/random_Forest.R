# Random Forest















# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable", "randomForest", "pROC", "doParallel", "doRNG")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})
imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})
the.data <- imputed.data[[2]]
the.validation.data <- imputed.data[[3]]


# set up for model fit, separate predictors
the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]


the.data.complete <- the.data[complete.cases(the.data),]
the.validation.data.complete <- the.validation.data[complete.cases(the.validation.data),]


the.predictors <- the.data.complete[,the.vars]
the.validation.predictors <- the.validation.data.complete[,the.vars]





# set up tuning grid
## stratification based on response class
### if TRUE, what is class ratio for resampling (i.e., how many "Did Not Defaults" for every "Defaulted", now 1:1 or 2:1 or 3:1)
### right now the minimum sample size is the "Defaulted" frequency
#### cannot oversample using this built in argument
## number of trees for the random forest
### set to odd numbers, divisible by 4 (n-1) for parallel processing
## mtrys from 2 to 18, does not include entire 20, 18 likely too high too

rf.tuning.grid <- bind_rows(
  expand.grid(.stratified = TRUE, 
              .sampsize.ratio = 1:3, 
              .ntree = seq(401, 2001, by = 400), 
              .mtrys = 2:18), 
  expand.grid(.stratified = FALSE, 
              .sampsize.ratio = NA, 
              .ntree = seq(401, 2001, by = 400), 
              .mtrys = 2:18)
)



# set up function to iterate over tuning grid
## parallel computing within this function
## set up for 4 cores to fit equally sized random forests, then combine (probably a better way to optimize this)
fit.RF <- function(.stratified, .sampsize.ratio, .ntree, .mtrys){
  
  
  if(.stratified){
    .strata = the.data.complete[,"Default_ind"]
    .sampsize = c(sum(the.data.complete$Default_ind=="Defaulted")*.sampsize.ratio, sum(the.data.complete$Default_ind=="Defaulted"))
  }else{
    .strata = NULL
    .sampsize = nrow(the.data.complete)
  }
  
  .mtry = .mtrys
  
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl) 

  rf.fit <-   foreach(ntree=c(rep(((.ntree-1)/4),3), ((.ntree-1)/4)+1), 
                      .combine = randomForest::combine, 
                      .multicombine = TRUE, 
                      .packages = "randomForest", 
                      .export = c("the.predictors", "the.data.complete")) %dorng% {
                  randomForest(x = the.predictors, 
                               y = the.data.complete[,"Default_ind"], 
                               
                               # xtest = the.validation.predictors, 
                               # ytest = the.validation.data.complete[,"Default_ind"], 
                               
                               ntree = ntree, 
                               mtry = .mtry,
                               
                               replace=TRUE, 
                               # classwt=c(10,1), 
                               # cutoff, 
                               strata = .strata,
                               sampsize = .sampsize,
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
    
  stopCluster(cl)
  
  
  return(rf.fit)
}



## debugging tuning grid
#
# rf.tuning.grid <- data.frame(.stratified = c(TRUE, FALSE),
#                              .sampsize.ratio = c(2, NA),
#                              .ntree = c(41, 41),
#                              .mtrys = c(2, 4))


# hopefully, this establishes reproducibility
## the %dorng% ensures parallel computation of random forests will be statistically independent, 
## also, stores RNGseed as attribute within object
### use attr(obj, 'rng')

set.seed(2021)

start = Sys.time()
lapply(1:nrow(rf.tuning.grid), function(.index){
  assign(
    paste0("rf_fit", .index), 
    fit.RF(rf.tuning.grid$.stratified[.index], rf.tuning.grid$.sampsize.ratio[.index], rf.tuning.grid$.ntree[.index], rf.tuning.grid$.mtry[.index])
    );
  save(
    list=paste0("rf_fit", .index), 
    file = paste0("./05-Model-Fitting/02-Machine-Learning/01-Models/rf_fit", .index, ".rds")
    )
})
end = Sys.time()






# rf.fit
# 
# varImpPlot(rf.fit)
# 
# 
# 
# preds <- predict(rf.fit)
# 
# the.data.preds <- the.data.complete
# 
# the.data.preds$prediction <- predict(rf.fit, type = "response")
# 
# the.data.preds$prediction.probs.def <- NA
# the.data.preds$prediction.probs.dnd <- NA
# 
# the.data.preds[, c("prediction.probs.dnd", "prediction.probs.def")] <- predict(rf.fit, type = "prob")
# 
# roc(the.data.preds$Default_ind, the.data.preds$prediction.probs.def, levels = c("Did Not Default", "Defaulted"), direction = "<", plot = TRUE)




































