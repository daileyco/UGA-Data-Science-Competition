# Evaluate Random Forests



# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable", "randomForest", "pROC", "doParallel", "doRNG")
lapply(my.packages, library, character.only=T)


# load data
# load("./01-Data/01-Clean/all_data.rds")
load("all_data.rds")


## extract datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})
imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})
the.data <- imputed.data[[2]]
the.validation.data <- imputed.data[[3]]


the.data.complete <- the.data[complete.cases(the.data),]
# the.data.complete <- the.validation.data[complete.cases(the.validation.data),]


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


# identify model files

# rf.files <- list.files("./05-Model-Fitting/02-Machine-Learning/01-Models", pattern = "rf_fit.*[.]rds", full.names = TRUE)
rf.files <- list.files("01-Models", pattern = "rf_fit.*[.]rds", full.names = TRUE)
rf.files.info <- file.info(rf.files)
rf.files <- rf.files[order(rf.files.info$ctime)]


evaluate.Random.Forests <- function(.index){
  
  the.file <- rf.files[.index]
  load(the.file)
  
  the.object <- gsub(".*/(rf_fit[0-9]+)[.]rds", "\\1", the.file)
  the.params <- rf.tuning.grid[.index,]
  # the.probs <- predict(get(the.object), type = "prob") %>% as.data.frame()
  the.probs <- predict(get(the.object), the.data.complete[,-which(names(the.data.complete)=="Default_ind")], type = "prob") %>% as.data.frame()
  
  the.truth <- the.data.complete$Default_ind
  the.roc <- roc(the.truth, the.probs$Defaulted, levels = c("Did Not Default", "Defaulted"), direction = "<")
  
  the.params$auc <- the.roc$auc
  the.metrics <- data.frame(threshold = the.roc$thresholds, sensitivity = the.roc$sensitivities, specificity = the.roc$specificities) %>%
    mutate(youden = sensitivity + specificity - 1, 
           d2c = sqrt((1-sensitivity)^2+(1-specificity)^2))
  
  cl <- makePSOCKcluster(15)
  registerDoParallel(cl) 
  
  clusterExport(cl, varlist = c("the.probs", "the.truth"), envir = environment())
  
  the.predictions <- parLapply(cl, the.metrics$threshold, function(.threshold){
    require(dplyr)
    these.predictions <- factor(ifelse(the.probs$Defaulted>.threshold, "Defaulted", "Did Not Default"), levels = c("Defaulted", "Did Not Default")) %>% 
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
    mutate(Model = the.object) %>%
    select(Model, everything())
  
  return(the.best.thresholds)
}






total <- length(rf.files)
pb <- txtProgressBar(min = 0, max = total, style = 3)


model.evaluations <- lapply(1:length(rf.files), function(.index){
  setTxtProgressBar(pb, .index);
  evaluate.Random.Forests(.index)
})


# save(model.evaluations, file = "./05-Model-Fitting/02-Machine-Learning/model_metrics_validation.rds")
save(model.evaluations, file = "model_metrics_training2.rds")


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



