# Evaluate Random Forests










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





paste0("./05-Model-Fitting/02-Machine-Learning/01-Models/rf_fit", .index, ".rds")






























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





































































