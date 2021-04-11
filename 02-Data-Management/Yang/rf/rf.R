train_dat <- read.csv("01-Data/00-Raw/Simulated_Data_Train.csv")

library(caret)
library(randomForest)
library(tidyverse)
library(doParallel)
set.seed(12345)

summary(train_dat)
train_dat2 <- train_dat %>% drop_na() %>% mutate(Default_ind = factor(Default_ind))
nzv <- nearZeroVar(train_dat, saveMetrics= TRUE)
summary(train_dat2)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

cl <- makePSOCKcluster(10)
registerDoParallel(cl)
m_rf <- train(Default_ind ~ ., data = train_dat2, method = "rf", metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
stopCluster(cl)

# saveRDS(m_rf,"m_rf.rds")

m_rf
(16559-1301)/16559