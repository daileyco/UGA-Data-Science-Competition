## YANG'S EXPLORATORY WORKSPACE

library(tidyverse)
library(brms)
library(caret)
library(here)
library(skimr)
library(DataExplorer)

# data
tran_dat <- read.csv(file = here("01-Data/00-Raw/Simulated_Data_Train.csv"))
tran_dat <- tran_dat %>% 
  mutate(ind_acc_XYZ = factor(ind_acc_XYZ),States = factor(States))

skim(tran_dat)
summary(tran_dat)

# mod1 <- brm(Default_ind ~ rep_income, family = bernoulli,data = tran_dat, cores = 4, chains = 4)
# summary(mod1)
# plot(mod1)
# pp_check(mod1)

DataExplorer::create_report(data = tran_dat)

library(pROC)
model_glm = glm(Default_ind ~ rep_income, data = tran_dat, family = "binomial")
test_prob = predict(model_glm, newdata = tran_dat, type = "response")
test_roc = roc(tran_dat$Default_ind ~ test_prob, plot = TRUE, print.auc = TRUE)


