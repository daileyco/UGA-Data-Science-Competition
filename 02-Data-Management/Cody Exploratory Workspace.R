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


