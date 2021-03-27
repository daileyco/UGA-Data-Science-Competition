## ISHAAN'S EXPLORATORY WORKSPACE

setwd(paste0(getwd(), "/01-Data/00-Raw/"))

train = read.csv("Simulated_Data_Train.csv")
test = read.csv("Simulated_Data_Train.csv")
valid = read.csv("Simulated_Data_Validation.csv")


for(i in 1:ncol(train)){
  if (length(unique(train[,i])) < 6 ) {
    train[,i] = as.character(train[,i])
  }
}

for(i in 1:ncol(test)){
  if (length(unique(test[,i])) < 6 ) {
    test[,i] = as.character(test[,i])
  }
}


for(i in 1:ncol(valid)){
  if (length(unique(valid[,i])) < 6 ) {
    valid[,i] = as.character(valid[,i])
  }
}
library(dlookr)
library(tidyverse)

train %>%
  eda_report(target = Default_ind, output_format = "pdf", output_file = "WF_train_EDA.pdf")


test %>%
  eda_report(target = Default_ind, output_format = "pdf", output_file = "WF_test_EDA.pdf")


valid %>%
  eda_report(target = Default_ind, output_format = "pdf", output_file = "WF_valid_EDA.pdf")

