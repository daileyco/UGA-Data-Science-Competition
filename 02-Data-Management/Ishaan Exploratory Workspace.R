## ISHAAN'S EXPLORATORY WORKSPACE

setwd(paste0(getwd(), "/01-Data/00-Raw/"))

train = read.csv("Simulated_Data_Train.csv")
test = read.csv("Simulated_Data_Train.csv")
valid = read.csv("Simulated_Data_Validation.csv")


for(i in 1:(ncol(train)-1)){
  if (length(unique(train[,i])) < 6 ) {
    train[,i] = as.character(train[,i])
  }
}


train$out = as.factor(as.character(train$Default_ind))

for(i in 1:(ncol(test)-1)){
  if (length(unique(test[,i])) < 6 ) {
    test[,i] = as.character(test[,i])
  }
}


test$out = as.factor(as.character(test$Default_ind))


for(i in 1:(ncol(valid)-1)){
  if (length(unique(valid[,i])) < 6 ) {
    valid[,i] = as.character(valid[,i])
  }
}

valid$out = as.factor(as.character(valid$Default_ind))


library(dlookr)
library(tidyverse)


  eda_report(train, target = out, output_dir = "./../../03-Exploratory-Analyses/", output_format = "pdf", 
             output_file = "WF_train_EDA_ID.pdf")



  eda_report(test, target = out, output_dir = "./../../03-Exploratory-Analyses/", output_format = "pdf", 
             output_file = "WF_test_EDA_ID.pdf")


  
  

  eda_report(valid, target = out, output_dir = "./../../03-Exploratory-Analyses/", output_format = "pdf", 
             output_file = "WF_valid_EDA_ID.pdf")

