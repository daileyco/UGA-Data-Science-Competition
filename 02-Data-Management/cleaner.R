# cleaner.R
##
## This script processes raw data files.
##
### It recodes the data to appropriate data types (i.e., class())
###
### It uses randomForest::rfImpute() to impute missing data (default arguments)
###
### It standardized the continuous data by centering (i.e., subtracting mean) and scaling (i.e., dividing by standard deviation)
####
#### For consistency, the data for Validation and Testing are standardized using the means and standard deviations for the Training data
###
### The data are saved into the ./01-Data/01-Clean directory as an aggregate list
####
#### This is the data that should be used for downstream analyses




# load packages
my.packages <- c("dplyr", "magrittr", "randomForest", "parallel", "doParallel")
lapply(my.packages, library, character.only=T)




# set-up training data for processing all others
train <- read.csv("./01-Data/00-Raw/Simulated_Data_Train.csv")
train %<>% mutate(non_mtg_acc_past_due_12_months_num = as.integer(non_mtg_acc_past_due_12_months_num),
                  non_mtg_acc_past_due_6_months_num = as.integer(non_mtg_acc_past_due_6_months_num), 
                  mortgages_past_due_6_months_num = as.integer(mortgages_past_due_6_months_num), 
                  inq_12_month_num = as.integer(inq_12_month_num), 
                  card_inq_24_month_num = as.integer(card_inq_24_month_num), 
                  card_open_36_month_num = as.integer(card_open_36_month_num), 
                  auto_open_.36_month_num = as.integer(auto_open_.36_month_num), 
                  ind_acc_XYZ = factor(ind_acc_XYZ, levels = c(1,0), labels = c("Account w/ XYZ", "No Account")), 
                  Default_ind = factor(Default_ind, levels = c(1,0), labels = c("Defaulted", "Did Not Default")), 
                  States = factor(States, levels = c("AL", "FL", "GA", "LA", "MS", "NC", "SC")))

## these data vectors are used for each standardization
train.means <- lapply(1:ncol(train), function(.index){if(is.double(train[,.index])){return(mean(train[,.index], na.rm = T))}else{return(NA)}}) %>% unlist()
train.sds <- lapply(1:ncol(train), function(.index){if(is.double(train[,.index])){return(sd(train[,.index], na.rm = T))}else{return(NA)}}) %>% unlist()


### for some reason, this output differs from the "hardcoded" versions below
# train.std <- train %>%
#               mutate_if(is.numeric, scale)











# set-up functions to streamline process for each dataset to ensure consistency
## standardization if the variable is continuous
standardize.Data <- function(the.data, the.data.means, the.data.sds){
  
  the.indices <- which(sapply(the.data, is.double))
  
  the.data.stdz <- the.data
  
  the.data.stdz[,the.indices] <- sapply(the.indices, 
                                         function(.index){
                                           if(is.double(the.data[,.index])){
                                             return(scale(the.data[,.index], center = the.data.means[.index], scale = the.data.sds[.index]))
                                             }else{
                                               return(the.data[,.index])}
                                           }) 
  
  return(the.data.stdz)
}


### comparison to other dataset, this is what is different for some reason
# train.stdz <- standardize.Data(train, train.means, train.sds)
# 



## reformat data types
recode.Data <- function(the.data){
  the.data %<>% mutate(non_mtg_acc_past_due_12_months_num = as.integer(non_mtg_acc_past_due_12_months_num),
                    non_mtg_acc_past_due_6_months_num = as.integer(non_mtg_acc_past_due_6_months_num), 
                    mortgages_past_due_6_months_num = as.integer(mortgages_past_due_6_months_num), 
                    inq_12_month_num = as.integer(inq_12_month_num), 
                    card_inq_24_month_num = as.integer(card_inq_24_month_num), 
                    card_open_36_month_num = as.integer(card_open_36_month_num), 
                    auto_open_.36_month_num = as.integer(auto_open_.36_month_num), 
                    ind_acc_XYZ = factor(ind_acc_XYZ, levels = c(1,0), labels = c("Account w/ XYZ", "No Account")), 
                    Default_ind = factor(Default_ind, levels = c(1,0), labels = c("Defaulted", "Did Not Default")), 
                    States = factor(States, levels = c("AL", "FL", "GA", "LA", "MS", "NC", "SC"))) %>%
                mutate(Default_ind = relevel(Default_ind, ref = "Did Not Default"))
  
  return(the.data)
}


## read in csv file, unnecessary but why not
read.Data <- function(the.file){
  the.data <- read.csv(the.file)
  return(the.data)
}



## wrapper function to show overall management
### note returned data is list containing three elements
clean.Data <- function(the.file){
  the.data <- read.Data(the.file)
  
  the.data.recode <- recode.Data(the.data)
  
  the.data.stdz <- standardize.Data(the.data.recode, train.means, train.sds)
  
  
  cl <- makePSOCKcluster(4)
  registerDoParallel(cl)
  the.data.imputed <- rfImpute(x = the.data.recode[,-which(names(the.data.recode)=="Default_ind")], 
                               y = the.data.recode[, "Default_ind"])
  stopCluster(cl)
  
  names(the.data.imputed)[1] <- "Default_ind"
  the.data.imputed <- the.data.imputed[,c(2:ncol(the.data.imputed), 1)]
  
  the.data.imputed.stdz <- standardize.Data(the.data.imputed, train.means, train.sds)
  
  return(list(file = the.file, clean = the.data.recode, standardized = the.data.stdz, imputed = the.data.imputed, imputed.standardized = the.data.imputed.stdz))
}






# apply functions to all datasets
## get filenames from ./01-Data/00-Raw file directory
the.files <- list.files("./01-Data/00-Raw", pattern = ".csv", full.names = TRUE)

## create list of lists returned by wrapper function
all.data <- lapply(the.files, clean.Data)

## save the output in the ./01-Data/01-Clean directory for use in other scripts
save(all.data, file = "./01-Data/01-Clean/all_data.rds")


# clean environment
rm(list = ls())
