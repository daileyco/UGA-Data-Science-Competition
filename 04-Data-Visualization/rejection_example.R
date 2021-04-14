










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


the.data.complete <- the.data[complete.cases(the.data),]


the.vars <- names(the.data.complete[,-which(names(the.data.complete)=="Default_ind")]






# identify model files

rf.files <- list.files("./05-Model-Fitting/02-Machine-Learning/01-Models", pattern = "rf_fit.*[.]rds", full.names = TRUE)
rf.files.info <- file.info(rf.files)
rf.files <- rf.files[order(rf.files.info$ctime)]


.index <- 1

the.file <- rf.files[.index]
load(the.file)


the.object <- gsub(".*/(rf_fit[0-9]+)[.]rds", "\\1", the.file)


the.probs <- predict(get(the.object), the.data.complete[,the.vars], type = "prob") %>% as.data.frame()

the.threshold <- 0.5

these.predictions <- factor(ifelse(the.probs$Defaulted>the.threshold, "Defaulted", "Did Not Default"), levels = c("Defaulted", "Did Not Default")) %>% 
    relevel(., ref = "Did Not Default")

the.truth <- the.data.complete$Default_ind

the.observations <- the.data.complete %>% bind_cols(., pred = these.predictions) %>% mutate(TP = Default_ind=="Defaulted" & pred == "Defaulted", FP = Default_ind=="Did Not Default" & pred == "Defaulted", TN = Default_ind=="Did Not Default" & pred == "Did Not Default", FN = Default_ind=="Defaulted" & pred == "Did Not Default")

the.tn.means <- the.observations %>% filter(TN) %>% summarise_all(mean)

the.positives <- the.observations %>% filter(pred=="Defaulted")



set.seed(74)
the.example.index <- sample(1:nrow(the.positives), 1)
the.example <- bind_rows(the.positives[the.example.index,] %>% mutate(ex=1), the.tn.means %>% mutate(ex=0)) %>% t() %>% as.data.frame()


names(the.example) <- c("TN.mean", "Ex.obs")
the.example$variable <- the.vars


for(i in 1:length(the.vars)){
the.example[,paste0("replace.", the.vars[i])] <- ifelse(the.example$variable == the.vars[i], the.example$TN.mean, the.example$Ex.obs)
}

library(tidyr)
the.example %<>% pivot_wider(names_from = variable, values_from = everything())


the.example$prob <- predict(get(the.object), the.example, type = "prob")


dotchart(rep(the.example$prob[1], nrow(the.example)-1))

which(grepl(the.vars[i], names(the.example))








































