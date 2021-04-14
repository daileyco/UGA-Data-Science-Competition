







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
  
  
  # cost <- TP * geometric.mean.avg.card.debt["Defaulted"] - FP * (geometric.mean.avg.card.debt["Did Not Default"]*(profit.merchant+profit.interest*quadratic.mean.n.delinq["Did Not Default"])) + TN * (geometric.mean.avg.card.debt["Did Not Default"]*(profit.merchant+profit.interest*quadratic.mean.n.delinq["Did Not Default"])) - FN * geometric.mean.avg.card.debt["Defaulted"]
  cost <- TN * (geometric.mean.avg.card.debt["Did Not Default"]*(profit.merchant+profit.interest*quadratic.mean.n.delinq["Did Not Default"])) - FN * geometric.mean.avg.card.debt["Defaulted"]
  
  return(cost)
  
}


# works as designed
## calculate.Cost(TP = 0, FP = 0, TN = 0, FN = 1)




specificity <- seq(0,1, by = 0.01)-0.5

sensitivity <- sqrt(0.25 - specificity^2)
sensitivity <- c(sensitivity, -1*sensitivity)
specificity <- rep(specificity, 2)

sensitivity <- sensitivity[which(sensitivity>(1-specificity))]
specificity <- specificity[which(sensitivity>(1-specificity))]

specificity <- specificity[order(specificity, descending = T)]
sensitivity <- sensitivity[order(sensitivity, descending = T)]

plot(sensitivity, 1-specificity, type = "o")


library(ROC)

trapzint(1-specificity, sensitivity, 0, 1)


d2c <- sqrt((1-sensitivity)^2+(1-specificity)^2)

n.def <- 100
n.dnd <- 100

n.tp <- floor(sensitivity * n.def)
n.fn <- n.def - n.tp

n.tn <- floor(specificity * n.dnd)
n.fp <- n.dnd - n.tn


the.cost <- sapply(1:length(n.tp), function(.index){calculate.Cost(TP=n.tp[.index], FP=n.fp[.index], TN=n.tn[.index], FN=n.fn[.index])})


plot(d2c, the.cost, type = "o")
plot(sensitivity, the.cost, type = "o")
plot(specificity, the.cost, type = "o")




















