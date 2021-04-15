







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




specificity <- seq(-1,1, by = 0.001)

# sensitivity <- sqrt(0.25 - specificity^2)

a <- 0.5
b <- 0.25

h <- 0
k <- 0


sensitivity <- c(sqrt((b^2)*(1-(((specificity-h)^2)/(a^2))))+k,
                 -1*sqrt((b^2)*(1-(((specificity-h)^2)/(a^2))))+k)

# sensitivity <- c(sensitivity, -1*sensitivity)
specificity <- rep(specificity, 2)

plot(1-specificity, sensitivity, type = "o", asp = 1)

xc <- 0.5 # center x_c or h
yc <- 0.5 # y_c or k
a <- 0.5 # major axis length
b <- 0.5 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.001) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*sin(phi) + b*sin(t)*cos(phi)
plot(x,y,pch=19, col='blue', asp = 1, xaxs = 'i', yaxs='r')

summary(x)
summary(y)

sensitivity <- c(0,y[which(y>1-x)],1)
specificity <- c(1,x[which(y>1-x)],0)




which(x==max(x))


plot(1-specificity, sensitivity, asp = 1)



sensitivity <- sensitivity[which(sensitivity>(1-specificity))]
specificity <- specificity[which(sensitivity>(1-specificity))]



specificity <- specificity[order(specificity, descending = T)]
sensitivity <- sensitivity[order(sensitivity, descending = T)]



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




















