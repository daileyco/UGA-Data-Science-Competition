







# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable", "glmnet", "doParallel", "parallel", "pROC", "randomForest", "tibble")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract recoded datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})

imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})


the.data <- imputed.data[[1]]
vd <- imputed.data[[3]]



the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]


the.data.complete <- the.data[complete.cases(the.data),]


the.predictors <- the.data.complete[,the.vars]


the.interactions <- t(combn(the.vars, 2))
the.interactions <- apply(the.interactions, MARGIN = 1, paste0, collapse="*")



# the.predictors.formula <- as.formula(paste0("~", paste0(the.interactions, collapse = "+")))
the.predictors.formula <- as.formula(paste0("~", paste0(the.vars, collapse = "+")))

x <- model.matrix(the.predictors.formula, data = the.predictors)[,-1]

y <- the.data.complete$Default_ind


the.truth <- the.data.complete$Default_ind










# formulating the cost function 
## use population averages, could use individuals, but would be very involved

## averages of the average monthly card debt
## harmonic.mean.avg.card.debt <- tapply(the.data.complete$avg_card_debt, the.data.complete$Default_ind, function(x){length(x) / sum(1/x)})

geometric.mean.avg.card.debt <- tapply(vd$avg_card_debt, vd$Default_ind, function(x){exp(mean(log(x)))})

## average of the number of non mortgage deliquencies in past 12 months , divided by 12 to get monthly
quadratic.mean.n.delinq <- tapply(vd$non_mtg_acc_past_due_12_months_num, vd$Default_ind, function(x){sqrt(mean(x^2))/12})

## proportion of avg monthly debt for bank profit 
### conservative? small to account for avg debt being spread across all cards (unknown number, best we have is # cards opened in last 36 months, many zeros)
profit.merchant <- 1/100
profit.interest <- 1/10


## actual function
calculate.Cost <- function(TP, FP, TN, FN){
  
  
  cost <-  TN * (geometric.mean.avg.card.debt["Did Not Default"]*(profit.merchant+profit.interest*quadratic.mean.n.delinq["Did Not Default"])) - FN * geometric.mean.avg.card.debt["Defaulted"]
  
  
  return(cost)
  
}


# works as designed
## calculate.Cost(TP = 0, FP = 0, TN = 0, FN = 1)

































# adaptive LASSO

load("./05-Model-Fitting/01-Logistic-Regression/alasso_fits.rds")


load("./05-Model-Fitting/01-Logistic-Regression/the-chosen-one.rds")

lasso.chosen <- the.chosen.one

## 73, 
# > the.chosen.one$lambda
# [1] 0.000673994
# gamma == 0.25

the.alasso <- the.models[[1]]



lasso.probs <- predict(the.alasso, x, type = "response")[,which(the.alasso$lambda==lasso.chosen$lambda)]



lasso.predictions <- factor(ifelse(lasso.probs>lasso.chosen$threshold, "Defaulted", "Did Not Default"), levels = c("Defaulted", "Did Not Default")) %>% 
  relevel(., ref = "Did Not Default")



lasso.cm <- data.frame(truth = the.truth, prediction = lasso.predictions)


lasso.df <- lasso.cm %>% 
  summarise(
    model = "LASSO", 
    TP = sum(truth == "Defaulted" & prediction == "Defaulted"), 
    FP = sum(truth == "Did Not Default" & prediction == "Defaulted"), 
    TN = sum(truth == "Did Not Default" & prediction == "Did Not Default"), 
    FN = sum(truth == "Defaulted" & prediction == "Did Not Default"), 
    n = n()
  )














# random forest


load("./05-Model-Fitting/02-Machine-Learning/01-Models/rf_fit23.rds")

load("./05-Model-Fitting/02-Machine-Learning/the-chosen-one.rds")


rf.chosen <- the.chosen.one

rm(the.chosen.one)


rf.probs <- predict(rf_fit23, the.predictors, type = "prob") %>% as.data.frame()


rf.predictions <- factor(ifelse(rf.probs$Defaulted>rf.chosen$threshold, "Defaulted", "Did Not Default"), levels = c("Defaulted", "Did Not Default")) %>% 
  relevel(., ref = "Did Not Default")


rf.cm <- data.frame(truth = the.truth, prediction = rf.predictions)

rf.df <- rf.cm %>%
  summarise(
    model = "RF", 
    TP = sum(truth == "Defaulted" & prediction == "Defaulted"), 
    FP = sum(truth == "Did Not Default" & prediction == "Defaulted"), 
    TN = sum(truth == "Did Not Default" & prediction == "Did Not Default"), 
    FN = sum(truth == "Defaulted" & prediction == "Did Not Default"), 
    n = n()
  )







comparison.df <- bind_rows(lasso.df, rf.df) %>%
                  mutate(
                    Accuracy = (TP+TN) / n *100,
                    Error = (FP+FN) / n *100,
                    Profit = calculate.Cost(TP, FP, TN, FN),
                    Sensitivity = TP / (TP+FN), #TPR
                    Specificity = TN / (TN+FP), #TNR
                    PPV = TP / (TP + FP),
                    NPV = TN / (TN + FN)
                  ) %>%
                  mutate(
                    Profit.mp = Profit / n,
                    Youden = Sensitivity + Specificity - 1,
                    d2c = sqrt((1-Sensitivity)^2+(1-Specificity)^2),
                    LR.pos = Sensitivity / (1-Specificity),
                    LR.neg = (1-Sensitivity) / Specificity,
                  ) %>%
                  mutate(
                    Profit = paste0("$", round(Profit, 2)),
                    Profit.mp = paste0("$", round(Profit.mp, 2)),
                    DOR = LR.pos / LR.neg
                  ) %>% 
                  select(
                    model, Accuracy, Error, Profit, Profit.mp, 
                    PPV, NPV,
                    Sensitivity, Specificity, Youden, d2c, 
                    LR.pos, LR.neg, DOR
                  ) %>% 
                  mutate_at(
                    c(2,3), function(x){paste0(round(x, 2), "%")}
                  ) %>%
                  mutate_at(
                    c(6:13), round, digits = 3
                  ) %>% 
                  mutate_at(
                    14, round, digits = 2
                  )



l.cm <- addmargins(table(lasso.cm$prediction, lasso.cm$truth, dnn = c("Prediction", "Truth"))[2:1, 2:1])
l.cm <- rbind(l.cm, NA) %>% as.data.frame()
l.cm <- l.cm[-4,]

l.cm <- rownames_to_column(l.cm)
l.cm <- add_column(l.cm, .before = 1, z = "NA")

l.cm$rowname[3] <- ""



l.cm.ft <- l.cm %>% flextable() %>% 
  compose(part = "header", j=c(1,2,5), value = as_paragraph("")) %>% 
  compose(part = "body", j=2, i=3, value = as_paragraph("")) %>% 
  compose(part = "body", j=1, value = as_paragraph("")) %>%
  compose(part = "body", j=1, i=1:2, value = as_paragraph("Prediction")) %>%
  add_header_row(values = c("", "", "Truth", "Truth", "")) %>% 
  border_remove() %>%
  merge_v(j = 1) %>% 
  merge_h(part = "header", i = 1) %>%
  
  align(part = "header", align = "center") %>% 
  valign(part = "header", valign = "center") %>%
  align(j = 1:2, align = "left") %>% 
  valign(j = 1:2, valign = "center") %>% 
  bold(part = "header", i = 2, bold = FALSE) %>%
  bold(part = "header", i = 1, bold = TRUE) %>%
  bold(j = 1) %>%
  italic(part = "header", i = 2) %>%
  italic(j = 2) %>% 
  font(part = "all", fontname = "Arial") %>%
  fontsize(part = "header", i = 1, size = 14) %>%
  fontsize(part = "header", i = 2, size = 12) %>% 
  fontsize(part = "body", size = 12) %>%
  fontsize(part = "body", j = 1, size = 14) %>%
  border(part = "header", j = 3:4, i = 2, border.bottom = fp_border_default(color = "black", width = 2)) %>% 
  border(part = "body", j = 2, i = 1:2, border.right = fp_border_default(color = "black", width = 2)) %>% 
  border(part = "body", j = 3:4, i = 1:2, border.right = fp_border_default(color = "black", width = 0.5)) %>%
  border(part = "body", j = 3:4, i = 1:2, border.bottom = fp_border_default(color = "black", width = 0.5)) %>% 
  border(part = "body", j = 5, i = 3, border.top = fp_border_default(color = "black", width = 0.25), border.left = fp_border_default(color = "black", width = 0.25)) %>%
  autofit()










r.cm <- addmargins(table(rf.cm$prediction, rf.cm$truth, dnn = c("Prediction", "Truth"))[2:1, 2:1])
r.cm <- rbind(r.cm, NA) %>% as.data.frame()
r.cm <- r.cm[-4,]

r.cm <- rownames_to_column(r.cm)
r.cm <- add_column(r.cm, .before = 1, z = "NA")

r.cm$rowname[3] <- ""



r.cm.ft <- r.cm %>% flextable() %>% 
  compose(part = "header", j=c(1,2,5), value = as_paragraph("")) %>% 
  compose(part = "body", j=2, i=3, value = as_paragraph("")) %>% 
  compose(part = "body", j=1, value = as_paragraph("")) %>%
  compose(part = "body", j=1, i=1:2, value = as_paragraph("Prediction")) %>%
  add_header_row(values = c("", "", "Truth", "Truth", "")) %>% 
  border_remove() %>%
  merge_v(j = 1) %>% 
  merge_h(part = "header", i = 1) %>%
  
  align(part = "header", align = "center") %>% 
  valign(part = "header", valign = "center") %>%
  align(j = 1:2, align = "left") %>% 
  valign(j = 1:2, valign = "center") %>% 
  bold(part = "header", i = 2, bold = FALSE) %>%
  bold(part = "header", i = 1, bold = TRUE) %>%
  bold(j = 1) %>%
  italic(part = "header", i = 2) %>%
  italic(j = 2) %>% 
  font(part = "all", fontname = "Arial") %>%
  fontsize(part = "header", i = 1, size = 14) %>%
  fontsize(part = "header", i = 2, size = 12) %>% 
  fontsize(part = "body", size = 12) %>%
  fontsize(part = "body", j = 1, size = 14) %>%
  border(part = "header", j = 3:4, i = 2, border.bottom = fp_border_default(color = "black", width = 2)) %>% 
  border(part = "body", j = 2, i = 1:2, border.right = fp_border_default(color = "black", width = 2)) %>% 
  border(part = "body", j = 3:4, i = 1:2, border.right = fp_border_default(color = "black", width = 0.5)) %>%
  border(part = "body", j = 3:4, i = 1:2, border.bottom = fp_border_default(color = "black", width = 0.5)) %>% 
  border(part = "body", j = 5, i = 3, border.top = fp_border_default(color = "black", width = 0.25), border.left = fp_border_default(color = "black", width = 0.25)) %>%
  autofit()






comparison.ft <- comparison.df %>% t() %>% as.data.frame() %>% rownames_to_column(var = "Metric") %>% flextable() %>% 
  delete_part(part = "header") %>%
  compose(part = "body", j=1, i = 1, value = as_paragraph("Metric")) %>%
  bold(i = 1, j=2:3) %>%
  align(j=2:3, align = "right") %>%
  align(i = 1, j=2:3, align = "center") %>%
  align(j = 1, align = "left") %>%
  border_remove() %>% 
  border(i = 1, j= 2:3, border.bottom = fp_border_default(color = "black", width = 2)) %>%
  italic(i = 1, j= 1) %>% 
  valign(i = 1, j = 1, valign = "bottom") %>%
  font(part = "all", fontname = "Arial") %>% 
  fontsize(size = 12) %>% 
  fontsize(i = 1, j = 2:3, size = 14) %>%
  autofit()



save(l.cm.ft, r.cm.ft, comparison.ft, file = "./04-Data-Visualization/01-Tables/comparisons.rdata")




flextable::save_as_image(l.cm.ft, path = "./04-Data-Visualization/01-Tables/lasso_confusion.png")

flextable::save_as_image(r.cm.ft, path = "./04-Data-Visualization/01-Tables/rf_confusion.png")

flextable::save_as_image(comparison.ft, path = "./04-Data-Visualization/01-Tables/comparison.png")














