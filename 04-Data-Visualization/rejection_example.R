













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











# random forest


load("./05-Model-Fitting/02-Machine-Learning/01-Models/rf_fit23.rds")

load("./05-Model-Fitting/02-Machine-Learning/the-chosen-one.rds")


rf.chosen <- the.chosen.one

rm(the.chosen.one)


rf.probs <- predict(rf_fit23, the.predictors, type = "prob") %>% as.data.frame()


rf.predictions <- factor(ifelse(rf.probs$Defaulted>rf.chosen$threshold, "Defaulted", "Did Not Default"), levels = c("Defaulted", "Did Not Default")) %>% 
  relevel(., ref = "Did Not Default")


rf.cm <- data.frame(truth = the.truth, prediction = rf.predictions)

# rf.df <- rf.cm %>%
#   summarise(
#     model = "RF", 
#     TP = sum(truth == "Defaulted" & prediction == "Defaulted"), 
#     FP = sum(truth == "Did Not Default" & prediction == "Defaulted"), 
#     TN = sum(truth == "Did Not Default" & prediction == "Did Not Default"), 
#     FN = sum(truth == "Defaulted" & prediction == "Did Not Default"), 
#     n = n()
#   )



the.observations <- the.data.complete %>% 
                    bind_cols(., 
                              prob = rf.probs$Defaulted,
                              pred = rf.predictions) %>% 
                    mutate(
                      TP = Default_ind=="Defaulted" & pred == "Defaulted", 
                      FP = Default_ind=="Did Not Default" & pred == "Defaulted", 
                      TN = Default_ind=="Did Not Default" & pred == "Did Not Default", 
                      FN = Default_ind=="Defaulted" & pred == "Did Not Default"
                      )

the.tn.obs <- the.observations %>% 
                  filter(TN) 



                    # %>% 
                  # summarise_all(mean) %>% 
                  # mutate(ind_acc_XYZ = "No Account", 
                  #        States = "GA", 
                  #        Default_ind = "Did Not Default", 
                  #        pred = "Did Not Default"
                  #        )

the.tn.obs <- the.observations %>% 
  filter(TN) %>%
summarise_all(mean) %>%
mutate(ind_acc_XYZ = "No Account",
       States = "GA",
       Default_ind = "Did Not Default",
       pred = "Did Not Default"
       )




the.positives <- the.observations %>% 
                  filter(pred=="Defaulted")



set.seed(74)
the.example.index <- sample(1:nrow(the.positives), 1)
the.tn.index <- sample(1:nrow(the.tn.obs), 1)
the.example <- bind_rows(the.positives[the.example.index,] %>% mutate(ex=1), 
                         the.tn.obs[the.tn.index,] %>% mutate(ex=0)) %>% 
                t() %>% 
                as.data.frame() %>%
                setNames(., nm = c("Ex.obs", "TN.obs")) %>%
                rownames_to_column(var = "variable")


for(i in 1:length(the.vars)){
the.example[,paste0("replace.", the.vars[i])] <- ifelse(the.example$variable == the.vars[i], the.example$TN.obs, the.example$Ex.obs)
}


the.var.combos <- combn(the.vars, 2) %>% t() %>% as.data.frame()





the.example %<>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "scenario")

names(the.example) <- the.example[1,]

the.example <- the.example[-1,]

the.example %<>% 
                mutate(non_mtg_acc_past_due_12_months_num = as.integer(non_mtg_acc_past_due_12_months_num),
                        non_mtg_acc_past_due_6_months_num = as.integer(non_mtg_acc_past_due_6_months_num), 
                        mortgages_past_due_6_months_num = as.integer(mortgages_past_due_6_months_num), 
                        inq_12_month_num = as.integer(inq_12_month_num), 
                        card_inq_24_month_num = as.integer(card_inq_24_month_num), 
                        card_open_36_month_num = as.integer(card_open_36_month_num), 
                        auto_open_.36_month_num = as.integer(auto_open_.36_month_num), 
                        ind_acc_XYZ = factor(ind_acc_XYZ, levels = c("Account w/ XYZ", "No Account")), 
                        Default_ind = factor(Default_ind, levels = c("Defaulted", "Did Not Default")), 
                        States = factor(States, levels = c("AL", "FL", "GA", "LA", "MS", "NC", "SC"))) %>%
                mutate(Default_ind = relevel(Default_ind, ref = "Did Not Default")) %>% 
                mutate_at(c(2:6,10,15:18,20, 23,29), as.double) %>% 
                mutate(prob = c(the.positives$prob[the.example.index],
                                the.tn.obs$prob[the.tn.index], 
                                rep(the.positives$prob[the.example.index], nrow(.)-2)))



the.new.probs <- predict(rf_fit23, the.example[,the.vars], type = "prob") %>% as.data.frame()

the.example <- bind_cols(the.example, 
                         the.new.probs
                        ) %>% 
                mutate(States = as.character(States), 
                       ind_acc_XYZ = as.character(ind_acc_XYZ))


the.vars.pretty <- c("Total Credit Debt", "Average Monthly Credit Card Debt", 
                     "Credit Age", "Credit Age (Good)", "Credit Card Age", 
                     "Non-mortgage Deliquencies (12 Months)", "Non-mortgage Deliquencies (6 Months)", "Mortgage Deliquencies (6 Months)", "Total Credit Past Due", 
                     "Credit Inquiries (12 Months)", "Credit Card Inquiries (24 Months)", 
                     "Opened Credit Cards (36 Months)", "Opened Auto Loans (36 Months)", "Credit Card Utilization (All)", "Percentage Credit Products >50% Utilization", "Credit Utilization (Highest Limit)", "Percentage Credit Cards >50% Utilization", 
                     "Account @ Bank XYZ", "Reported Income", "Residence State")











png(filename = "./04-Data-Visualization/02-Figures/rejection_example.png", width = 16, height = 9, unit = "in", res = 300, pointsize = 16, family = "sans")


par(mar = c(5.1,23.1,4.1,2.1))
dotchart(rep(the.example$prob[1], 
             nrow(the.example)-2), 
         xlim = (range(the.example$Defaulted)-median(the.example$Defaulted))*1.2+median(the.example$Defaulted), 
         pt.cex = 1.5, 
         pch = 16, 
         xaxt = 'n'
         )


abline(v = rf.chosen$threshold, lty = 3, col = "red1", lwd = 2)

axis(2, at = c((nrow(the.example)-2):1), labels = the.vars.pretty, las = 1, cex.axis = 0.7, hadj = 0, line = 22, tick = F)
axis(2, at = c((nrow(the.example)-2):1), labels = FALSE, las = 1, cex.axis = 0.7, hadj = 0, line = 10, tick = T, tck = 0)
axis(2, at = c((nrow(the.example)-2):1), labels = as.character(the.example[1, the.vars]), las = 1, cex.axis = 0.7, hadj = 1, line = 5, tick = F)
axis(2, at = c((nrow(the.example)-2):1), labels = as.character(the.example[2, the.vars]), las = 1, cex.axis = 0.7, hadj = 1, line = 0, tick = F)

axis(2, at = c((nrow(the.example)-0.4)), labels = "Client\nObservations", las = 1, cex.axis = 0.7, hadj = 0, line = 8, tick = F, xpd = T)
axis(2, at = c((nrow(the.example)-0.4)), labels = "Mean\nAccepted", las = 1, cex.axis = 0.7, hadj = 0, line = 3, tick = F, xpd = T)

axis(2, at = c((nrow(the.example)-1)), labels = F, las = 1, cex.axis = 0.7, hadj = 0.5, line = 1, tick = T, tcl = -8, xpd = T)


axis(1, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.02), labels = T, cex.axis = 0.7, line = 0, tick = T, tcl = -0.25, padj = -2)
axis(1, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.01), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = -0.125)

axis(1, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.02), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = 0.25)
axis(1, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.01), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = 0.125)



axis(3, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.02), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = -0.25)
axis(3, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.01), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = -0.125)

axis(3, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.02), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = 0.25)
axis(3, at = seq(0,round(max(the.example$Defaulted)*1.2, 2), by = 0.01), labels = F, cex.axis = 0.7, line = 0, tick = T, tcl = 0.125)



axis(3, at = rf.chosen$threshold, labels = round(rf.chosen$threshold, 4), cex.axis = 0.7, line = 0, tick = T, tcl = -1, padj = 0, col.ticks = "red1", lty = 3, lwd = 2)



the.example$pch <- ifelse(the.example$prob>the.example$Defaulted, 24, ifelse(the.example$prob<the.example$Defaulted, 25, 0))
the.example$col <- ifelse(the.example$prob>the.example$Defaulted, 
                          "green", 
                          ifelse(the.example$prob<the.example$Defaulted, 
                                 "red", 
                                 'gainsboro'))
the.example$nudge <- ifelse(the.example$prob>the.example$Defaulted, -par('pin')[2]/22*par('cin')[2], ifelse(the.example$prob<the.example$Defaulted, par('pin')[2]/22*par('cin')[2], 0))
the.example$cex <- ifelse(the.example$prob>the.example$Defaulted, 1, ifelse(the.example$prob<the.example$Defaulted, 1, 2))

the.example$nudge2 <- the.example$nudge*3

the.seq <- (nrow(the.example)-2):1


for(i in 1:length(the.seq)){
points(the.example$Defaulted[nrow(the.example)-the.seq[i]+1], 
       the.seq[i]+the.example$nudge2[nrow(the.example)-the.seq[i]+1], 
       pch = the.example$pch[nrow(the.example)-the.seq[i]+1], 
       bg = the.example$col[nrow(the.example)-the.seq[i]+1], 
       cex = the.example$cex[nrow(the.example)-the.seq[i]+1])
}



arrows(x0=par('usr')[1]+abs(par('usr')[1])*0.5, 
       x1=par('usr')[1], 
       y0 = nrow(the.example)-which(the.example$Defaulted<rf.chosen$threshold)[-1]+1, 
       lwd = 2)


dev.off()






png(filename = "./04-Data-Visualization/02-Figures/rf_variable_importance.png", width = 16, height = 9, unit = "in", res = 300, pointsize = 16, family = "sans")

dotchart(rf_fit23$importance[order(rf_fit23$importance[,3]),3], 
         xlim = range(rf_fit23$importance[,1:3]%>%as.numeric())*1.1, 
         pch = 8, 
         labels = the.vars.pretty[order(rf_fit23$importance[,3])])
points(rf_fit23$importance[order(rf_fit23$importance[,3]),2], 1:20, pch = 7)
points(rf_fit23$importance[order(rf_fit23$importance[,3]),1], 1:20, pch = 16)
legend('bottomright', pch = c(8, 7, 16), legend = c("Overall", "Defaulted", "Did Not Default"), cex = 0.6)



dev.off()






















