







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





# load model evaluations
## random forests

dir("./05-Model-Fitting/02-Machine-Learning")

load("./05-Model-Fitting/02-Machine-Learning/model_metrics_training2.rds")

metrics.training <- model.evaluations



load("./05-Model-Fitting/02-Machine-Learning/model_metrics_training_oob_cost2.rds")

metrics.training.oob <- model.evaluations



load("./05-Model-Fitting/02-Machine-Learning/model_metrics_validation2.rds")

metrics.validation <- model.evaluations

rm(model.evaluations)






training.rocs <- lapply(metrics.training, function(x){
  pluck(x, "auc") %>% attr(., "roc")
})


metrics.training <- lapply(metrics.training, function(x){
  x %>% mutate(auc = as.numeric(auc))
})


metrics.training <- bind_rows(metrics.training) %>% bind_cols(Metric = rep(c("profit", "d2c"), 340))







training.oob.rocs <- lapply(metrics.training.oob, function(x){
  pluck(x, "auc") %>% attr(., "roc")
})


metrics.training.oob <- lapply(metrics.training.oob, function(x){
  x %>% mutate(auc = as.numeric(auc))
})


metrics.training.oob <- bind_rows(metrics.training.oob)[-555,] %>% bind_cols(Metric = rep(c("profit", "d2c"), 340))









validation.rocs <- lapply(metrics.validation, function(x){
  pluck(x, "auc") %>% attr(., "roc")
})

metrics.validation <- lapply(metrics.validation, function(x){
  x %>% mutate(auc = as.numeric(auc))
})

metrics.validation <- bind_rows(metrics.validation) %>% bind_cols(Metric = rep(c("profit", "d2c"), 340))









rf.tuning.grid <- bind_rows(
  expand.grid(.stratified = TRUE, 
              .sampsize.ratio = 1:3, 
              .ntree = seq(401, 2001, by = 400), 
              .mtrys = 2:18), 
  expand.grid(.stratified = FALSE, 
              .sampsize.ratio = NA, 
              .ntree = seq(401, 2001, by = 400), 
              .mtrys = 2:18)
) %>% mutate(Model = paste0("rf_fit", 1:340), model.num = 1:340)




metrics <- bind_rows(metrics.training %>% mutate(dataset = "training"), 
                     bind_rows(metrics.training.oob %>% mutate(dataset = "training.oob"), 
                               metrics.validation %>% mutate(dataset = "validation"))) %>% 
            mutate(model.num = as.numeric(gsub("rf_fit", "", Model))) %>%
            arrange(dataset, model.num) %>% 
            select(-.stratified, -.sampsize.ratio, -.ntree, -.mtrys) %>% 
            full_join(rf.tuning.grid, ., by = c("Model", "model.num")) %>% 
            select(dataset, Model, model.num, everything()) %>%
            arrange(dataset, model.num) %>%
            mutate(profit.pp = profit / n) %>% 
            mutate_at(4:7, as.factor)


metrics.profit %<>% mutate(profit.pp = profit / n )


metrics.model.auc <- metrics %>% group_by(dataset) %>% filter(!duplicated(Model)) %>% ungroup() %>% select(1:8)
metrics.profit <- metrics %>% filter(Metric == "profit" & dataset != "training.oob")
metrics.d2c <- metrics %>% filter(Metric == "d2c" & dataset != "training.oob")

boxplot(metrics.model.auc$auc~metrics.model.auc$dataset)
boxplot(metrics.model.auc$auc~metrics.model.auc$.stratified+metrics.model.auc$dataset)
boxplot(metrics.model.auc$auc~metrics.model.auc$.sampsize.ratio+metrics.model.auc$dataset)
boxplot(metrics.model.auc$auc~metrics.model.auc$.ntree+metrics.model.auc$dataset)
boxplot(metrics.model.auc$auc~metrics.model.auc$.mtrys+metrics.model.auc$dataset)



boxplot(metrics.d2c$d2c~metrics.d2c$dataset)
boxplot(metrics.d2c$d2c~metrics.d2c$.stratified+metrics.d2c$dataset)
boxplot(metrics.d2c$d2c~metrics.d2c$.sampsize.ratio+metrics.d2c$dataset)

boxplot(metrics.d2c$sensitivity~metrics.d2c$.stratified+metrics.d2c$dataset)
boxplot(metrics.d2c$sensitivity~metrics.d2c$.sampsize.ratio+metrics.d2c$dataset)

boxplot(metrics.d2c$specificity~metrics.d2c$.stratified+metrics.d2c$dataset)
boxplot(metrics.d2c$specificity~metrics.d2c$.sampsize.ratio+metrics.d2c$dataset)


boxplot(metrics.d2c$d2c~metrics.d2c$.ntree+metrics.d2c$dataset)
boxplot(metrics.d2c$d2c~metrics.d2c$.mtrys+metrics.d2c$dataset)







boxplot(metrics.profit$profit~metrics.profit$dataset)
boxplot(metrics.profit$profit~metrics.profit$.stratified+metrics.profit$dataset)
boxplot(metrics.profit$profit~metrics.profit$.sampsize.ratio+metrics.profit$dataset)

boxplot(metrics.profit$sensitivity~metrics.profit$.stratified+metrics.profit$dataset)
boxplot(metrics.profit$sensitivity~metrics.profit$.sampsize.ratio+metrics.profit$dataset)

boxplot(metrics.profit$specificity~metrics.profit$.stratified+metrics.profit$dataset)
boxplot(metrics.profit$specificity~metrics.profit$.sampsize.ratio+metrics.profit$dataset)


boxplot(metrics.profit$profit~metrics.profit$.ntree+metrics.profit$dataset)
boxplot(metrics.profit$profit~metrics.profit$.mtrys+metrics.profit$dataset)






boxplot(metrics.profit$profit.pp~metrics.profit$dataset)
boxplot(metrics.profit$profit.pp~metrics.profit$.stratified+metrics.profit$dataset)
boxplot(metrics.profit$profit.pp~metrics.profit$.sampsize.ratio+metrics.profit$dataset)
boxplot(metrics.profit$profit.pp~metrics.profit$.ntree+metrics.profit$dataset)
boxplot(metrics.profit$profit.pp~metrics.profit$.mtrys+metrics.profit$dataset)







metrics.validation <- metrics %>% 
                        filter(dataset=="validation") %>% 
                        mutate(.sampsize.ratio = ifelse(is.na(.sampsize.ratio), 0, .sampsize.ratio)) %>%
                        mutate(.ss = as.logical(as.character(.stratified)) * .sampsize.ratio)

mv.profit <- metrics.validation %>%
              filter(Metric=="profit")

mv.d2c <- metrics.validation %>%
            filter(Metric=="d2c")


boxplot(mv.d2c$auc~(mv.d2c$.stratified+1)*mv.d2c$.sampsize.ratio)
boxplot(mv.d2c$auc~mv.d2c$.ss)

boxplot(mv.profit$profit.pp~mv.profit$.ss)
# boxplot(mv.profit$profit~mv.profit$.ss)

boxplot(mv.d2c$d2c~mv.d2c$.ntree)
boxplot(mv.profit$profit.pp~mv.profit$.ntree)


boxplot(mv.d2c$d2c~mv.d2c$.mtrys)
boxplot(mv.profit$profit.pp~mv.profit$.mtrys)


the.ylims <- (range(mv.profit$profit.pp)-median(mv.profit$profit.pp))*1.2+median(mv.profit$profit.pp)







png(filename = "./04-Data-Visualization/02-Figures/rf_models_profit.png", width = 16, height = 9, units = "in", res = 300, pointsize = 16)

plot(NA, type = 'n', xlim = c(0,length(unique(mv.profit$.ss))*10+10), ylim = the.ylims, axes = F, xlab = "", ylab = "")

axis(1, at = c(10, 20, 30, 40), labels = c("Random", rep("Stratified",3)), padj = -1, tck = 0.02, cex.axis = 0.7)
axis(1, at = c(20, 30, 40), labels = c("1:1", "1:2", "1:3"), tick = F, padj = 0.25, cex.axis = 0.7)

axis(2, at = round(seq(min(mv.profit$profit.pp), max(the.ylims), by = 10), 0), labels = TRUE, tick = TRUE, las = 1, cex.axis = 0.7)

title(xlab = "Resampling Scheme", ylab = "Estimated Monthly Profit per Person (USD)", cex.lab = 1)


points(jitter((as.numeric(mv.profit$.ss)+1)*10, 1.5), mv.profit$profit.pp, pch = 16, cex = (as.numeric(mv.profit$.ntree)-3)*0.2+1, col = viridis::viridis(17)[as.numeric(mv.profit$.mtrys)])


l.gradient <- rep(NA, 17)
l.gradient[c(1,9,17)] = c(1,9,17)

legend(x = par('usr')[1], y = par('usr')[4], 
       pt.cex = c(0.6,0.8,1,1.2,1.4, rep(NA, 12+length(l.gradient))), 
       pch = c(rep(1, 5), rep(NA, 12+length(l.gradient))), 
       fill = c(rep(NA, 5+12), viridis::viridis(17)),
       border = c(rep(NA, 5+12), viridis::viridis(17)),
       legend = c(401,801,1201,1601,2001, rep(NA, 12), l.gradient+1), 
       y.intersp = c(rep(1,5), rep(0.5, 12+17)), 
       cex = c(rep(0.6, 5+12), rep(0.6, 17)), 
       ncol = 2, 
       bty = 'n')
# 
# legend(x = par('usr')[1]*2, y = par('usr')[4], 
#        legend = c("ntree", "mtry"), 
#        ncol = 2, 
#        pch = c(NA, NA), 
#        bty = 'n', 
#        adj = c(0,-1.2), xpd = T, cex = 0.8)


text(x = par('usr')[1], y = par('usr')[4], labels = "ntree", xpd = T, adj = c(-0.2, 0))
text(x = par('usr')[1], y = par('usr')[4], labels = "mtry", xpd = T, adj = c(-1.7, 0))

dev.off()








the.ylims <- (range(mv.profit$auc)-median(mv.profit$auc))*1.2+median(mv.profit$auc)



png(filename = "./04-Data-Visualization/02-Figures/rf_models_auc.png", width = 16, height = 9, units = "in", res = 300, pointsize = 16)

plot(NA, type = 'n', xlim = c(0,length(unique(mv.profit$.ss))*10+10), ylim = c(0.850, the.ylims[2]), axes = F, xlab = "", ylab = "")

axis(1, at = c(10, 20, 30, 40), labels = c("Random", rep("Stratified",3)), padj = -1, tck = 0.02, cex.axis = 0.7)
axis(1, at = c(20, 30, 40), labels = c("1:1", "1:2", "1:3"), tick = F, padj = 0.25, cex.axis = 0.7)

axis(2, at = c(0.850, 0.855, 0.860, 0.865), labels = TRUE, tick = TRUE, las = 1, cex.axis = 0.7)

title(xlab = "Resampling Scheme", ylab = "Area Under the Receiver Operating Characteristic Curve", cex.lab = 1)


points(jitter((as.numeric(mv.profit$.ss)+1)*10, 1.5), mv.profit$auc, pch = 16, cex = (as.numeric(mv.profit$.ntree)-3)*0.2+1, col = viridis::viridis(17)[as.numeric(mv.profit$.mtrys)])


l.gradient <- rep(NA, 17)
l.gradient[c(1,9,17)] = c(1,9,17)

legend(x = par('usr')[1], y = par('usr')[4], 
       pt.cex = c(0.6,0.8,1,1.2,1.4, rep(NA, 12+length(l.gradient))), 
       pch = c(rep(1, 5), rep(NA, 12+length(l.gradient))), 
       fill = c(rep(NA, 5+12), viridis::viridis(17)),
       border = c(rep(NA, 5+12), viridis::viridis(17)),
       legend = c(401,801,1201,1601,2001, rep(NA, 12), l.gradient+1), 
       y.intersp = c(rep(1,5), rep(0.5, 12+17)), 
       cex = c(rep(0.6, 5+12), rep(0.6, 17)), 
       ncol = 2, 
       bty = 'n')
# 
# legend(x = par('usr')[1]*2, y = par('usr')[4], 
#        legend = c("ntree", "mtry"), 
#        ncol = 2, 
#        pch = c(NA, NA), 
#        bty = 'n', 
#        adj = c(0,-1.2), xpd = T, cex = 0.8)


text(x = par('usr')[1], y = par('usr')[4], labels = "ntree", xpd = T, adj = c(-0.2, 0))
text(x = par('usr')[1], y = par('usr')[4], labels = "mtry", xpd = T, adj = c(-1.7, 0))

dev.off()





























the.most.accurate.auc <- mv.d2c$Model[which(mv.d2c$auc > quantile(mv.d2c$auc, 0.975))]

# the.most.accurate.d2c <- mv.d2c$Model[which(mv.d2c$d2c < quantile(mv.d2c$d2c, 0.025))]


the.most.profitable <- mv.profit[which(mv.profit$Model%in%the.most.accurate.auc),]

# the.most.profitable.d2c <- mv.profit[which(mv.profit$Model%in%the.most.accurate.d2c),]


# the.chosen.few <- bind_rows(the.most.profitable.auc[which(the.most.profitable.auc$profit.pp > quantile(the.most.profitable.auc$profit.pp, 0.75)),],
                            the.most.profitable.d2c[which(the.most.profitable.d2c$profit.pp > quantile(the.most.profitable.d2c$profit.pp, 0.75)),])
# View(the.chosen.few[,c("Model", "auc", "sensitivity", "specificity", "d2c", "TP", "FP", "TN", "FN", "profit.pp")])




# points(jitter((as.numeric(mv.profit$.ss[which(mv.profit$Model%in%c("rf_fit80", "rf_fit53", "rf_fit23"))])+1)*10, 1.5), mv.profit$profit.pp[which(mv.profit$Model%in%c("rf_fit80", "rf_fit53", "rf_fit23"))], pch = c(8,9,10), cex = 5)


the.chosen.one <- the.most.profitable[which.min(the.most.profitable$d2c),]




















