







# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable", "glmnet", "doParallel", "parallel", "pROC")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract recoded datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})

imputed.data <- lapply(all.data, function(x){pluck(x, "imputed")})

the.data <- clean.data[[2]]
the.data <- imputed.data[[2]]
the.validation.data <- imputed.data[[3]]




the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]


the.data.complete <- the.data[complete.cases(the.data),]
the.data.complete <- the.validation.data[complete.cases(the.validation.data),]


the.predictors <- the.data.complete[,the.vars]


the.interactions <- t(combn(the.vars, 2))
the.interactions <- apply(the.interactions, MARGIN = 1, paste0, collapse="*")



# the.predictors.formula <- as.formula(paste0("~", paste0(the.interactions, collapse = "+")))
the.predictors.formula <- as.formula(paste0("~", paste0(the.vars, collapse = "+")))

x <- model.matrix(the.predictors.formula, data = the.predictors)[,-1]

y <- the.data.complete$Default_ind

















load("./05-Model-Fitting/01-Logistic-Regression/model_metrics_validation.rds")





mv <- lapply(all.model.evaluations, bind_rows) %>% 
        bind_rows() %>%
        mutate(profit.pp = profit / n, 
               model.num = 1:nrow(.))

# mv %>% group_by(Model, gamma, lambda) %>% summarise(n=n()) %>% filter(n>2)

mv <- mv[-which(mv$Model==1 & is.infinite(mv$threshold) & !duplicated(mv$lambda)),]


mv <- mv %>% mutate(metric = rep(c("profit", "d2c"), 450), 
                    gamma = factor(gamma))

mv.profit <- mv %>% filter(metric == "profit")





the.ylims <- (range(mv.profit$profit.pp)-median(mv.profit$profit.pp))*1.2+median(mv.profit$profit.pp)


png(filename = "./04-Data-Visualization/02-Figures/alasso_models_profit.png", width = 16, height = 9, units = "in", res = 300, pointsize = 16)

plot(NA, type = 'n', xlim = c(0,length(unique(mv.profit$gamma))*10+10), ylim = the.ylims, axes = F, xlab = "", ylab = "")

axis(1, at = c(10, 20, 30, 40, 50), labels = levels(mv.profit$gamma), padj = -1, tck = 0.02, cex.axis = 0.7)

axis(2, at = round(seq(min(mv.profit$profit.pp), max(the.ylims), by = 2), 0), labels = TRUE, tick = TRUE, las = 1, cex.axis = 0.7)

title(xlab = expression(gamma), ylab = "Estimated Monthly Profit per Person (USD)", cex.lab = 1)


points(jitter((as.numeric(mv.profit$gamma))*10, 1.5), mv.profit$profit.pp, pch = 16, col = viridis::viridis(length(unique(mv.profit$lambda)))[order(mv.profit$lambda)])


l.gradient <- rep(NA, 450)
l.gradient[c(1,150,300,450)] = c(1,150,300,450)

legend(x = par('usr')[2], y = par('usr')[4], 
       pt.cex = rep(NA, 12+length(l.gradient)), 
       fill = viridis::viridis(450),
       border = viridis::viridis(450),
       legend = l.gradient, 
       y.intersp = rep(0.01, 450),  
       bty = 'n', xjust = 1, 
       cex = 0.6)

text(x = par('usr')[2], y = par('usr')[4], labels = expression(lambda), xpd = T, adj = c(4, 0))

dev.off()










the.ylims <- (range(mv.profit$auc)-median(mv.profit$auc))*1.2+median(mv.profit$auc)


png(filename = "./04-Data-Visualization/02-Figures/alasso_models_auc.png", width = 16, height = 9, units = "in", res = 300, pointsize = 16)

plot(NA, type = 'n', xlim = c(0,length(unique(mv.profit$gamma))*10+10), ylim = the.ylims, axes = F, xlab = "", ylab = "")

axis(1, at = c(10, 20, 30, 40, 50), labels = levels(mv.profit$gamma), padj = -1, tck = 0.02, cex.axis = 0.7)

axis(2, at = round(seq(min(mv.profit$auc), max(the.ylims), by = 0.05), 3), labels = TRUE, tick = TRUE, las = 1, cex.axis = 0.7)

title(xlab = expression(gamma), ylab = "Area Under the Receiver Operating Characteristic Curve", cex.lab = 1)


points(jitter((as.numeric(mv.profit$gamma))*10, 1.5), mv.profit$auc, pch = 16, col = viridis::viridis(length(unique(mv.profit$lambda)))[order(mv.profit$lambda)])


l.gradient <- rep(NA, 450)
l.gradient[c(1,150,300,450)] = c(1,150,300,450)

legend(x = par('usr')[2], y = par('usr')[4], 
       pt.cex = rep(NA, 12+length(l.gradient)), 
       fill = viridis::viridis(450),
       border = viridis::viridis(450),
       legend = l.gradient, 
       y.intersp = rep(0.01, 450),  
       bty = 'n', xjust = 1, 
       cex = 0.6)

text(x = par('usr')[2], y = par('usr')[4], labels = expression(lambda), xpd = T, adj = c(4, 0))

dev.off()













the.most.accurate.auc <- mv.profit$model.num[which(mv.profit$auc > quantile(mv.profit$auc, 0.975))]

the.most.profitable <- mv.profit[which(mv.profit$model.num%in%the.most.accurate.auc),]


points(jitter((as.numeric(mv.profit$gamma[which(mv.profit$model.num=="146")]))*10, 1.5), mv.profit$profit.pp[which(mv.profit$model.num=="146")], pch = 8, cex = 5)


the.chosen.one <- the.most.profitable[which.min(the.most.profitable$d2c),]












































