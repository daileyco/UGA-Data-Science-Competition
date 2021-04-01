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
  
  library(Hmisc)
corrs_num_num = rcorr(as.matrix(train[,c(1:5,9,14:17,19)]))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
corrs = flattenCorrMatrix(corrs_num_num$r, corrs_num_num$P)

library(corrplot)
rownames(corrs_num_num$r) = c("Total Credit Debt", "Average Card Debt", "Credit Age", 
                              "Good Credit Age", "Card Age", "Credit Amount Past Due", 
                              "Utili. on Accounts", "Credit Products >50% Utili.", 
                              "Max Credit Limit Utili.", "Credit Cards >50% Utili.", 
                              "Income")

colnames(corrs_num_num$r) = c("Total Credit Debt", "Average Card Debt", "Credit Age", 
                              "Good Credit Age", "Card Age", "Credit Amount Past Due", 
                              "Utili. on Accounts", "Credit Products >50% Utili.", 
                              "Max Credit Limit Utili.", "Credit Cards >50% Utili.", 
                              "Income")
setwd("./../../03-Exploratory-Analyses")

png(file = "corrMatrixNumNum_ID.png")
corrplot(corrs_num_num$r, type = "upper", order = "FPC", diag = F, 
         tl.col = "black", tl.srt = 45, method = "circle", outline=T)

dev.off()


  fp <- ggplot(corrs, aes(row, column)) + 
  geom_tile(aes(fill = cor), colour = "white") + 
  scale_fill_gradient2(low = "steelblue3", mid = "lightgray", high = "firebrick3")+
  theme_grey(base_size = 14) + 
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  geom_text(aes(label = round(cor, 3)), size = 3.5) +
  labs(title = "Partial Correlation Matrix",
       x = "Outputs", 
       y = "Varied Inputs") +
  labs(fill = "Correlation") + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
  axis.text.x = element_text(angle = 45, hjust=1, vjust = 1))

p

