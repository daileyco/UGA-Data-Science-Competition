



# load packages
my.packages <- c("dplyr", "magrittr", "purrr", "flextable")
lapply(my.packages, library, character.only=T)


# load data
load("./01-Data/01-Clean/all_data.rds")


## extract recoded datasets
clean.data <- lapply(all.data, function(x){pluck(x, "clean")})

the.data <- clean.data[[2]]



# tab1 <- cbind(
#           Variable = names(the.data)[1], 
#           t(
#           tapply(the.data[,1], 
#                  the.data$Default_ind, 
#                  function(x){
#                    paste0(round(mean(x, na.rm=T), 2), " (", round(sd(x, na.rm=T), 2), ")")
#                    }
#                  )
#           ), 
#           t(unlist(t.test(the.data[which(the.data$Default_ind=="Defaulted"),1], the.data[which(the.data$Default_ind=="Did Not Default"),1]))) %>% as.data.frame() %>% select(Statistic = statistic.t, Parameter = parameter.df, p = p.value, Test = method)
# )
# 




create.Summary.stats <- function(the.variable, the.data, the.method){
  the.variable.data <- the.data[,the.variable]
  
  
  if(the.method == "means"){
    the.summary <- rbind(
                    cbind(
                      Levels = NA,
                      t(
                      tapply(
                        the.data[,the.variable], 
                        the.data$Default_ind, 
                        function(x){
                          paste0(round(mean(x, na.rm=T), 2), " (", round(sd(x, na.rm=T), 2), ")")
                          }
                            )
                     )
                     ),
                    cbind(
                      Levels = "Missing",
                      Defaulted = paste0(
                        sum(is.na(the.data[which(the.data$Default_ind=="Defaulted"),the.variable])), 
                        " (", round(sum(is.na(the.data[which(the.data$Default_ind=="Defaulted"),the.variable]))/nrow(the.data[which(the.data$Default_ind=="Defaulted"),])*100, 1), ")"
                      ),
                      "Did Not Default" = paste0(
                        sum(is.na(the.data[which(the.data$Default_ind=="Did Not Default"),the.variable])), 
                        " (", round(sum(is.na(the.data[which(the.data$Default_ind=="Did Not Default"),the.variable]))/nrow(the.data[which(the.data$Default_ind=="Did Not Default"),])*100, 1), ")"
                      )
                    )
                   )
    the.test <- t(
                  unlist(
                    t.test(
                      the.data[which(the.data$Default_ind=="Defaulted"),the.variable], 
                      the.data[which(the.data$Default_ind=="Did Not Default"),the.variable]
                      )
                    )
                  ) %>% 
                  as.data.frame() %>% 
                  select(
                    Statistic = statistic.t, 
                    Parameter = parameter.df, 
                    p = p.value, 
                    Test = method
                    )
  }
  
  if(the.method == "medians"){
    the.summary <- rbind(
                    cbind(
                      Levels = NA,
                      t(
                      tapply(
                        the.data[,the.variable], 
                        the.data$Default_ind, 
                        function(x){
                          paste0(round(median(x, na.rm=T), 2), " [", paste0(quantile(x, probs = c(0.25, 0.75), na.rm=T), collapse = ", "), "]")
                        }
                      )
                    )
                    ), 
                    cbind(
                      Levels = "Missing",
                      Defaulted = paste0(
                                    sum(is.na(the.data[which(the.data$Default_ind=="Defaulted"),the.variable])), 
                                    " (", round(sum(is.na(the.data[which(the.data$Default_ind=="Defaulted"),the.variable]))/nrow(the.data[which(the.data$Default_ind=="Defaulted"),])*100, 1), ")"
                                  ),
                      "Did Not Default" = paste0(
                                            sum(is.na(the.data[which(the.data$Default_ind=="Did Not Default"),the.variable])), 
                                            " (", round(sum(is.na(the.data[which(the.data$Default_ind=="Did Not Default"),the.variable]))/nrow(the.data[which(the.data$Default_ind=="Did Not Default"),])*100, 1), ")"
                                          )
                    )
                  )
    
    the.test <- t(
                  unlist(
                    wilcox.test(
                      the.data[which(the.data$Default_ind=="Defaulted"), the.variable], 
                      the.data[which(the.data$Default_ind=="Did Not Default"), the.variable])
                    )
                  ) %>% 
                as.data.frame() %>% 
                select(
                  Statistic = statistic.W, 
                  Parameter = NULL, 
                  p = p.value, 
                  Test = method
                  )
  }
  
  if(the.method == "frequencies"){
    
    the.summary <- matrix(
                    paste0(
                      table(the.data[,the.variable], the.data$Default_ind, useNA = 'always'), 
                      " (", round(prop.table(table(the.data[,the.variable], the.data$Default_ind, useNA = 'always'), margin = 2)*100, 1), ")"
                      ), 
                    ncol = 3
                    ) %>% 
                    set_colnames(
                      ., 
                      c(levels(the.data$Default_ind), "Missing")
                      ) %>% 
                    set_rownames(
                      ., 
                      c(levels(the.data[,the.variable]), "Missing")
                      ) %>% 
                    as.data.frame() %>% 
                    tibble::rownames_to_column("Levels")
    
    the.test <- t(
                  unlist(
                    chisq.test(
                      table(
                        the.data[,the.variable], 
                        the.data$Default_ind
                        )
                      )
                    )
                  ) %>% 
                  as.data.frame() %>% 
                  select(
                    Statistic = "statistic.X-squared", 
                    Parameter = parameter.df, 
                    p = p.value, 
                    Test = method
                    )
    
  }
  
  
  table.entry <- cbind(
                  Variable = c(the.variable, rep(NA, nrow(the.summary)-1)), 
                  the.summary, 
                  rbind(the.test, matrix(rep(rep(NA, ncol(the.test)), nrow(the.summary)-1), ncol = ncol(the.test))%>%set_colnames(names(the.test)))
                  )
  
  return(table.entry)
}




the.vars <- names(the.data)[-which(names(the.data)=="Default_ind")]
default.methods <- ifelse(sapply(the.data, class)=="numeric", "means", ifelse(sapply(the.data, class)=="integer", "medians", "frequencies"))




create.Table.1 <- function(the.vars, the.data, the.methods){
  
  the.table <- lapply(1:length(the.vars), function(.index){
    create.Summary.stats(the.vars[.index], the.data, the.methods[.index])
  })
  
  
  the.table %<>% bind_rows() %>% select(Variable, Levels, Defaulted, "Did Not Default", Statistic, Parameter, p, Test)
  
  the.table %<>% add_row(.before = 1)
  the.table[1,c("Defaulted", "Did Not Default")] <- cbind(
                                                    paste0(
                                                      "n = ",
                                                      nrow(the.data[which(the.data$Default_ind=="Defaulted"),]),
                                                      " (", round(nrow(the.data[which(the.data$Default_ind=="Defaulted"),])/nrow(the.data)*100, 1), ")"
                                                      ), 
                                                    paste0(
                                                      "n = ",
                                                      nrow(the.data[which(the.data$Default_ind=="Did Not Default"),]),
                                                      " (", round(nrow(the.data[which(the.data$Default_ind=="Did Not Default"),])/nrow(the.data)*100, 1), ")"
                                                      )
                                                    )
  return(the.table)
}



train.table.1 <- create.Table.1(the.vars, the.data, default.methods)


table.1s <- lapply(clean.data, function(x){create.Table.1(the.vars, x, default.methods)})

save(table.1s, file = "./04-Data-Visualization/01-Tables/table_1s.rds")
rm(list=ls())


