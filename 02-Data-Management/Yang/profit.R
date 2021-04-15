# In a population, assume we get a number of applications (1e4) in which with x% of clients will
# finally refuse to pay back money, with y sensitivity and z specificity, we would have a profit
# function of Profit = (1e4)*(x%)*(z)*161 - (1e4)*(1-x%)*(1-y)*13763. We used this function to explore which
# level of sensitivity and specificity we should consider
library(tidyverse)
library(plotly)


applications = 1;
earn = 1000
loss = 5000
specificity = seq(0,1,0.01)
x = 1 - specificity
sensitivity = -(x-1)^2 + 1
badapplications = seq(0,0.5,0.001); # x%
models_performance = crossing(data.frame(specificity,sensitivity,x),badapplications)
profit = data.frame(Profit = (applications)*(1-badapplications)*(models_performance$specificity)*earn - (applications)*(badapplications)*(1-models_performance$sensitivity)*loss)

final_data <- bind_cols(models_performance,profit)


ggplot(final_data,aes(x = specificity, y = sensitivity)) +
  geom_line()


p <- plot_ly(data = final_data, x = ~x, y = ~sensitivity, z = ~badapplications, color = ~Profit) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = '1-Specificity'),
                      yaxis = list(title = 'Sensitivity'),
                      zaxis = list(title = 'Prevalence of the outcome')))
p 


