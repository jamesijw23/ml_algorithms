library(ggplot2)
library(dplyr)
library(tidyr)
df = iris %>% select(Species,Sepal.Length,
                     Sepal.Width,Petal.Length,
                     Petal.Width )

k = 3
## Testing Split Function
s_df = split_df(df,0.1)
test_df = s_df$test_df  
train_df = s_df$train_df 

model1 = knn_cpp(train_df, test_df, k, distance='euclid')

tr = test_df[,1]
es = model1$predictions

m = metrics(tr,es)
long_m = gather(m,metrics_name,metric_value,TPR_metric:MKd_metric)
few_metric = long_m %>% 
  filter(metrics_name == "TPR_metric" |
           metrics_name == "TNR_metric" |
           metrics_name == "ACC_metric" | 
           metrics_name == "PPV_metric")
ggplot(few_metric,aes(x=metrics_name,y=metric_value)) + 
  ggtitle("Recovery of Each Level") +
  geom_point()+
  xlab("Metrics Names") +
  ylab("Measurements") +
  facet_grid(~Levels_name) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
