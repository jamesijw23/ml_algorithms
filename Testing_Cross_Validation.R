library(dplyr)
library(tidyr)
library(ggplot2)
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


cross_validation_KNN(df,type_cv = 2,p_test=0.2,KN = 3)






###########


beaver_df1 = datasets::beaver1
beaver_df2 = datasets::beaver2
beaver_df = rbind(beaver_df1,beaver_df2)
beaver_df = beaver_df %>% select(activ,time,temp,day)

cross_validation_KNN(beaver_df,type_cv = 1,p_test=0.2,KN = 3)
