library(dplyr)
## When using this package be sure:
##  1. Make Outcome the first feature
##  2. Load dplyr
##  2. Make Sure to provide a test df



## Makes Response Variable first column in df (in this case!!!)
iris_mod = iris %>% select(Species,Sepal.Length,
                           Sepal.Width,Petal.Length,
                           Petal.Width )

## Testing Split Function
s_df = split_df(iris_mod,0.1)
test_df = s_df$test_df  
train_df = s_df$train_df 

knn_cpp(train_df, test_df, k, distance='euclid')




chickw = tbl_df(ChickWeight)
iris_mod = iris %>% select(Diet,Time,
                           Sepal.Width,Petal.Length,
                           Petal.Width )
