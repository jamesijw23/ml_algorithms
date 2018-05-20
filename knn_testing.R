## Testing Split Function
s_df = split_df(iris,0.1)
test_df = s_df$test_df %>% select(-Species)
train_df = s_df$train_df %>% select(-Species)


## Test of Distance Functions
v1 = c(1,-1,3)
v2 = c(4,2,3)

euclideanDistance(v1,v2,2)
absoluteDistance(v1,v2,2)




## Test get Neighbors function
d = getNeighbors(train_df,test_df[1,],3, type_distance="euclid")