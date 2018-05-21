## Makes Response Variable first column in df (in this case!!!)
iris_mod = iris %>% select(Species,Sepal.Length,
                           Sepal.Width,Petal.Length,
                           Petal.Width )

## Testing Split Function
s_df = split_df(iris_mod,0.1)
test_df = s_df$test_df  
train_df = s_df$train_df 


## Test of Distance Functions
v1 = c(1,-1,3)
v2 = c(4,2,3)

euclideanDistance(v1,v2,2)
absoluteDistance(v1,v2,2)


## Test get Neighbors function
d = getNeighbors(train_df,test_df[10,],4, type_distance="euclid")

## Test get Response Function
r = getResponse(d)

## Test main KNN function
p = emans_knn(train_df,test_df,1)
