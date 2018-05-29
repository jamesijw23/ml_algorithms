df = iris %>% select(Species,Sepal.Length,
                           Sepal.Width,Petal.Length,
                           Petal.Width )


cross_validation_KNN(df,type_cv = 2,p_test=0.2,KN = 10)
