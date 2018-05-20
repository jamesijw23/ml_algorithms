library(dplyr)
##--------------------------------------
## Functions
##--------------------------------------

##--------------------------------------
## Dependencies:
## a. dplyr: df manipulation
##--------------------------------------
##--------------------------------------
## Name: split_df
## Input: 
## 1) df--> dataframe
## 2) split --> percent of df to be the test 
## Output: A list that contains train and test dfs
## Purpose: Split the df into a training and test dfs for
## validation purposes
## Improvements: Not have to create & delete tmp row_num col
##--------------------------------------
split_df = function(df,split){
  n_df = nrow(df)
  n_test = round(split*n_df)
  rows_test = sample(n_df,n_test)
  temp_df = data.frame(row_num = seq(1,n_df),df)
  test_df = temp_df %>% filter(row_num %in% rows_test)
  train_df = temp_df %>% filter(!(row_num %in% rows_test))
  return(list(train_df = train_df %>% select(-row_num),
              test_df = test_df %>% select(-row_num)))
}



##--------------------------------------
## Name: euclideanDistance
## Input: 
## 1) instance1 --> 1st vector of data
## 2) instance1 --> 2nd vector of data
## 3) length --> vector length
## Output: Square root of distance
## Purpose: Ability to find euclidean distance between
## two vectors
## Improvements: Do not use for loop
##--------------------------------------
euclideanDistance = function(instance1, instance2, length){
  distance = 0
  for(i in 1:length){
    distance = distance + (instance1[i] - instance2[i])^2
  }
    return(sqrt(distance))
}


##--------------------------------------
## Name:
## Input: 
## Output:
## Purpose:
##--------------------------------------  
getNeighbors(trainingSet, testInstance, k):

  
##--------------------------------------
## Name:
## Input: 
## Output:
## Purpose:
##--------------------------------------  
getResponse(neighbors):

  
##--------------------------------------
## Name:
## Input: 
## Output:
## Purpose:
##--------------------------------------  
getAccuracy(testSet, predictions):
