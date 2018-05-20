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
## Name: abosluteDistance
## Input: 
## 1) instance1 --> 1st vector of data
## 2) instance1 --> 2nd vector of data
## 3) length --> vector length
## Output: absolute of distance
## Purpose: Ability to find absolute distance between
## two vectors
## Improvements: Do not use for loop
##--------------------------------------
absoluteDistance = function(instance1, instance2, length){
  distance = 0
  for(i in 1:length){
    distance = distance + abs(instance1[i] - instance2[i])
  }
  return(distance)
}
## NOTE: More distance can be found
## https://numerics.mathdotnet.com/distance.html


##--------------------------------------
## Name: getNeighbors
## Input: 
## 1) trainingSet --> training df
## 2) testInstance --> row of test df
## 3) k --> number of neighbors
## 4) type_distance --> "euclid" or "absolute"
## Output: Labels of training df that has the shortest 
## distance between test row
## Purpose: To be able to find the label of shortest distance
## of test instance
## Improvements:  no for loops in general
##--------------------------------------  
getNeighbors = function(trainingSet, 
                       testInstance, 
                       k, 
                       type_distance="euclid"){
  ## State parameters for getting Neighbors
  distances = vector()
  len_inst = length(testInstance)-1
  
  ## Find all distances from training set to 1 instance of test (wow)
  if(type_distance == "euclid"){
    for(i in 1:nrow(trainingSet)){
      dist = euclideanDistance(testInstance, trainingSet[i,], len_inst)
      distances =rbind(distances,data.frame(trainingSet[i,], dist))
    }
  } else if (type_distance == "absolute"){
    for(i in 1:nrow(trainingSet)){
      dist = absoluteDistance(testInstance, trainingSet[i,], len_inst)
      distances =rbind(distances,data.frame(trainingSet[i,],dist))
    }
  }
  colnames(distances)[ncol(distances)] = 'distance'
  distances = distances %>% arrange(distance)
  
  
  ## Obtain k neighbors  
  neighbors = distances[1:k,]

  return(neighbors)
}


##--------------------------------------
## Name:
## Input: 
## Output:
## Purpose:
## Improvements:
##--------------------------------------  
getResponse(neighbors):
  
  
  ##--------------------------------------
## Name:
## Input: 
## Output:
## Purpose:
## Improvements:
##--------------------------------------  
getAccuracy(testSet, predictions):
  