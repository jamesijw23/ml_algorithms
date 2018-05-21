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
## Improvements: 
## a) Not have to create & delete tmp row_num col
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
## Improvements: 
## a) Do not use for loop
## b) Create an algorithm that allows you to create a distance 
## based on the feature
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
## Improvements: 
## a) Do not use for loop
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
## Improvements:  
## a) No for loops in general
## b) Store distances into a df
##--------------------------------------  
getNeighbors = function(trainingSet, 
                        testInstance, 
                        k, 
                        type_distance="euclid"){
  ## State parameters for getting Neighbors
  distances = vector()
  num_features = ncol(trainingSet)-1
  
  ## Find all distances from training set to 1 instance of test 
  if(type_distance == "euclid"){ ## Using Euclidean Distance
    for(i in 1:nrow(trainingSet)){
      dist = euclideanDistance(testInstance, trainingSet[i,-1],
                               num_features)
      distances =rbind(distances,data.frame(trainingSet[i,], dist))
    }
  } else if (type_distance == "absolute"){ ## Using Absolute Distance
    for(i in 1:nrow(trainingSet)){
      dist = absoluteDistance(testInstance, trainingSet[i,-1],
                              num_features)
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
## Name: getResponse
## Input: Data of the neighbors
## Output: Classification of row from test
## Purpose: To be able to classify data based on nearest neighbors
## Improvements: 
## a) Determine an better algorithm for ties, could be random
##--------------------------------------  
getResponse = function(neighbors){
  ## Creates a table based on response variable 
  ## Make this table into a df
  list_levels = data.frame(table(neighbors[,1]))  %>%
    ## Arranges df based on Freq of the levels response var
    arrange(desc(Freq)) %>%
    ## Selects only variable of interest  
    select(Var1)
  ## Select first maximum
  result = as.character(list_levels[1,1])
  return(result)
}


##--------------------------------------
## Name: getAccuracy
## Input: 
  ## a) True Responses
  ## b) Estimates
## Output: 
  ## Percent Error
## Purpose: Be able to state how well the model did at predicting
## Improvements:
  ## a) Other Metrics for classification
  ## a) Determine what kind of response 
  ## b) Based type of response, do a type of measurement
  ## d) Plot Metrics
##--------------------------------------  
getAccuracy = function(est_values, true_values){
  ## Error Percent
  results = sum(est_values != true_values)/length(true_values)
  return(results)
}




##--------------------------------------
## Name: emans_knn
## Input: 
## a) train_df --> training df
## b) test_df --> testing df
## c) k --> number of neighbors
## d) distance --> type of distance
## Output:
## Purpose:
## Improvements:
## a) No For Loops
## NOTE:
## a) The response variable is always first
## b1) Test df will not have a response, if it does it should be
## removed for this version of KNN
## b2) If test df has the same dimensions as train getAccuracy will be
## used
## Parameters to Change:
## a) k: number of neighbors
## b) Type of Distance
## c) Amount of Test/Train data
## d) ** Handle of Ties, change choice based on MSE after done
##--------------------------------------
emans_knn = function(train_df, test_df, k, distance='euclid'){
  ## Determine if the the test df has reponse
  if(ncol(train_df) == ncol(test_df)){
    
    est_vector= vector()
    
    
    test_df = test_df[,-1]
    true_values = test_df[,1]
    
    
    for (i in 1:nrow(test_df)){
      neighbors = getNeighbors(train_df,test_df[i,],k, type_distance=distance)
      result = getResponse(neighbors)
      est_vector = rbind(est_vector,result)
    } 
    
    p_e = getAccuracy(as.vector(est_vector), as.vector(true_values))
    
    
     return(list(predictions = predictions_vector, percent_error = p_e ))
  } else {
    for (i in 1:nrow(test_df)){
      neighbors = getNeighbors(train_df,test_df[i,],k, type_distance=distance)
      result = getResponse(neighbors)
      predictions_vector = rbind(predictions_vector,result)
    } 
    return(list(predictions = predictions_vector))
  }
}



