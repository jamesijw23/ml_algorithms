#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

//-----------------------------------
// Name: repbyrow
// Input: 
// a) v1 --> Vector of values
// b) num_row --> rnumber of rows repeat
// Output: Matrix of vector repeated by row
// Purpose: Repeat a vector into num_row times by rows
// Improvements: 
// a) No for loop 
//-----------------------------------
// [[Rcpp::export]]
arma::mat repbyrow(arma::vec v1,int num_col){
  arma::mat matrix_v1(v1.size(),num_col);
  for(int i=0;i<num_col;i++){
    matrix_v1.col(i) = v1;
  }
  return arma::trans(matrix_v1);
}


//-----------------------------------
// Name: euclidean_dist_matrix
// Input:
// a) mat1 --> matrix of data
// b) v1 --> vector
// Output: Euclidean distance between vector and each row vector in matrix
// Purpose: Euclidean Distance of a test vector and train matrix (only features)
// Improvements:  
//
//-----------------------------------
// [[Rcpp::export]]
arma::colvec euclidean_dist_matrix(arma::mat mat1,arma::vec v1) {
  int num_rows = mat1.n_rows;
  arma::mat mat2 = repbyrow(v1,num_rows);
  arma::colvec sqrt_vec = sqrt(sum(pow(mat1 - mat2,2),1));
  return sqrt_vec;
}

//-----------------------------------
// Name: absolute_dist_matrix
// Input:
// a) mat1 --> matrix of data
// b) v1 --> vector
// Output: Absolute distance between vector and each row vector in matrix
// Purpose: Absolute Distance of a test vector and train matrix (only features)
// Improvements:  
//
//-----------------------------------
// [[Rcpp::export]]
arma::colvec absolute_dist_matrix(arma::mat mat1,arma::vec v1) {
  int num_rows = mat1.n_rows;
  arma::mat mat2 = repbyrow(v1,num_rows);
  arma::colvec abs_vec = sum(abs(mat1 - mat2),1);
  return abs_vec;
}



/*** R
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
  
  
  d = as.matrix(trainingSet[,-1]); colnames(d) <- NULL
  v = as.matrix(testInstance); colnames(v) <- NULL
  
## Find all distances from training set to 1 instance of test 
  if(type_distance == "euclid"){ ## Using Euclidean Distance
    dist_train = euclidean_dist_matrix(d,v)
  } else if (type_distance == "absolute"){ ## Using Absolute Distance
    dist_train = absolute_dist_matrix(d,v)
  }
  
  d_new = data.frame(distance = as.matrix(dist_train),trainingSet)
    d_new = d_new %>% arrange(distance)
## Obtain k neighbors  
    neighbors = d_new[1:k,]
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
  list_levels = data.frame(table(neighbors[,2]))  %>%
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
## Name: knn_cpp
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
knn_cpp = function(train_df, test_df, k, distance='euclid'){
## Determine estimated responses
  est_vector= vector()
  
## Determine if the the test df has reponse
  if(ncol(train_df) == ncol(test_df)){
    
## Test df and true response values
    true_values = test_df[,1]
    test_df = test_df[,-1]
    
    
    
    for (i in 1:nrow(test_df)){
      neighbors = getNeighbors(train_df,test_df[i,],k, type_distance=distance)
      result = getResponse(neighbors)
      est_vector = rbind(est_vector,result)
    } 
## Calculate accuracy of estimation
    p_e = getAccuracy(as.vector(est_vector), as.vector(true_values))
      
      
      return(list(predictions = est_vector, percent_error = p_e, type_distance = distance ))
  } else {
    
    for (i in 1:nrow(test_df)){
      neighbors = getNeighbors(train_df,test_df[i,],k, type_distance=distance)
      result = getResponse(neighbors)
      est_vector= rbind(est_vector,result)
    } 
    return(list(predictions = est_vector))
  }
}



*/
