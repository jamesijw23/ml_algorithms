

##--------------------------------------
## Name: is_number
## Input: df_c --> vector
## Output: T/F
## Purpose: to check if a column is numeric
## Improvements: 
## a) Make sure this function is doing what it is supposed to do
## compared to python code
## b) This function subsumes is_number_value function from python
##--------------------------------------  
is_number = function(df_c){
  result<- sum(unlist(lapply(df_c, is.numeric))) == length(df_c)
  return(result)
}




# Function: Performs a Chi_Squared Test or Fisher Exact Test 
##--------------------------------------
## Name: chi_squared_test
## Input: 
## a) label_df --> Classification df
## b) feature_df --> features df
## Output: T/F
## Purpose: to check if a column is numeric
## Improvements: 
##--------------------------------------
chi_squared_test = function(label_df, feature_df){
  
 
  return(p_value)
}