

##--------------------------------------
## Name: is_number
## Input: data frame
## Output: if a the column is numeric
## Purpose: to check if a column is numeric
## Improvements: 
##--------------------------------------  
is_number =function(df){
  
  result<- unlist(lapply(df, is.numeric))
  return(result)
}

df1 = c(1,2,4,NA)
is_number(df1)
