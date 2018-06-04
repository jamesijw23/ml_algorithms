







##--------------------------------------
## Name: unique_vals
## Input: 
  ## a) df--> Data Frame
  ## b) col --> Column number
## Output: Unique levels or numbers 
## Purpose: Find the unique values for a column in a dataset.
## Improvements: 
  ## a) Less in house Functions
##--------------------------------------
unique_vals = function(df,col){
  vals = as.character(unique(df[,col]))
  vals = gsub("\\s", "", vals)
  return(vals)
}
unique_vals(df,4)
