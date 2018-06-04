


##--------------------------------------
## Notes:
## a) Data Frame needs column name used to depict response varible
## needs be named Label
##--------------------------------------




##--------------------------------------
## Name: unique_vals
## Input: 
  ## a) df--> Data Frame
  ## b) col --> Column number
## Output: Unique levels or numbers 
## Purpose: Find the unique values for a column in a dataset.
## Improvements: 
  ## a) Less in R functions
##--------------------------------------
unique_vals = function(df,col){
  vals = as.character(unique(df[,col]))
  vals = gsub("\\s", "", vals)
  return(vals)
}
unique_vals(df,4)


##--------------------------------------
## Name: class_counts
## Input: 
  ## a) df--> Data Frame
## Output: A df with column 1 is name column 2 freq.
## Purpose: Counts the number of each type of label in a dataset.
## Improvements: 
  ## a) Less in R functions
##--------------------------------------
class_counts = function(df){
  return(data.frame(table(df$Label)))
}
##--------------------------------------

##--------------------------------------
## Name: is_numeric
## Input: 
  ## a) val --> a string or number
## Output: bool
## Purpose: Test if a value is numeric.
## Improvements: 
  ## a) Less in R functions
##--------------------------------------
is_numeric = function(val){
  return(is.numeric(val))
}