##--------------------------------------
## Name: metrics
## Input: 
## 1) true_vector --> true classifications
## 2) esti_vector --> esitmated classifications
## Output: 
  ## a) Classification Accuracy
  ## b) Confusion Matrix
  ## c) Logaritmic Loss
  ## d) Area Under Curve
  ## e) F1 Score
  ## f) F2 Score
  ## g) Mean Absolute Error
  ## h) Mean Square Error
## Purpose: Measure the accuracy of matrix
## Improvements: 
## a)
##--------------------------------------
metrics = function(true_vector,esti_vector){
  
  ## Classification Accuracy
  ca_error 
  ## Logaritmic Loss
  ll_error
  ## Confusion Matrix
  cm_error
  ## Area Under Curve
  auc_error
  ## F1 Score
  f1_error
  ## F2 Score
  f2_error
  ## Mean Absolute Error
  ma_error
  ## Mean Square Error
  ms_error
  
  results = list(## Classification Accuracy
    ca_error=ca_error,ll_error=ll_error,cm_error=cm_error,auc_error=auc_error,
    f1_error=f1_error,f2_error=f2_error,ma_error=ma_error,ms_error=ms_error)
  return(results)
}