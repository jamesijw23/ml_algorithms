  # https://datawookie.netlify.com/blog/2015/12/making-sense-of-logarithmic-loss/
  # https://en.wikipedia.org/wiki/Confusion_matrix 
  
  ## 1) Logaritmic Loss: Another Function need probability matrix for each class
  ## 2) AUC   -->  http://rpubs.com/prcuny/161764 
  







##--------------------------------------
## Name: metrics_function
## Input: 
## 1) matrixM --> Confusion Matrix 
## Output: 
## a) Classification Accuracy
## s) Precision/Recall
## b) Confusion Matrix
## c) Logaritmic Loss
## d) Area Under Curve
## e) F1 Score
## f) F2 Score
## g) Mean Absolute Error
## h) Mean Square Error
## Purpose: Measure the accuracy of recovery of classification
## Improvements: 
## a)
##--------------------------------------
metrics_function = function(matrixM){
  ##---------------------------------------
  ## Condition positive (P) - The number of real positive cases in the data
  ##---------------------------------------
  P_cond = sum(matrixM[1,])
  ##---------------------------------------
  ## Condition negative (N) - The number of real negative cases in the data
  ##---------------------------------------
  N_cond = sum(matrixM[2,])
  ##---------------------------------------
  ## True positive (TP) - Eqv. with hit
  ##---------------------------------------
  TP_cond = matrixM[1,1]
  ##---------------------------------------
  ## True negative (TN) - Eqv. with correct rejection
  ##---------------------------------------
  TN_cond = matrixM[2,2]
  ##---------------------------------------
  ## False positive (FP) - Eqv. with false alarm, Type I error
  ##---------------------------------------
  FP_cond = matrixM[1,2]
  ##---------------------------------------
  ## False negative (FN) - Eqv. with miss, Type II error
  ##---------------------------------------
  FN_cond = matrixM[2,1]
  
  ## Metrics
  ## 1) Sensitivity/Recall/True Positive Rate
  TPR_metric = TP_cond / P_cond
  ## 2) SpecificityTrue Negative Rate
  TNR_metric = TN_cond / N_cond
  ## 3) Precision/ Positive Predictive Value
  PPV_metric = TP_cond / (TP_cond + FP_cond)
  ## 4) Negative Predictive value
  NPV_metric = TN_cond / (TN_cond + FN_cond)
  ## 5) False Negative Rate
  FNR_metric = FN_cond / P_cond
  ## 6) False Positive Rate
  FPR_metric = FP_cond / N_cond
  ## 7) False Discovery Rate
  FDR_metric = FP_cond / ( FP_cond + TP_cond)
  ## 8) False Omission Rate
  FOR_metric = FN_cond / (FN_cond + TN_cond)
  ## 9) Accuracy
  ACC_metric = (TP_cond + TN_cond)/ (P_cond + N_cond)
  ## 10) F1 Score
  F1s_metric = 2 *((PPV_metric*TPR_metric) / (PPV_metric + TPR_metric))
  ## 11) F2 Score
  F2s_metric =  5 *((PPV_metric*TPR_metric) / (4*PPV_metric + TPR_metric))
  
  ## 12) Matthews Correlation Cofficient
  MCC_metric = ((TP_cond * TN_cond) - (FP_cond * FN_cond)) / 
    sqrt((TP_cond+FP_cond)*(TP_cond+FN_cond)*(TN_cond+FP_cond)*(TN_cond+FN_cond))
  ## 13) Informedness 
  BMi_metric = TPR_metric + TNR_metric - 1
  
  ## 14) MarKedness
  MKd_metric = PPV_metric + NPV_metric - 1
  
  
  metrics = cbind(TPR_metric, TNR_metric, PPV_metric, NPV_metric,
                  FNR_metric, FPR_metric, FDR_metric, FOR_metric, 
                  ACC_metric, F1s_metric, F2s_metric, MCC_metric,
                  BMi_metric, MKd_metric)
  
  colnames(metrics) = c("TPR_metric", "TNR_metric", 
                        "PPV_metric", "NPV_metric",
                        "FNR_metric", "FPR_metric", 
                        "FDR_metric", "FOR_metric", 
                        "ACC_metric", "F1s_metric", 
                        "F2s_metric", "MCC_metric",
                        "BMi_metric", "MKd_metric")
  return(metrics)
}



##--------------------------------------
## Name: metrics
## Input: 
## 1) tr --> true classifications
## 2) es --> esitmated classifications
## Output: 
## a) metrics for each level
## Purpose: Display Metrics for each level of true classification
## Improvements: 
## a)
##--------------------------------------
metrics = function(tr,es){
  d = table(tr,es)
  
  ##---------------------------------------
  ## Obtain all Confusion matricies
  ##---------------------------------------
  all_confuse_matrix = list()
  for(i in 1:length(unique(tr))){
    ## Correct Classification True Positives
    TP_i = diag(d)[i]
    
    ## True Values misclassified
    FN_i = rowSums(d)[i] - TP_i
    
    
    ## False Values Classified
    FP_i = colSums(d)[i] - TP_i
    
    
    ## Correct Classification True Negative
    TN_i = sum(diag(d)[-i])
    
    confuse_matrix = matrix(c(TP_i, FP_i, FN_i, TN_i),2,2)
    all_confuse_matrix[i] = list(confuse_matrix)
  }
  ##---------------------------------------
  ## Obtain all metrics for each Confusion matricies
  ##---------------------------------------
  
  all_confuse_matrix_metrics = vector()
  for(i in 1:length(unique(tr))){
    metric_tmp = metrics_function(all_confuse_matrix[[i]])
    all_confuse_matrix_metrics = rbind(all_confuse_matrix_metrics,metric_tmp)
  }
  
  all_confuse_matrix_metrics = as.data.frame(all_confuse_matrix_metrics)
  all_confuse_matrix_metrics$Levels_name =  colnames(d)
  return(all_confuse_matrix_metrics)
}

