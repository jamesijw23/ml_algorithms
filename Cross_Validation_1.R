library(readr)
library(modelr)


##--------------------------------------
## Name: different_combination_df
## Input: 
## 1) 10 dfs    
## Output: 10 dfs 
## Purpose: Create dfs in which 1 df is lefted out to be test and rest is train set
## Improvements: 
## a) 
##--------------------------------------

different_combination_df = function(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9){
  ## Without df 0
  wo_0 = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
  ## Without df 1
  wo_1 = rbind(d0,d2,d3,d4,d5,d6,d7,d8,d9)
  ## Without df 2
  wo_2 = rbind(d1,d0,d3,d4,d5,d6,d7,d8,d9)
  ## Without df 3
  wo_3 = rbind(d1,d2,d0,d4,d5,d6,d7,d8,d9)
  ## Without df 4
  wo_4 = rbind(d1,d2,d3,d0,d5,d6,d7,d8,d9)
  ## Without df 5
  wo_5 = rbind(d1,d2,d3,d4,d0,d6,d7,d8,d9)
  ## Without df 6
  wo_6 = rbind(d1,d2,d3,d4,d5,d0,d7,d8,d9)
  ## Without df 7
  wo_7 = rbind(d1,d2,d3,d4,d5,d6,d0,d8,d9)
  ## Without df 8
  wo_8 = rbind(d1,d2,d3,d4,d5,d6,d7,d0,d9)
  ## Without df 9
  wo_9 = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d0)
  dfs = list(wo_0,wo_1,wo_2,wo_3,wo_4,
             wo_5,wo_6,wo_7,wo_8,wo_9)
  
  return(dfs)
}





##--------------------------------------
## Name: cross_validation
## Input: 
  ## 1) df --> dataframe
  ## 2) type_cv --> type of cv 1: 2-fold 2: 10-fold 
  ## 3) p_test --> percent of df to be the test
  ## 4) method: machine learning technique on data
  ## 5)   
  ## 6) 
## Output: metrics and plots
## Purpose: To validate model
## Improvements: 
## a) 
##--------------------------------------

cross_validation_KNN = function(df,type_cv = 1,p_test=0.2,KN = 3){
  
  ##### Type 1: Divide the Data into 2 sets (80/20)
  if(type_cv == 1){
    
    ## a) Partition df
    partitions = resample_partition(df,c(part0 = 1 - p_test,
                                         part1 = p_test))
    train_df = as.data.frame(partitions$part0)
    test_df  = as.data.frame(partitions$part1)
    
  
    ## b) Implement ML Algorithm
    knn_model = knn_cpp(train_df, test_df, k=KN, distance='euclid')
    
    ## c) Find Metrics
    tr = test_df[,1]
    es = knn_model$predictions
    m = metrics(tr,es)
   
    
    ## d) Find Plots
    long_m = gather(m,metrics_name,metric_value,TPR_metric:MKd_metric)
    few_metric = long_m %>% 
      filter(metrics_name == "TPR_metric" |
               metrics_name == "TNR_metric" |
               metrics_name == "ACC_metric" | 
               metrics_name == "PPV_metric")
    ggplot(few_metric,aes(x=metrics_name,y=metric_value)) + 
      ggtitle("Recovery of Each Level") +
      geom_point()+
      xlab("Metrics Names") +
      ylab("Measurements") +
      facet_grid(~Levels_name) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    
    
    
  } else if(type_cv == 2){
    grouping_df = resample_partition(df,c(p0 = 0.1,
                                         p1 = 0.1,
                                         p2 = 0.1,
                                         p3 = 0.1,
                                         p4 = 0.1,
                                         p5 = 0.1,
                                         p6 = 0.1,
                                         p7 = 0.1,
                                         p8 = 0.1,
                                         p9 = 0.1))
    
    ## Split Data Frames
    p0 = as.data.frame(grouping_df$p0)
    p1 = as.data.frame(grouping_df$p1)
    p2 = as.data.frame(grouping_df$p2)
    p3 = as.data.frame(grouping_df$p3)
    p4 = as.data.frame(grouping_df$p4)
    p5 = as.data.frame(grouping_df$p5)
    p6 = as.data.frame(grouping_df$p6)
    p7 = as.data.frame(grouping_df$p7)
    p8 = as.data.frame(grouping_df$p8)
    p9 = as.data.frame(grouping_df$p9)
    
    all_dfs = different_combination_df(p0,p1,p2,p3,p4,
                             p5,p6,p7,p8,p9)
    train_df_wo0 =all_dfs[[1]];train_df_wo1 =all_dfs[[2]]
    train_df_wo2 =all_dfs[[3]];train_df_wo3 =all_dfs[[4]]
    train_df_wo4 =all_dfs[[5]];train_df_wo5 =all_dfs[[6]]
    train_df_wo6 =all_dfs[[7]];train_df_wo7 =all_dfs[[8]]
    train_df_wo8 =all_dfs[[9]];train_df_wo9 =all_dfs[[10]]
    
    
    ## b) Implement ML Algorithm
    
    ## train and test:
    knn_model_test0 = knn_cpp(train_df_wo0, p0, k=KN, distance='euclid')
    knn_model_test1 = knn_cpp(train_df_wo1, p1, k=KN, distance='euclid')
    knn_model_test2 = knn_cpp(train_df_wo2, p2, k=KN, distance='euclid')
    knn_model_test3 = knn_cpp(train_df_wo3, p3, k=KN, distance='euclid')
    knn_model_test4 = knn_cpp(train_df_wo4, p4, k=KN, distance='euclid')
    knn_model_test5 = knn_cpp(train_df_wo5, p5, k=KN, distance='euclid')
    knn_model_test6 = knn_cpp(train_df_wo6, p6, k=KN, distance='euclid')
    knn_model_test7 = knn_cpp(train_df_wo7, p7, k=KN, distance='euclid')
    knn_model_test8 = knn_cpp(train_df_wo8, p8, k=KN, distance='euclid')
    knn_model_test9 = knn_cpp(train_df_wo9, p9, k=KN, distance='euclid')
    
    
    
    ## c) Find Metrics
    tr0 = p0[,1];tr1 = p1[,1];tr2 = p2[,1];tr3 = p3[,1];tr4 = p4[,1];
    tr5 = p5[,1];tr6 = p6[,1];tr7 = p7[,1];tr8 = p8[,1];tr9 = p9[,1];
    es0= knn_model_test0$predictions;es1= knn_model_test1$predictions
    es2= knn_model_test2$predictions;es3= knn_model_test3$predictions
    es4= knn_model_test4$predictions;es5= knn_model_test5$predictions
    es6= knn_model_test6$predictions;es7= knn_model_test7$predictions
    es8= knn_model_test8$predictions;es9= knn_model_test9$predictions
    
    test_metrics0 = metrics(tr0,es0);test_metrics1 = metrics(tr1,es1)
    test_metrics2 = metrics(tr2,es2);test_metrics3 = metrics(tr3,es3)
    test_metrics4 = metrics(tr4,es4);test_metrics5 = metrics(tr5,es5)
    test_metrics6 = metrics(tr6,es6);test_metrics7 = metrics(tr7,es7)
    test_metrics8 = metrics(tr8,es8);test_metrics9 = metrics(tr9,es9)
    
    cv10_metrics_df = rbind(test_metrics0,test_metrics1,
                            test_metrics2,test_metrics3,
                            test_metrics4,test_metrics5,
                            test_metrics6,test_metrics7,
                            test_metrics8,test_metrics9)
    number_levels = length(unique(cv10_metrics_df$Levels_name))
    
    cv10_metrics_df$Fold_Number = sort(rep(seq(0,9),number_levels))  
    return(cv10_metrics_df)
    
    ## d) Find Plots
    
    
    
    
    
  }else{
    cat('Cross Validation Not Setup')
  }
}




