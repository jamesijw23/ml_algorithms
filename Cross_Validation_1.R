library(readr)
library(modelr)


##### Cross Validation





## b) Find Metrics


##### Type 2: Divide the Data into 10 sets 

## a) Split Data based on function (look at shipt)


## b) Find Metrics based on doing analysis on 90% and tesing 10%


## c) Plot Metric for each sample removed


cross_validation = function(df,type_cv = 1,p_test=0.2,method = "KNN"){
  
  ##### Type 1: Divide the Data into 2 sets (80/20)
  if(type_cv == 1){
    
    ## a) Partition df
    partitions = resample_partition(df,c(part0 = 1 - p_test,
                                         part1 = p_test))
    train_df = as.data.frame(partitions$part0)
    test_df  = as.data.frame(partitions$part1)
    
  
    ## b) Implement ML Algorithm
    
    
    ## c) Find Metrics
    
    
    ## d) Find Plots
    
    
    
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
    ## train_df_wo0, p0
    ## train_df_wo1, p1
    ## train_df_wo2, p2
    ## train_df_wo3, p3
    ## train_df_wo4, p4
    ## train_df_wo5, p5
    ## train_df_wo6, p6
    ## train_df_wo7, p7
    ## train_df_wo8, p8
    ## train_df_wo9, p9
    
    
    
    
    ## c) Find Metrics
    
    
    ## d) Find Plots
    
    
    
    
    
  }else{
    cat('Cross Validation Not Setup')
  }
}




##
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

