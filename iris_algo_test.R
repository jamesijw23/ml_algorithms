library(dplyr)
library(modelr)
library(class)
## testing R's: Logistic Regression, KNN, CART, Random Forest, XGBoost

## Only 2 outcomes data set
iris_mod = iris %>%
  filter(Species == "versicolor" | Species == "virginica")
iris_mod$Species = droplevels(iris_mod$Species)

## Partition Data set
p_test = 0.25
partitions = resample_partition(iris_mod,c(part0 = 1 - p_test,
                                     part1 = p_test))
train_df = as.data.frame(partitions$part0)
test_df  = as.data.frame(partitions$part1)
train_true = ifelse(train_df[,5] == "versicolor",0,1)
test_true = ifelse(test_df[,5] == "versicolor",0,1)


#################################
## Logistic Regression
#################################
log_model = glm(Species ~., data = train_df ,family="binomial")
## Predict for logistic regression
est_log = ifelse(predict(log_model, test_df[,-5], type = "response")>0.5,1,0)
## Confusion matrix for logistic regression
table(test_true,est_log)



#################################
## KNN
#################################
est_knn = knn(train_df[,-5],test_df[,-5],cl = train_df[,5])
## Confusion matrix for KNN
table(test_df[,5],est_knn)
