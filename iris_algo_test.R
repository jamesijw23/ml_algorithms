library(dplyr)   ## Data Manipulation
library(modelr)  ## Partition Data set
library(class)   ## KNN
library(rpart)   ## CART
library(randomForest) ## Random Forest
library(xgboost)

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


#################################
## CART
#################################
cart_model = rpart(Species ~. , method="class", data = train_df)
## Predict for KNN
result_cart_test = predict(cart_model,test_df[,-5])
result_cart_test = apply(result_cart_test,1,which.max)
est_cart = ifelse(result_cart_test == 1, "versicolor", "virginica")
## Confusion matrix for CART
table(test_df[,5],est_cart)



#################################
## Random Forest
#################################
rf_model = randomForest(as.factor(Species) ~.,
                    data=train_df, 
                    importance=TRUE, 
                    ntree=2000)
## Predict for logistic regression
est_rf = predict(rf_model,test_df[-5])
## Confusion matrix for Random Forest
table(test_df[,5],est_rf)


#################################
## XGBoost
#################################
## Preprocessing Data
x_train = as.matrix(train_df[, 1:4])
y_train = as.numeric(factor(train_df[, 5]))-1

x_test = as.matrix(test_df[, 1:4])
y_test = as.numeric(factor(test_df[, 5]))-1
## Run Model
xgb_model <- xgboost(data = x_train, label = y_train, nrounds = 10)
## Predict for logistic regression
est_xgb = ifelse(predict(xgb_model,x_test)>0.5,1,0)
## Confusion matrix for logistic regression
table(test_true,est_xgb)
