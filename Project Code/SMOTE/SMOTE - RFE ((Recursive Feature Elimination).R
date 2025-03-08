# Working directory - Setting/Getting
getwd()
# Set working directory
# setwd('/Users/melanieloaiza/Desktop/BU - Data Science /Spring 2025/MET CS699 - Data Mining/project_assignment')

getwd()
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

df <- read.csv("project_data.csv", header=TRUE, sep = ",")
head(df)

##### ##### ##### ##### DATA EXPLORATION :  
dim(df) 
str(df)

n_miss(df) 
prop_miss(df)
summary(df)  

##### ##### ##### ##### HANDLING MISSING VALUES :  
df1 <- df
miss_var_summary(df1) %>%
  arrange(desc(pct_miss)) %>%  
  slice_head(n = 50) %>%       
  print(n = 50)

gg_miss_var(df1) +
  theme_minimal() +  
  theme(axis.text.y = element_text(size = 5))  

threshold <- 0.50 * nrow(df1)
df1 <- df1 %>% select(where(~ sum(is.na(.)) <= threshold))

dim(df1) 
summary(df1) 

df1 <- df1[, !names(df1) %in% c("SERIALNO", "SPORDER")]

dim(df1) 
summary(df1) 
str(df1)

n_miss(df1) 
prop_miss(df1) 
miss_var_summary(df1) %>% print(n = Inf)

##### ##### ##### ##### CHECKING FOR LOW VARIANCE :  
zerovar <- nearZeroVar(df1, names = TRUE) 
print(zerovar) 

df1 <- df1 %>% select(-all_of(zerovar))  

dim(df1)  
str(df1)
summary(df1)   
miss_var_summary(df1) %>% print(n = Inf) 

##### ##### ##### ##### IMPUTATION :  
df2 <- df1 

df2$INDP[is.na(df2$INDP)] <- 9920  
df2$OCCP[is.na(df2$OCCP)] <- 9920  

df2$NWAB[is.na(df2$NWAB)] <- "2"  
df2$NWLA[is.na(df2$NWLA)] <- "2"  
df2$NWLK[is.na(df2$NWLK)] <- "2" 

df2$COW[is.na(df2$COW)] <- "0"   
df2$WRK[is.na(df2$WRK)] <- "0"  
df2$ESR[is.na(df2$ESR)] <- "0" 
df2$MARHT[is.na(df2$MARHT)] <- "0"  
df2$JWTRNS[is.na(df2$JWTRNS)] <- "0"    

df2$WKHP <- ifelse(is.na(df2$WKHP), median(df2$WKHP, na.rm = TRUE), df2$WKHP)
df2$PERNP <- ifelse(is.na(df2$PERNP), median(df2$PERNP, na.rm = TRUE), df2$PERNP)
df2$POVPIP <- ifelse(is.na(df2$POVPIP), median(df2$POVPIP, na.rm = TRUE), df2$POVPIP) 

df2$MARHYP <- ifelse(is.na(df2$MARHYP), 0,  ifelse(df2$MARHYP <= 1944, 1, 2))  
df2$POWPUMA <- ifelse(is.na(df2$POWPUMA), 0,  ifelse(df2$POWPUMA == "00001", 1, 2)) 

n_miss(df2) 
prop_miss(df2) 
miss_var_summary(df2) %>% print(n = Inf)

#zerovar_2 <- nearZeroVar(df2, names = TRUE) 
#print(zerovar_2) 

#write.csv(df2, "data_1.csv", row.names = TRUE) 

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### MODEL TRAINING :  
df3 <- df2 
df3$Class <- ifelse(df3$Class == "Yes", 1, 0)
df3$Class <- as.factor(df3$Class)
table(df3$Class)

set.seed(123)

trainIndex <- createDataPartition(df3$Class, p = 0.7, list = FALSE)
trainData <- df3[trainIndex, ]
testData <- df3[-trainIndex, ]

print(table(trainData$Class))
print(table(testData$Class)) 

##### ##### SCALING AND NORMALIZING 

summary(testData)
summary(trainData) 
str(trainData)
dim(trainData) 

trainData <- trainData %>% mutate(across(-Class, as.numeric))
var_numeric_train <- setdiff(names(trainData), "Class") 
trainData[var_numeric_train] <- scale(trainData[var_numeric_train]) 

testData <- testData %>% mutate(across(-Class, as.numeric))
var_numeric_test <- setdiff(names(trainData), "Class") 
testData[var_numeric_test] <- scale(testData[var_numeric_test]) 

trainData$Class <- as.factor(trainData$Class)
testData$Class <- as.factor(testData$Class)

summary(testData)
summary(trainData)
str(trainData)
dim(trainData)

n_miss(trainData) 
prop_miss(trainData) 

n_miss(testData) 
prop_miss(testData) 

##### ##### SMOTE AND FEATURE SELECTION :
#Synthetic Minority Oversampling Technique
#We want to use this method to address the class imbalance in our data 
#by generating random synthetic data points for our minority class.

library(smotefamily) 

# SMOTE
set.seed(123)
#Dividing dataframe between features

#data frame or matrix of numeric-attributed dataset
X <- trainData[, -which(names(trainData) == "Class")]
#Making the dataframe all numeric variables
X <- as.data.frame(lapply(X, as.numeric))

# A vector of a target class attribute corresponding to a dataset X
Y <- trainData$Class
# We will use this Y later 
Y <- trainData$Class

smote_result <- SMOTE(X, Y, K = 5)
typeof(smote_result)

trainData_SMOTE <- smote_result$data
dim(trainData_SMOTE)
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"

trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
#Dimentions of the class variable after SMOTE
table(trainData_SMOTE$Class) 

##### ##### ##### ##### ##### #####  RFE
#Recursive Feature Elimination: This selects the 
#most important features from the balanced data set created by
#SMOTE. This process happens recursively. 

set.seed(123)
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
#rfFuncs:Uses random forests method of assessing the mean decrease in accuracy over 
#the features of interest i.e. the x (independent variables) and through the recursive 
#nature of the algorithm looks at which IVs have the largest affect on the mean decrease 
#in accuracy for the predicted y. The algorithm then purges the features with a low feature 
#importance, those that have little effect on changing this accuracy metric.
#cv: Cross Validation
# Number = 5 Fold Cross Validation

rfe_model <- rfe(trainData_SMOTE[, -ncol(trainData_SMOTE)], trainData_SMOTE$Class,
                 sizes = c(5, 10, 15, 20), rfeControl = rfe_control)

# trainData_SMOTE[, -ncol(trainData_SMOTE)]: All classes except the last (class) 
# trainData_SMOTE$Class: Target Variable
# sizes = c(5, 10, 15, 20): Breaks it down by subset and chooses the most important features in that particular subset saves the variables
# rfeControl = rfe_control: Applies the control settings defied earlier (random forest + cv))

selected_features_rfe <- predictors(rfe_model) 
print(selected_features_rfe)
#Number of important variables:
length(selected_features_rfe)

#Converting all features into numeric type except "Class"
testData <- testData %>% mutate(across(-Class, as.numeric))
#Scaling data so they have a mean of 0 and a SD of 1
testData[var_numeric_test] <- scale(testData[var_numeric_test])
# We are creating new dataset that only contains features selected from RFE + target variable
testData_selected <- testData[, c(selected_features_rfe, "Class")] 

#print(testData_selected)
#dim(testData_selected)

##### #####  1. RANDOM FOREST 
library(randomForest)
#Hyper Parameters:
# ntree:Number of trees to grow. 
# This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.

# importance: Should importance of predictors be assessed?

# classwt: Priors of the classes. Need not add up to one. Ignored for regression.

# sampsize: Size(s) of sample to draw. 
# For classification, if sampsize is a vector of the length the number of strata, then sampling is stratified by strata, 
# and the elements of sampsize indicate the numbers to be drawn from the strata.

# Comment: I think we should remane our models here because everytime we run them they are just getting replaced
# by the previous one and we are losing our results.

# 1 . 
# rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
#                          ntree = 500,  importance = TRUE)

# 2.
# rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
#                          ntree = 1000, importance = TRUE, classwt = c(1, 5))
# 
# # 3. The best it gets for random forests with these parameters. 
# this might be the best accuracy we can get in random forest
rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                         ntree = 1500, 
                         importance = TRUE, 
                         classwt = c(1, 10),
                         maxnodes = 300,
                         replace = FALSE)
# # 

# 4 
# rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
#                          ntree = 2000, importance = TRUE, classwt = c(1, 15))
# 5.
# rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
#                          ntree = 1000, importance = TRUE, sampsize = c(500, 500))


print(rf_model)
rf_train_predictions <- predict(rf_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
rf_train_cm <- confusionMatrix(rf_train_predictions, trainData_SMOTE$Class)
rf_train_cm 

rf_test_predictions <- predict(rf_model, testData_selected)
rf_test_cm <- confusionMatrix(rf_test_predictions, testData_selected$Class)
rf_test_cm 

##### #####  2. LOGISTIC REGRESSION 

# 1. Best-accuracy : 
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                  method = "glm",
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5))
# 2. 
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                  method = "glm", 
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5),
                  weights = ifelse(trainData_SMOTE$Class == 0, 1.5, 1)) 
# 3. 
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                  method = "glm", 
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5),
                  weights = ifelse(trainData_SMOTE$Class == 1, 2, 1))

varImp(lr_model)

print(lr_model)
lr_train_predictions <- predict(lr_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
lr_train_cm <- confusionMatrix(lr_train_predictions, trainData_SMOTE$Class)
lr_train_cm 

lr_test_predictions <- predict(lr_model, testData_selected)
lr_test_cm <- confusionMatrix(lr_test_predictions, testData_selected$Class)
lr_test_cm 

# Replacing the threshold : 
# Steveen: What happened that we decided to replace the threshold?
lr_test_probabilities <- predict(lr_model, testData_selected, type = "prob")
lr_test_predictions <- ifelse(lr_test_probabilities[,2] > 0.45, 1, 0)  
lr_test_predictions <- as.factor(lr_test_predictions)

lr_test_cm <- confusionMatrix(lr_test_predictions, testData_selected$Class)
lr_test_cm 

library(pROC)

lr_test_probabilities <- predict(lr_model, testData_selected, type = "prob")
actual_classes <- as.numeric(testData_selected$Class)  
roc_curve <- roc(actual_classes, lr_test_probabilities[,2])  
plot(roc_curve, col="blue", lwd=2, main="ROC Curve for Logistic Regression")
abline(a=0, b=1, lty=2, col="red")  
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))


##### #####  3. BALANCED RANDOM FOREST 
#install.packages("UBL")

library(ranger)
library(UBL)

#balanced_smote <- SmoteClassif(Class ~ ., trainData, C.perc = "balance")
#balanced_tomek_result <- TomekClassif(Class ~ ., balanced_smote)
#balanced_data <- balanced_tomek_result[[1]]
#rf_balanced_model <- randomForest(Class ~ ., data = balanced_data, ntree = 1000)


# Steveen: Same thing here, I think we should rename the name of the rf_balanced_model


# 1. 
rf_balanced_model <- ranger(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                            num.trees = 500, class.weights = c(1, 2))

# 2. 
rf_balanced_model <- randomForest(Class ~ ., data = trainData_SMOTE, 
                                  ntree = 1000, importance = TRUE, sampsize = c(500, 500))  

# 3. 
rf_balanced_model <- randomForest(Class ~ ., 
                                  data = trainData_SMOTE,
                                  ntree = 1000,
                                  importance = TRUE,
                                  sampsize = c(700, 700))
# 4. 
rf_balanced_model <- randomForest(Class ~ ., 
                                  data = trainData_SMOTE,
                                  ntree = 1500,
                                  mtry = floor(sqrt(ncol(trainData_SMOTE)-1)), 
                                  nodesize = 1,
                                  importance = TRUE,
                                  sampsize = c(700,700))

print(rf_balanced_model)

rf_balanced_train_predictions <- predict(rf_balanced_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
rf_balanced_train_cm <- confusionMatrix(rf_balanced_train_predictions, trainData_SMOTE$Class)
rf_balanced_train_cm

rf_balanced_test_predictions <- predict(rf_balanced_model, testData_selected)
rf_balanced_test_cm <- confusionMatrix(rf_balanced_test_predictions, testData_selected$Class)
rf_balanced_test_cm 

##### #####  4.  SUPPORT VECTOR MACHINE 
# Documentation: https://www.rdocumentation.org/packages/e1071/versions/1.7-16/topics/svm
library(e1071)
#Different parameters to try: 

# Kernel: Linear, polynomial, radial, sigmoid
# gamma: parameter needed for all kernels except linear
# type: "C-classification" , "eps-regression", 'nu-classification','one-classification (for novelty detection)', 'eps-regression', 'nu-regression'
# deggree: parameter needed for kernel of type polynomial
# coef0: parameter needed for kernels of type polynomial and sigmoid (default: 0)
# class.weights: a named vector of weights for the different classes, used for asymmetric class sizes. 
# Not all factor levels have to be supplied (default weight: 1). All components have to be named. 
# Specifying "inverse" will choose the weights inversely proportional to the class distribution.
# quiero ver que podemos mejorar en los accuracy readings

# test

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                 kernel = "radial", 
                 cost = 1, 
                 probability = TRUE)

print(svm_model)

svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

##### #####  5.  NAIVE BAYES 
# Documentation: https://cran.r-project.org/web/packages/e1071/e1071.pdf

library(e1071)

# 1. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")])

# 2. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], usekernel = TRUE)

# 3. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], laplace = 1)

print(nb_model)

nb_train_predictions <- predict(nb_model, trainData_SMOTE[, c(selected_features_rfe, "Class")], type = "class")
nb_train_cm <- confusionMatrix(nb_train_predictions, trainData_SMOTE$Class)
nb_train_cm

nb_test_predictions <- predict(nb_model, testData_selected)
nb_test_cm <- confusionMatrix(nb_test_predictions, testData_selected$Class)
nb_test_cm

##### #####  6.  NEURAL NETWORKS
# Documentation: https://cran.r-project.org/web/packages/nnet/nnet.pdf

library(nnet)

nn_model <- nnet(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                 size = 10, decay = 0.01, maxit = 200)
print(nn_model)

nn_train_predictions <- predict(nn_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
nn_train_predictions <- ifelse(nn_train_predictions > 0.5, 1, 0)  
nn_train_predictions <- as.factor(nn_train_predictions)  

nn_train_cm <- confusionMatrix(nn_train_predictions, trainData_SMOTE$Class)
nn_train_cm

nn_test_predictions <- predict(nn_model, testData_selected)
nn_test_predictions <- ifelse(nn_test_predictions > 0.5, 1, 0)
nn_test_predictions <- as.factor(nn_test_predictions)

nn_test_cm <- confusionMatrix(nn_test_predictions, testData_selected$Class)
nn_test_cm

##### #####  7.  Decision Tree (CART)
library(rpart)

cart_model <- rpart(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                    method = "class", control = rpart.control(cp = 0.01))
print(cart_model)

cart_train_predictions <- predict(cart_model, trainData_SMOTE[, c(selected_features_rfe, "Class")], type = "class")
cart_train_cm <- confusionMatrix(cart_train_predictions, trainData_SMOTE$Class)
cart_train_cm

cart_test_predictions <- predict(cart_model, testData_selected, type = "class")
cart_test_cm <- confusionMatrix(cart_test_predictions, testData_selected$Class)
cart_test_cm

##### #####  8.  KNN
library(class)

knn_predictions <- knn(train = trainData_SMOTE[, selected_features_rfe], 
                       test = testData_selected[, selected_features_rfe], 
                       cl = trainData_SMOTE$Class, k = 5)

knn_test_cm <- confusionMatrix(knn_predictions, testData_selected$Class)
knn_test_cm

##### #####  9.  XGBoost (Extreme Gradient Boosting)
library(xgboost)

train_matrix <- xgb.DMatrix(data = as.matrix(trainData_SMOTE[, selected_features_rfe]), 
                            label = as.numeric(trainData_SMOTE$Class) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(testData_selected[, selected_features_rfe]))

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  scale_pos_weight = sum(trainData_SMOTE$Class == 0) / sum(trainData_SMOTE$Class == 1)
)

xgb_model <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 200)

xgb_test_predictions <- predict(xgb_model, test_matrix)
xgb_class <- ifelse(xgb_test_predictions > 0.5, 1, 0)

xgb_test_cm <- confusionMatrix(factor(xgb_class), testData_selected$Class)
xgb_test_cm