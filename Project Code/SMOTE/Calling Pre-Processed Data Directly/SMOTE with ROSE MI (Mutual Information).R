getwd()
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

df3 <- read.csv("pre_processed_data.csv", header=TRUE, sep = ",")
head(df)

set.seed(123)

trainIndex <- createDataPartition(df3$Class, p = 0.7, list = FALSE)
trainData <- df3[trainIndex, ]
testData <- df3[-trainIndex, ]

print(table(trainData$Class))
plot(trainData$Class)
print(table(testData$Class)) 
plot(testData$Class)

##### ##### SCALING AND NORMALIZING 

summary(testData)
summary(trainData) 
str(trainData)
dim(trainData)
dim(testData)
#Making all varibles numeric except class
trainData <- trainData %>% mutate(across(-Class, as.numeric))
#finds elements in traindata that are not in the variable class
var_numeric_train <- setdiff(names(trainData), "Class")
print(var_numeric_train)
#scaling numerical variables that are in the trainset at placing them in the train data
trainData[var_numeric_train] <- scale(trainData[var_numeric_train]) 

#Placing all the test data as numeric so it matches the train data structure
testData <- testData %>% mutate(across(-Class, as.numeric))
var_numeric_test <- setdiff(names(trainData), "Class") 
testData[var_numeric_test] <- scale(testData[var_numeric_test]) 

#Placing the class variable as a factor
trainData$Class <- as.factor(trainData$Class)
testData$Class <- as.factor(testData$Class)

summary(testData)
summary(trainData)
str(trainData)
dim(trainData)
dim(testData)

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

# A vector of a target class attribute corresponding to the train dataset X
Y <- trainData$Class
# We will use this Y later 
Y <- trainData$Class

#SMOTE
library(smotefamily) 
borderline_smote_result <- BLSMOTE(X, Y, K = 5, C = 5, dupSize = 0, method = "type1")
trainData_SMOTE <- borderline_smote_result$data
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
table(trainData_SMOTE$Class)

## ROSE  
#install.packages("ROSE")
library(ROSE)  
trainData_SMOTE <- ovun.sample(Class ~ ., data = trainData_SMOTE, method = "under", N = sum(trainData_SMOTE$Class == 1) * 2, seed = 123)$data
table(trainData_SMOTE$Class) 

##### ##### ##### ##### ##### #####  MI (Mutual Information)
install.packages("FSelectorRcpp")
library(FSelectorRcpp)  

set.seed(123)
mi_scores <- information_gain(Class ~ ., trainData_SMOTE)  
mi_scores <- mi_scores[order(-mi_scores$importance), ]  

selected_features_mi <- head(mi_scores$attributes, 15)  
print(selected_features_mi)

testData <- testData %>% mutate(across(-Class, as.numeric))
testData[var_numeric_test] <- scale(testData[var_numeric_test])
testData_selected <- testData[, c(selected_features_mi, "Class")] 

##### #####  4.  SUPPORT VECTOR MACHINE 
library(e1071)

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_mi, "Class")], 
                 kernel = "radial",
                 gamma = c(0.001,0.01, 0.1, 1),
                 cost = c(0.1, 1, 10, 100),
                 class.weights = c("0"=1, "1"=1.2),
                 probability = TRUE)


print(svm_model)


svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_mi, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

##### #####  5.  NAIVE BAYES 
library(e1071)

# # 1. 
# nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_mi, "Class")])

# # 2.
# nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_mi, "Class")], usekernel = TRUE)
# 
# 3.
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_mi, "Class")], 
                       laplace = c(0, 0.5, 1, 2),
                       usekernel = TRUE,
                       )

print(nb_model)

nb_train_predictions <- predict(nb_model, trainData_SMOTE[, c(selected_features_mi, "Class")], type = "class")
nb_train_cm <- confusionMatrix(nb_train_predictions, trainData_SMOTE$Class)
nb_train_cm

nb_test_predictions <- predict(nb_model, testData_selected)
nb_test_cm <- confusionMatrix(nb_test_predictions, testData_selected$Class)
nb_test_cm

##### #####  6.  NEURAL NETWORKS 
library(nnet)

nn_model <- nnet(Class ~ ., data = trainData_SMOTE[, c(selected_features_mi, "Class")], 
                 size = 17, decay = 0.00001, maxit = 2000, rang = 0.3)
print(nn_model)

nn_train_predictions <- predict(nn_model, trainData_SMOTE[, c(selected_features_mi, "Class")])
nn_train_predictions <- ifelse(nn_train_predictions > 0.5, 1, 0)  
nn_train_predictions <- as.factor(nn_train_predictions)  

nn_train_cm <- confusionMatrix(nn_train_predictions, trainData_SMOTE$Class)
nn_train_cm

nn_test_predictions <- predict(nn_model, testData_selected)
nn_test_predictions <- ifelse(nn_test_predictions > 0.5, 1, 0)
nn_test_predictions <- as.factor(nn_test_predictions)

nn_test_cm <- confusionMatrix(nn_test_predictions, testData_selected$Class)
nn_test_cm


