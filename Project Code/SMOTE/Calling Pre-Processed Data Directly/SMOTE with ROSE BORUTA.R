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
borderline_smote_result <- BLSMOTE(X, Y, K = 3, C = 10, dupSize = 0, method = "type1")
trainData_SMOTE <- borderline_smote_result$data
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
table(trainData_SMOTE$Class)

## ROSE  
#install.packages("ROSE")
library(ROSE)  
trainData_SMOTE <- ovun.sample(Class ~ ., data = trainData_SMOTE, method = "under", N = sum(trainData_SMOTE$Class == 1) * 2, seed = 123)$data
table(trainData_SMOTE$Class) 

##### ##### ##### ##### ##### #####  BORUTA  
library(Boruta)
set.seed(123)

boruta_model <- Boruta(Class ~ ., data = trainData_SMOTE, doTrace = 0)
selected_features_boruta <- getSelectedAttributes(boruta_model, withTentative = FALSE)
print(selected_features_boruta)


testData <- testData %>% mutate(across(-Class, as.numeric))
testData[var_numeric_test] <- scale(testData[var_numeric_test])
testData_selected <- testData[, c(selected_features_boruta, "Class")] 
length(testData_selected)

##### #####  2. LOGISTIC REGRESSION 

# 1. Best-accuracy : 
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")],
                  method = "glm",
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5))
# 2. 
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                  method = "glm", 
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5),
                  weights = ifelse(trainData_SMOTE$Class == 0, 1.5, 1)) 
# 3. 
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                  method = "glm", 
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5),
                  weights = ifelse(trainData_SMOTE$Class == 1, 2, 1))

varImp(lr_model)

print(lr_model)
lr_train_predictions <- predict(lr_model, trainData_SMOTE[, c(selected_features_boruta, "Class")])
lr_train_cm <- confusionMatrix(lr_train_predictions, trainData_SMOTE$Class)
lr_train_cm 

lr_test_predictions <- predict(lr_model, testData_selected)
lr_test_cm <- confusionMatrix(lr_test_predictions, testData_selected$Class)
lr_test_cm 

# Replacing the threshold :   
lr_test_probabilities <- predict(lr_model, testData_selected, type = "prob")
lr_test_predictions <- ifelse(lr_test_probabilities[,2] > 0.45, 1, 0)  
lr_test_predictions <- as.factor(lr_test_predictions)

lr_test_cm <- confusionMatrix(lr_test_predictions, testData_selected$Class)
lr_test_cm 


##### #####  4.  SUPPORT VECTOR MACHINE 
library(e1071)

# tuned_svm <- tune(svm, 
#                   Species ~ ., 
#                   data = trainData,
#                   kernel = "radial",
#                   ranges = list(cost = 10^(-1:3),  # 0.1, 1, 10, 100
#                                 gamma = c(0.001,0.01, 0.1, 1, 10, 100, 500)))

tune_result <- tune.svm(Class ~ ., 
                        data= trainData_SMOTE[, c(selected_features_boruta, "Class")],
                        kernel = "radial",
                        cost = 10^(-1:3),  # 0.1, 1, 10, 100,
                        gamma = c(0.001,0.01, 0.1, 1, 10, 100, 500))

tune_result$best.model
tune_result$best.parameters
tune_result$best.performance
tune_result$performances
tune_result$best.svm

# Print best parameters
print(tuned_svm$best.parameters)

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                 kernel = "radial",
                 gamma = c(0.001,0.01, 0.1, 1, 10, 100, 500),
                 cost = c(0.1, 1, 50, 100 , 150, 200),
                 class.weights = c("0"=1, "1"=1.2),
                 probability = TRUE)

print(svm_model)


svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_boruta, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm


