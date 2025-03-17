###### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)

# ###################################################################
# Start of the analysis
# Get content into a data frame
df <- read.csv("project_data.csv", header=TRUE, sep = ",")

#Method 1: Remove irrelevant attributes 
#(e.g., tuple id, sample date)
# Getting number of original columns
columns <- ncol(df)
cat("Number of original columns:", columns, "\n")

# Remove specific columns
df <- subset(df, select = -c(RT, 
                             SERIALNO,
                             DIVISION, 
                             REGION, 
                             STATE, 
                             ADJINC,RACNH))


# Get number of columns in the new dataframe
columns_2 <- ncol(df)
cat("Number of columns in the new df:", "\n")
dim(df)
#Method 2: -	Remove columns that have more than 50% of ther values missing

missing_counts <- colSums(is.na(df))
sort(missing_counts, decreasing=TRUE)

#creating threshold of 50% 
threshold <- nrow(df)/2
threshold

#removing columns that do not meet the threshold
df <- df[,missing_counts <= threshold]
columns_3 <- ncol(df)
cat("Number of columns in the new df:", columns_3, "\n")
dim(df)

sorted <- sort(colSums(is.na(df)), decreasing=TRUE)
# sorted

# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)


# #placing the variables as numeric
# df$Class <- ifelse(df$Class == "Yes", 1,0)
# str(df)
# numericData <- df %>% mutate(across(-Class, as.numeric))
# var_numeric_data <- setdiff(names(numericData), "Class") 
# numericData[var_numeric_data] <- scale(numericData[var_numeric_data]) 
# 
# dim(numericData)
# str(numericData)
# 
# numericData %>% 
#   mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))
# 
# str(numericData)
# 
# 
# #impute median
# df_median <- impute_median(numericData)
# dim(df_median)
# 
# 
# df_median$Class <- as.factor(numericData$Class)
# str(df_median)

# Convert Class variable to numeric binary
df$Class <- ifelse(df$Class == "Yes", 1, 0)

# Convert all other columns to numeric
numericData <- df %>%
  mutate(across(-Class, as.numeric))

# Get numeric column names except "Class"
var_numeric_data <- setdiff(names(numericData), "Class")

# Scale numeric columns
numericData[var_numeric_data] <- scale(numericData[var_numeric_data])

# Fill NAs with median and store the result
numericData <- numericData %>%
  mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))

# Check structure and dimensions
dim(numericData)
str(numericData)
#Make class a factor so we can use in our ML algorithms to 
#test precision
numericData$Class <- factor(numericData$Class)
is.factor(numericData$Class)
str(numericData)
typeof(numericData)

#breaking the data into training and testing
df3 <- as.data.frame(numericData)
typeof(df3)

is.na(df3)



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

smote_result <- SMOTE(X, Y, K = 3)
typeof(smote_result)
trainData_SMOTE <- smote_result$data
dim(trainData_SMOTE)
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
#Dimentions of the class variable after SMOTE
table(trainData_SMOTE$Class) 

## ROSE  
#install.packages("ROSE")
library(ROSE)  
trainData_SMOTE <- ovun.sample(Class ~ ., data = trainData_SMOTE, method = "under", N = sum(trainData_SMOTE$Class == 1) * 2, seed = 123)$data
table(trainData_SMOTE$Class) 

##### ##### ##### ##### ##### #####  RFE
#Recursive Feature Elimination: This selects the 
#most important features from the balanced data set created by
#SMOTE. This process happens recursively. 

set.seed(123)
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 5, returnResamp = 'final', verbose = TRUE, )
#rfFuncs:Uses random forests method of assessing the mean decrease in accuracy over 
#the features of interest i.e. the x (independent variables) and through the recursive 
#nature of the algorithm looks at which IVs have the largest affect on the mean decrease 
#in accuracy for the predicted y. The algorithm then purges the features with a low feature 
#importance, those that have little effect on changing this accuracy metric.
#cv: Cross Validation
# Number = 5 Fold Cross Validation

rfe_model <- rfe(trainData_SMOTE[, -ncol(trainData_SMOTE)], trainData_SMOTE$Class,
                 sizes = 2^(2:5), rfeControl = rfe_control, metric = ifelse(is.factor(trainData_SMOTE$Class), "Accuracy", "Kappa"))

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

table(trainData_SMOTE$Class)
varImpPlot(rf_model)

varImp(rf_model)

low_importance_vars <- rownames(varImp(rf_model))[varImp(rf_model)[, 1] < 20]
print(low_importance_vars)

# Remove low-importance variables from training data
selected_features <- setdiff(names(trainData_SMOTE), low_importance_vars)

# Keep only selected features in training data
trainData_filtered <- trainData_SMOTE[, c(selected_features, "Class")]

# Check new dimensions
dim(trainData_filtered)



library(randomForest)
#### random forest

rf_model <- randomForest(Class ~ ., data = trainData_filtered,
                         ntree = 1500, 
                         importance = TRUE, 
                         classwt = c(1, 10),
                         maxnodes = 300,
                         replace = FALSE,
                         nodesize = 10)
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

#let's tune our svm with a search grid 
# 
# tune_result <- tune.svm(Class ~ ., 
#                         data= trainData_SMOTE[, c(selected_features_rfe, "Class")],
#                         kernel = "radial",
#                         cost = c(0.1, 1, 10, 50, 100), 
#                         gamma = c(0.0001, 0.001, 0.01, 0.1))
# 
# tune_result$best.model
# tune_result$best.parameters
# tune_result$best.performance
# tune_result$performances
# tune_result$best.svm
# 
# 
# print(best_model)
# 




svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                 kernel = "radial",
                 gamma = c(0.001,0.01, 0.1),
                 cost = c(0.1, 1, 10, 100,150),
                 class.weights = c("0"=1, "1"=1.2),
                 probability = TRUE)


print(svm_model)



# svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
#                  kernel = "radial", 
#                  cost = 1, 
#                  probability = TRUE)


print(svm_model)

svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm


