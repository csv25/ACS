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


########### SMOTE+TOMEK
# Apply SMOTE (Synthetic Minority Over-sampling Technique)

smote_result <- SMOTE(X, Y, K = 3)
typeof(smote_result)
trainData_SMOTE <- smote_result$data
dim(trainData_SMOTE)
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
#Dimentions of the class variable after SMOTE
table(trainData_SMOTE$Class) 


set.seed(123)# Ensure reproducibility
library(themis)

trainData_tomek <- recipe(Class ~ ., data = trainData_SMOTE) %>%
  step_tomek(Class) %>%
  prep() %>%
  bake(new_data = NULL)

trainData_tomek
length(trainData_tomek)
dim(trainData_tomek)
table(trainData_tomek$Class)

## ROSE  
#install.packages("ROSE")
library(ROSE)  
trainData_SMOTE.and.Tomek <- ovun.sample(Class ~ ., data = trainData_tomek, method = "under", N = sum(trainData_SMOTE$Class == 1) * 2, seed = 123)$data
table(trainData_SMOTE.and.Tomek$Class) 

set.seed(123)
rfe_control <- rfeControl(functions = rfFuncs, method = "boot", number = 5, returnResamp = 'final', verbose = TRUE, )
#rfFuncs:Uses random forests method of assessing the mean decrease in accuracy over 
#the features of interest i.e. the x (independent variables) and through the recursive 
#nature of the algorithm looks at which IVs have the largest affect on the mean decrease 
#in accuracy for the predicted y. The algorithm then purges the features with a low feature 
#importance, those that have little effect on changing this accuracy metric.
#cv: Cross Validation
# Number = 5 Fold Cross Validation

rfe_model <- rfe(trainData_SMOTE.and.Tomek[, -ncol(trainData_SMOTE.and.Tomek)], trainData_SMOTE.and.Tomek$Class,
                 sizes = 2^(2:5), rfeControl = rfe_control, metric = ifelse(is.factor(trainData_SMOTE.and.Tomek$Class), "Accuracy", "Kappa"))

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




###########: Trying SVM 
# 
# tune_result <- tune.svm(Class ~ ., 
#                         data= trainData_SMOTE.and.Tomek[, c(selected_features_rfe, "Class")],
#                         kernel = "radial",
#                         cost = 10^(-1:3),  # 0.1, 1, 10, 100,
#                         gamma = c(0.001,0.01, 0.1, 1, 10, 100))
# 
# tune_result$best.model
# tune_result$best.parameters
# tune_result$best.performance
# tune_result$performances
# tune_result$best.svm

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                 kernel = "radial",
                 gamma = c(0.001,0.01, 0.1, 1, 10),
                 cost = c(0.1, 1, 10),
                 class.weights = c("0"=1, "1"=1.5),
                 probability = TRUE)


print(svm_model)



svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

