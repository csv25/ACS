# Working directory - Setting/Getting
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
## Creating Threshold to remove columns who are missing more than 50% of their content
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


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### MODEL TRAINING :  
df3 <- df2 
df3$Class <- ifelse(df3$Class == "Yes", 1, 0)
df3$Class <- as.factor(df3$Class)
table(df3$Class)

#Setting seed to recreate results:
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

smote_result <- SMOTE(X, Y, K = 3)
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
                 sizes = 2^(2:4), rfeControl = rfe_control, metric = ifelse(is.factor(trainData_SMOTE$Class), "Accuracy", "Kappa"))

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

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                 kernel = "radial", 
                 cost = c(0.1, 1, 10, 100,150),
                 gamma = c(0.001,0.01, 0.1, 1, 10),
                 degree = 3,
                 type = "C-classification",
                 tolerance = 0.0005,
                 class.weights = c("0"=1, "1"=1.5),
                 probability = TRUE)


svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_rfe, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

