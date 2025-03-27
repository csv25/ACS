getwd()
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)

###SMOTE Family
library(smotefamily)

## 1: Random Forest
library(randomForest)

## 2: Logistic Regression

## 3 : SUPPORT VECTOR MACHINE
library(e1071)

## 4 :NAIVE BAYES

## 5.  NEURAL NETWORKS
library(nnet)

## 6: KNN
library(class)


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

######################################

dim(trainData)
table(trainData$Class)

dim(testData)

# SMOTE
set.seed(123)
#Dividing dataframe between features

#data frame or matrix of numeric-attributed dataset
X <- trainData[, -which(names(trainData) == "Class")]
#Making the dataframe all numeric variables
X <- as.data.frame(lapply(X, as.numeric))

# A vector of a target class attribute corresponding to a dataset X
Y <- trainData$Class
length(Y)
# We will use this Y later 
Y <- trainData$Class
length(Y)

smote_result <- SMOTE(X, Y, K = 3)
typeof(smote_result)

trainData_SMOTE <- smote_result$data
dim(trainData_SMOTE)
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"

trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
#Dimentions of the class variable after SMOTE
table(trainData_SMOTE$Class)

##############################################
# #ROSE
# trainData_SMOTE.ROSE <- ovun.sample(Class ~ ., 
#                                     data = trainData_SMOTE, 
#                                     method = "under", 
#                                     N = sum(trainData_SMOTE$Class == 1) * 2,
#                                     seed = 123)$data
# table(trainData_SMOTE.ROSE$Class)
# dim(trainData_SMOTE.ROSE)

################################################

# Creating RFE 
#rfFuncs:Uses random forests method of assessing the mean decrease in accuracy over 
#the features of interest i.e. the x (independent variables) and through the recursive 
#nature of the algorithm looks at which IVs have the largest affect on the mean decrease 
#in accuracy for the predicted y. The algorithm then purges the features with a low feature 
#importance, those that have little effect on changing this accuracy metric.
#cv: Cross Validation
# Number = 5 Fold Cross Validation

set.seed(123)

rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 5, verbose = TRUE)
#rfFuncs:Uses random forests method of assessing the mean decrease in accuracy over 
#the features of interest i.e. the x (independent variables) and through the recursive 
#nature of the algorithm looks at which IVs have the largest affect on the mean decrease 
#in accuracy for the predicted y. The algorithm then purges the features with a low feature 
#importance, those that have little effect on changing this accuracy metric.
#cv: Cross Validation
# Number = 5 Fold Cross Validation

rfe_model <- rfe(trainData_SMOTE[, -ncol(trainData_SMOTE)], 
                 trainData_SMOTE$Class,
                 sizes = c(5, 10, 15, 20), 
                 rfeControl = rfe_control, 
                 metric = ifelse(is.factor(trainData_SMOTE$Class), "Accuracy", "Kappa"))

rfe_model

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


###########################################
# 1: Random Forests

#Hyper Parameters:
# ntree:Number of trees to grow. 
# This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
# importance: Should importance of predictors be assessed?
# classwt: Priors of the classes. Need not add up to one. Ignored for regression.
# sampsize: Size(s) of sample to draw. 
# For classification, if sampsize is a vector of the length the number of strata, then sampling is stratified by strata, 
# and the elements of sampsize indicate the numbers to be drawn from the strata.

#Random Forest without using Caret package meets minimum requirements
#Picked best model for timing purposes
set.seed(125)
# Does not meet minimum requirements
# Sensitivity : 0.8542         
# Specificity : 0.6632 
 
rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                         ntree = 1500, 
                         importance = TRUE, 
                         classwt = c(1, 15),
                         maxnodes = 250,
                         replace = FALSE)

print(rf_model)

rf_test_predictions <- predict(rf_model, testData_selected)
rf_test_cm <- confusionMatrix(rf_test_predictions, testData_selected$Class)
rf_test_cm 

##################################################

# 2 : LOGISTIC REGRESSION
set.seed(125)
# Does not meet minimum requirements
# Sensitivity : 0.8192        
# Specificity : 0.7789 

lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                  method = "glm",
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 10),
                  )


lr_model

lr_test_predictions <- predict(lr_model, testData_selected)
lr_test_cm <- confusionMatrix(lr_test_predictions, testData_selected$Class)
lr_test_cm 

#########################################################

# 3 : Support Vector Machine
# Meets Minimum Requirements
# Sensitivity : 0.8067         
# Specificity : 0.8316 

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                 kernel = "radial", 
                 cost = c(0.1, 1, 10, 40, 100,150),
                 gamma = c(0.001,0.01, 0.1, 1, 10),
                 degree = 3,
                 type = "C-classification",
                 tolerance = 0.0005,
                 class.weights = c("0"=1, "1"=1.5),
                 probability = TRUE)

svm_model
svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

##########################################################

# 4 : Naive Bayes
set.seed(125)
# Meeets minimum requirement
# Sensitivity : 0.7900          
# Specificity : 0.8316  
nb.grid <- expand.grid(usekernel = c(TRUE, FALSE),
                       laplace=c(0,1),
                       adjust = c(1, 2, 3, 4))

train.control <- trainControl(method = 'cv',
                              number = 5,
                              verboseIter = TRUE)

nb.model <- train(Class ~., 
                  data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                  method = 'naive_bayes',
                  tuneGrid = nb.grid,
                  trControl = train.control
)
nb.model

#Running the model on the test data
nb_test_predictions <- predict(nb.model, testData_selected, probability = TRUE)
nb_test_cm <- confusionMatrix(nb_test_predictions, testData_selected$Class) 
nb_test_cm

#######################################################
# 5 : Neural Networks
# Does not meet Minimum Requirements
Sensitivity : 0.8475          
Specificity : 0.7158  

set.seed(125)

nnet.grid <- expand.grid(size = c(1, 2, 3),
                         decay = c(0.1, 0.5, 1, 2, 3))

nnet.trainControl <- trainControl(
  method = 'cv',
  number = 5,
  verboseIter = TRUE
)

nnet.model <- train(Class ~., 
                    data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                    method = 'nnet',
                    tuneGrid = nnet.grid,
                    trControl = nnet.trainControl,
                    preProcess= c('center', 'scale'),
                    trace = FALSE)

nnet.model

nnet_test_predictions <- predict(nnet.model, testData_selected)
nnet_test_cm <- confusionMatrix(nnet_test_predictions, testData_selected$Class) 
nnet_test_cm


#####################################################################
# 6: K-NN algorithm
# Does not meet minimum requirements
# Sensitivity : 0.8725         
# Specificity : 0.7158  

set.seed(124)

train.control <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE)

knn.grid <- expand.grid(k=c(3,4,5,6,7,8,9,11,13,14))

knn.model <- train(Class~., 
                   data = trainData_SMOTE[, c(selected_features_rfe, "Class")],
                   method = 'knn',
                   tuneGrid = knn.grid,
                   trControl = train.control,
                   preProcess = c('center', 'scale'),
                   tuneLength = 10)

knn.model

knn_test_predictions <- predict(knn.model, testData_selected)
knn_test_cm <- confusionMatrix(knn_test_predictions, testData_selected$Class) 
knn_test_cm

#############################################################
# KNN without using the Caret Package to perform Cross Validation
# Does not Meet Minimum Requirements
# Sensitivity : 0.8550          
# Specificity : 0.6947

knn_predictions <- knn(train = trainData_SMOTE[, c(selected_features_rfe, "Class")], 
                       test = testData_selected, 
                       cl = trainData_SMOTE$Class, 
                       k = 5)

plot(knn_predictions)

knn_test_cm <- confusionMatrix(knn_predictions, testData_selected$Class)
knn_test_cm


