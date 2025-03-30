## ** ------------------------------------------------------------------------------------ ** 
## CS699 – Spring 2025 - Project Assignment 
## Students : Melanie R Loaiza, BU ID : and Carlos Vargas, BU ID:  
## ** ------------------------------------------------------------------------------------ **  

## ** ----------  STEP 1: Load and Preview project_data.csv ---------- **
# Working directory - Setting/Getting
getwd()
# Set working directory
setwd('/Users/melanieloaiza/Desktop/BU - Data Science /Spring 2025/MET CS699 - Data Mining/project_assignment')

df <- read.csv("project_data.csv", header=TRUE, sep = ",") 

library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)

## ** ----------  STEP 2: Data Exploration ---------- ** 
# Quick exploration of the dataframe 
head(df)                # Print the first 6 records  
dim(df)                 # Print the dimensions 
str(df)                 # Print structure 
summary(df)             # Summary statistics 

n_miss(df)              # Total number of NA values in the entire dataframe 
prop_miss(df)           # Proportion of NA values in the entire dataframe 

miss_var_summary(df) %>% arrange(desc(pct_miss)) %>%slice_head(n = 50) %>% print(n = 50)  # Summary table of NA values (Variable , No. NA , Proportion NA) 
gg_miss_var(df)         # Plot 1 (Figure 1) : Bar chart of NA values per variable 
gg_miss_upset(df)       # Plot 2 (Figure 2) : Patterns of NA values amongst variables 

#---- Overview before Preprocessing-----#
# dim(df) : 4318  117
# n_miss(df) : 141635 
# prop_miss(df) : 0.280351 
#---------------------------------------#

## ** ----------  STEP 3: Data preprocessing ---------- **  
# NA values 
threshold <- 0.50 * nrow(df)                                  # 0.50 threshold for NA values 
df <- df %>% select(where(~ sum(is.na(.)) <= threshold))      # Drop variables that exceeds threshold 

# Dropping identifiers variables
df <- df[, !names(df) %in% c("SERIALNO", "SPORDER")]  

# nearZero variance variables using default 10 and uniqueCut = 6 
df1 <- df 
zerovar <- nearZeroVar(df, names = TRUE) 
print(zerovar)                            # Print nearZero variance variables (default uniqueCut=10)
length(zerovar)                           # 31 nearZero variance variables found  
df <- df %>% select(-all_of(zerovar))     # Dropping nearZero variance variables  

zerovar_1 <- nearZeroVar(df1, names = TRUE, uniqueCut =6)
print(zerovar_1)                          # Print nearZero variance variables (uniqueCut=6)
length(zerovar_1)                         # 29 nearZero variance variables found  
df1 <- df1 %>% select(-all_of(zerovar_1)) # Dropping nearZero variance variables   

# Imputation for DF
df$INDP[is.na(df$INDP)] <- 9920         # PUMPS : Industry recode for 2023 and later based on 2022 IND codes
df$OCCP[is.na(df$OCCP)] <- 9920         # PUMPS : Occupation recode for 2018 and later based on 2018 OCC codes

df$NWAB[is.na(df$NWAB)] <- "2"          # PUMPS : Temporary absence from work
df$NWLA[is.na(df$NWLA)] <- "2"          # PUMPS : On layoff from work
df$NWLK[is.na(df$NWLK)] <- "2"          # PUMPS : Looking for work

df$COW[is.na(df$COW)] <- "0"            # PUMPS : Class of worker   
df$WRK[is.na(df$WRK)] <- "0"            # PUMPS : Worked last week  
df$ESR[is.na(df$ESR)] <- "0"            # PUMPS : Employment status recode 
df$MARHT[is.na(df$MARHT)] <- "0"        # PUMPS : Number of times married
df$JWTRNS[is.na(df$JWTRNS)] <- "0"      # PUMPS : Means of transportation to work

df$WKHP <- ifelse(is.na(df$WKHP), median(df$WKHP, na.rm = TRUE), df$WKHP)           # PUMPS : Usual hours worked per week past 12 months 
df$PERNP <- ifelse(is.na(df$PERNP), median(df$PERNP, na.rm = TRUE), df$PERNP)       # PUMPS : Total person's earnings  
df$POVPIP <- ifelse(is.na(df$POVPIP), median(df$POVPIP, na.rm = TRUE), df$POVPIP)   # PUMPS : Income-to-poverty ratio recode

df$MARHYP <- ifelse(is.na(df$MARHYP), 0,  ifelse(df$MARHYP <= 1944, 1, 2))          # PUMPS : Year last married   
df$POWPUMA <- ifelse(is.na(df$POWPUMA), 0,  ifelse(df$POWPUMA == "00001", 1, 2))    # PUMPS : Place of work PUMA based on 2020 Census definition  

# Imputation for DF1 
df1$INDP[is.na(df1$INDP)] <- 9920         # PUMPS : Industry recode for 2023 and later based on 2022 IND codes
df1$OCCP[is.na(df1$OCCP)] <- 9920         # PUMPS : Occupation recode for 2018 and later based on 2018 OCC codes

df1$NWAB[is.na(df1$NWAB)] <- "2"          # PUMPS : Temporary absence from work
df1$NWLA[is.na(df1$NWLA)] <- "2"          # PUMPS : On layoff from work
df1$NWLK[is.na(df1$NWLK)] <- "2"          # PUMPS : Looking for work

df1$COW[is.na(df1$COW)] <- "0"            # PUMPS : Class of worker   
df1$WRK[is.na(df1$WRK)] <- "0"            # PUMPS : Worked last week  
df1$ESR[is.na(df1$ESR)] <- "0"            # PUMPS : Employment status recode 
df1$MARHT[is.na(df1$MARHT)] <- "0"        # PUMPS : Number of times married
df1$JWTRNS[is.na(df1$JWTRNS)] <- "0"      # PUMPS : Means of transportation to work

df1$WKHP <- ifelse(is.na(df1$WKHP), median(df1$WKHP, na.rm = TRUE), df1$WKHP)           # PUMPS : Usual hours worked per week past 12 months 
df1$PERNP <- ifelse(is.na(df1$PERNP), median(df1$PERNP, na.rm = TRUE), df1$PERNP)       # PUMPS : Total person's earnings  
df1$POVPIP <- ifelse(is.na(df1$POVPIP), median(df1$POVPIP, na.rm = TRUE), df1$POVPIP)   # PUMPS : Income-to-poverty ratio recode

df1$MARHYP <- ifelse(is.na(df1$MARHYP), 0,  ifelse(df1$MARHYP <= 1944, 1, 2))          # PUMPS : Year last married   
df1$POWPUMA <- ifelse(is.na(df1$POWPUMA), 0,  ifelse(df1$POWPUMA == "00001", 1, 2))    # PUMPS : Place of work PUMA based on 2020 Census definition  

dim(df) 
n_miss(df) 
prop_miss(df) 

dim(df1) 
n_miss(df1) 
prop_miss(df1) 

#---- Overview after Preprocessing-----#
# dim(df) : 4318   50
# n_miss(df) : 0
# prop_miss(df) : 0

# dim(df1) : 4318   52
# n_miss(df1) : 0
# prop_miss(df1) : 0
#--------------------------------------#

## ** ----------  STEP 4: Data splitting (df and df1) ---------- **    
# createDataPartition (Stratified sampling)
df$Class <- as.factor(df$Class)           # df : Factoring target variable 
table(df$Class)                           # df : Class distribution No: 4001 and Yes: 317

df1$Class <- as.factor(df1$Class)         # df1 : Factoring target variable 
table(df1$Class)                          # df1 : Class distribution No: 4001 and Yes: 317

library(rsample)
set.seed(123)
trainIndex <- createDataPartition(df$Class, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

table(trainData$Class)                    # df : Class distribution trainData No: 2801 and Yes: 222
dim(trainData)                            # df : trainData dimensions 3023   50 

table(testData$Class)                     # df : Class distribution testData No: 1200 and Yes: 95 
dim(testData)                             # df : testData dimensions 1295   50 

library(rsample)
set.seed(123)
trainIndex_df1 <- createDataPartition(df1$Class, p = 0.7, list = FALSE)
trainData_df1 <- df1[trainIndex_df1, ]
testData_df1 <- df1[-trainIndex_df1, ]

table(trainData_df1$Class)                # df1 : Class distribution trainData No: 2801 and Yes: 222
dim(trainData_df1)                        # df1 : trainData_df1 dimensions 3023   52

table(testData_df1$Class)                 # df1 : Class distribution testData No: 1200 and Yes: 95 
dim(testData_df1)                         # df1 : testData_df1 dimensions 1295   52

## ** ----------  STEP 5: Data transformation (df and df1) ---------- **  
set.seed(123)
trainData[, -which(names(trainData) == "Class")] <-  lapply(trainData[, -which(names(trainData) == "Class")], as.numeric)
testData[, -which(names(testData) == "Class")] <-  lapply(testData[, -which(names(testData) == "Class")], as.numeric)

str(trainData)                            # df : Print structure trainData 
str(testData)                             # df : Print structure testData 

set.seed(123)
trainData_df1[, -which(names(trainData_df1) == "Class")] <-  lapply(trainData_df1[, -which(names(trainData_df1) == "Class")], as.numeric)
testData_df1[, -which(names(testData_df1) == "Class")] <-  lapply(testData_df1[, -which(names(testData_df1) == "Class")], as.numeric)

str(trainData_df1)                        # df1 : Print structure trainData_df1 
str(testData_df1)                         # df1 : Print structure testData_df1 

## ** ----------  STEP 6: Oversampling Technique : SMOTE (df and df1) ---------- **    
library(smotefamily) 
set.seed(123)
X <- trainData[, -which(names(trainData) == "Class")]
X <- as.data.frame(lapply(X, as.numeric))

Y <- trainData$Class
Y <- trainData$Class

smoteResult <- SMOTE(X, Y, K = 5)  
trainDataSMOTE <- smoteResult$data
colnames(trainDataSMOTE)[ncol(trainDataSMOTE)] <- "Class"
trainDataSMOTE$Class <- as.factor(trainDataSMOTE$Class)

table(trainDataSMOTE$Class)               # df : Class distribution trainDataSMOTE No: 2801 and Yes: 2664

set.seed(123)
X_DF1 <- trainData_df1[, -which(names(trainData_df1) == "Class")]
X_DF1 <- as.data.frame(lapply(X_DF1, as.numeric))

Y_DF1 <- trainData_df1$Class
Y_DF1 <- trainData_df1$Class

smoteResult_DF1 <- SMOTE(X_DF1, Y_DF1, K = 5)  
trainDataSMOTE_df1 <- smoteResult_DF1$data
colnames(trainDataSMOTE_df1)[ncol(trainDataSMOTE_df1)] <- "Class"
trainDataSMOTE_df1$Class <- as.factor(trainDataSMOTE_df1$Class)

table(trainDataSMOTE_df1$Class)           # df1 : Class distribution trainDataSMOTE_df1 No: 2801 and Yes: 2664

## ** ----------  STEP 6.1: Feature Selection Technique : BORUTA (df and df1) ---------- **    
library(Boruta)
set.seed(123)
boruta <- Boruta(Class ~ ., data = trainDataSMOTE, doTrace = 0)
borutaFeatures <- getSelectedAttributes(boruta, withTentative = FALSE)

print(borutaFeatures)                     # df : Print borutaFeatures 
length(borutaFeatures)                    # df : 49 borutaFeatures found

trainDataSMOTE_BORUTA <- trainDataSMOTE[, c(borutaFeatures, "Class")]
testDataBORUTA <- testData[, c(borutaFeatures, "Class")] 

table(trainDataSMOTE_BORUTA$Class)        # df : Class distribution trainDataSMOTE_BORUTA No: 2801 and Yes: 2664 
dim(trainDataSMOTE_BORUTA)                # df : trainDataSMOTE_BORUTA dimensions 5465   50 

table(testDataBORUTA$Class)               # df : Class distribution testDataBORUTA No: 1200 and Yes: 95  
dim(testDataBORUTA)                       # df : testDataBORUTA dimensions 1295   50 


set.seed(123)
boruta_df1 <- Boruta(Class ~ ., data = trainDataSMOTE_df1, doTrace = 0)
borutaFeatures_df1 <- getSelectedAttributes(boruta_df1, withTentative = FALSE)

print(borutaFeatures_df1)               # df1 : Print borutaFeatures_df1 
length(borutaFeatures_df1)              # df1 : 51 borutaFeatures_df1 found

trainDataSMOTE_BORUTA_df1 <- trainDataSMOTE_df1[, c(borutaFeatures_df1, "Class")]
testDataBORUTA_df1 <- testData_df1[, c(borutaFeatures_df1, "Class")] 

table(trainDataSMOTE_BORUTA_df1$Class)  # df1 : Class distribution trainDataSMOTE_BORUTA_df1 No: 2801 and Yes: 2664 
dim(trainDataSMOTE_BORUTA_df1)          # df1 : trainDataSMOTE_BORUTA_df1 dimensions 5465   52

table(testDataBORUTA_df1$Class)         # df1 : Class distribution testDataBORUTA_df1 No: 1200 and Yes: 95  
dim(testDataBORUTA_df1)                 # df1 : testDataBORUTA_df1 dimensions 1295   52

## ** ----------  STEP 6.1.1:  SMOTE-BORUTA - Best Models (df and df1) ---------- **  
## MODEL 1 : Random Forest : 
## df : Sensitivity : 0.9617 
## df : Specificity : 0.4947  
library(randomForest)  

set.seed(1401) 
rf_model <- randomForest(Class ~ .,  data = trainDataSMOTE_BORUTA,  ntree = 2000,  importance = TRUE,  classwt = c(1, 15),
                         maxnodes = 250, replace = FALSE, sampsize = c(500, 500))
rf_model

rf_preds <- predict(rf_model, testDataBORUTA)
rf_cm <- confusionMatrix(rf_preds, testDataBORUTA$Class)
rf_cm 

## MODEL 2 : Support Vector Machine: 
## df : Sensitivity : 0.8158     
## df : Specificity : 0.8316    
library(e1071) 

set.seed(1401) 
svm_model <- svm(Class ~ ., data = trainDataSMOTE_BORUTA,  kernel = "radial",  cost = c(0.1, 1, 10, 50, 100,150),
                 gamma = c(0.001, 0.01, 0.1, 1), probability = TRUE, tolerance = 0.05, class.weights = c("No"=1, "Yes"=1.3) , preProc = c("center", "scale"))
svm_model

svm_preds <- predict(svm_model, testDataBORUTA, probability = TRUE)
svm_cm <- confusionMatrix(svm_preds, testDataBORUTA$Class) 
svm_cm  

## df1 : Sensitivity : 0.8175     
## df1 : Specificity : 0.8316    
set.seed(1401) 
svm_model <- svm(Class ~ ., data = trainDataSMOTE_BORUTA_df1,  kernel = "radial",  cost = c(0.1, 1, 10, 50, 100,150),
                 gamma = c(0.001, 0.01, 0.1, 1), probability = TRUE, tolerance = 0.05, class.weights = c("No"=1, "Yes"=1.3) , preProc = c("center", "scale"))
svm_model

svm_preds <- predict(svm_model, testDataBORUTA_df1, probability = TRUE)
svm_cm <- confusionMatrix(svm_preds, testDataBORUTA_df1$Class) 
svm_cm 

## MODEL 3 : kNN: 
## df : Sensitivity : 0.8517       
## df : Specificity : 0.5895
library(caret) 

set.seed(123)
knnGrid <- expand.grid(k = c(4,5,6,7,9,11,15))
knn_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE)

set.seed(1401)
knn_model <- train(Class ~.,  data = trainDataSMOTE_BORUTA, method = 'knn', tuneGrid = knnGrid, trControl = knn_trainControl, preProcess = c('center', 'scale'))
knn_model

plot(knn_model)

knn_preds <- predict(knn_model, testDataBORUTA)
knn_cm <- confusionMatrix(knn_preds, testDataBORUTA$Class)
knn_cm

knn_model$bestTune
knn_model$results 

## df1 : Sensitivity : 0.8517    
## df1 : Specificity : 0.6421
library(caret) 

set.seed(123)
knnGrid <- expand.grid(k = c(4,5,6,7,9,11,15))
knn_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE)

set.seed(1401)
knn_model <- train(Class ~.,  data = trainDataSMOTE_BORUTA_df1, method = 'knn', tuneGrid = knnGrid, trControl = knn_trainControl, preProcess = c('center', 'scale'))
knn_model

plot(knn_model)

knn_preds <- predict(knn_model, testDataBORUTA_df1)
knn_cm <- confusionMatrix(knn_preds, testDataBORUTA_df1$Class)
knn_cm

knn_model$bestTune
knn_model$results

## MODEL 4 : Naives Bayes: 
## df : Sensitivity : 0.7958 
## df : Specificity : 0.8211  
library(e1071)   

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), laplace=c(0,1), adjust = c(1, 2, 3, 4))
nb_trainControl <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)

set.seed(1401)
nb_model <- train(Class ~.,  data = trainDataSMOTE_BORUTA, method = 'naive_bayes', tuneGrid = nbGrid, trControl = nb_trainControl)
nb_model

plot(nb_model)

nb_preds <- predict(nb_model, testDataBORUTA)
nb_cm <- confusionMatrix(nb_preds, testDataBORUTA$Class)
nb_cm

## MODEL 5 : Neural Networks
## df : Sensitivity : 0.8300 
## df : Specificity : 0.6737 
library(nnet) 

set.seed(123)
nnetGrid <- expand.grid(size = c(1, 2,3), decay = c(0.01, 0.1, 0.5,1,2,3))
nnet_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE )

set.seed(1401)
nnet_model <- train(Class ~.,  data = trainDataSMOTE_BORUTA, method = 'nnet', tuneGrid = nnetGrid, trControl = nnet_trainControl,
                    preProcess= c('center', 'scale'), trace = FALSE)

nnet_model
plot(nnet_model) 

nnet_preds <- predict(nnet_model, testDataBORUTA, probability = TRUE)
nnet_cm <- confusionMatrix(nnet_preds, testDataBORUTA$Class) 
nnet_cm

## MODEL 6 :  Logistic Regression 
## df : Sensitivity : 0.8700  
## df : Specificity : 0.7053 
set.seed(123)
lr_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE )

set.seed(1401)
lr_model <- train(Class ~ ., data = trainDataSMOTE_BORUTA, method = "glm", family = "binomial",
                  trControl = lr_trainControl, weights = ifelse(trainDataSMOTE_BORUTA$Class == "No", 1.5, 1))
lr_model

lr_preds <- predict(lr_model, testDataBORUTA)
lr_cm <- confusionMatrix(lr_preds, testDataBORUTA$Class)
lr_cm  

## EXTRA MODEL 7 : STACKED ENSEMBLE METHOD 
## df : Sensitivity : 0.9533 
## df : Specificity : 0.3368    
library(caretEnsemble) 

## Approach 1 :   
algorithmList <- c("rpart", "naive_bayes", "nnet", "knn", "svmRadial")

set.seed(123)
stacked_trainControl <- trainControl(method = "cv", number = 10, savePredictions=TRUE, classProbs = TRUE)
base_models <- caretList(Class ~ ., data = trainDataSMOTE_BORUTA, trControl = stacked_trainControl,  methodList = algorithmList)
summary(resamples(base_models))

results <- resamples(base_models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)

set.seed(1401)
stackGlm_model <- caretStack(base_models, method="glm", metric="Accuracy", trControl=stacked_trainControl)
stackGlm_model

plot(stackGlm_model)

stackGlm_preds <- predict(stackGlm_model,  testDataBORUTA)
stackGlm_preds <- colnames(stackGlm_preds)[apply(stackGlm_preds, 1, which.max)]
stackGlm_preds <- factor(stackGlm_preds, levels = levels(testDataBORUTA$Class))

stackGlm_cm <- confusionMatrix(stackGlm_preds, testDataBORUTA$Class)
stackGlm_cm 

## ** ----------  STEP 6.2: Feature Selection Technique: RFE (Only df) ---------- **    
library(caret)
library(randomForest)

X_balanced <- trainDataSMOTE[, -which(names(trainDataSMOTE) == "Class")]
X_balanced <- as.data.frame(lapply(X_balanced, as.numeric))  
Y_balanced <- trainDataSMOTE$Class

set.seed(123)
rfe_trainControl <- rfeControl(functions = rfFuncs,  method = "cv", number = 10)
rfe <- rfe(X_balanced, Y_balanced, sizes = c(10, 20, 30), rfeControl = rfe_trainControl)

varImpRFE <- varImp(rfe)
rfeFeatures <- rownames(varImpRFE)[varImpRFE$Overall > 9.58]

print(rfeFeatures)               # Print rfeFeatures 
length(rfeFeatures)              # 42 rfeFeatures found

trainDataSMOTE_RFE <- trainDataSMOTE[, c(rfeFeatures, "Class")]
testDataRFE <- testData[, c(rfeFeatures, "Class")] 

table(trainDataSMOTE_RFE$Class)  # Class distribution trainDataSMOTE_RFE No: 2801 and Yes: 2664 
dim(trainDataSMOTE_RFE)          # trainDataSMOTE_RFE dimensions 5465   43 

table(testDataRFE$Class)         # Class distribution testDataRFE No: 1200 and Yes: 95   
dim(testDataRFE)                 # testDataRFE dimensions 1295   43 

## ** ----------  STEP 6.2.1:  SMOTE-RFE - Best Models (Only df) ---------- **    
## MODEL 1 : Random Forest : 
## df: Sensitivity : 0.9600
## df : Specificity : 0.5158  
set.seed(1401) 
rf_model <- randomForest(Class ~ ., data = trainDataSMOTE_RFE, ntree = 1500,  importance = TRUE,  classwt = c(1, 15),
                         maxnodes = 250, replace = FALSE)
rf_model
plot(rf_model)

rf_preds <- predict(rf_model, testDataRFE)
rf_cm <- confusionMatrix(rf_preds, testDataRFE$Class)
rf_cm 

## MODEL 2 : Support Vector Machine:
## df : Sensitivity : 0.8208
## df : Specificity : 0.8105 
library(e1071) 

set.seed(1401) 
svm_model <- svm(Class ~ ., data = trainDataSMOTE_RFE,  kernel = "radial",  cost = c(0.1, 1, 10, 40, 100,150),
                 gamma = c(0.001,0.01, 0.1, 1, 10), degree = 3, type = "C-classification", tolerance = 0.0005,
                 class.weights = c("No"=1, "Yes"=1.4), probability = TRUE)
svm_model

svm_preds <- predict(svm_model, testDataRFE, probability = TRUE)
svm_cm <- confusionMatrix(svm_preds, testDataRFE$Class) 
svm_cm

## MODEL 3 : kNN: 
## df : Sensitivity : 0.8667
## df : Specificity : 0.5474  
set.seed(123)
knnGrid <- expand.grid(k = seq(3, 100, by = 2)) 
knn_trainControl <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE)

set.seed(1401)
knn_model <- train(Class~.,  data = trainDataSMOTE_RFE, method = 'knn', tuneGrid = knnGrid, trControl = knn_trainControl,
                   preProcess = c('center', 'scale'), tuneLength = 10)
knn_model
plot(knn_model)

knn_preds <- predict(knn_model, testDataRFE)
knn_cm <- confusionMatrix(knn_preds, testDataRFE$Class) 
knn_cm 

## MODEL 4 : Naives Bayes:
## df : Sensitivity : 0.8017
## df : Specificity : 0.8316 
library(e1071)    

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), laplace=c(0,1), adjust = c(1, 2, 3, 4))
nb_trainControl <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)

set.seed(1401)
nb_model <- train(Class ~.,  data = trainDataSMOTE_RFE, method = 'naive_bayes', tuneGrid = nbGrid, trControl = nb_trainControl)
nb_model 

plot(nb_model)

nb_preds <- predict(nb_model, testDataRFE, probability = TRUE)
nb_cm <- confusionMatrix(nb_preds, testDataRFE$Class) 
nb_cm

## MODEL 5 : Neural Networks 
## df : Sensitivity : 0.8467  
## df : Specificity : 0.6737 
set.seed(123)
nnetGrid <- expand.grid(size = c(1, 2, 3), decay = c(0.1, 0.5, 1, 2, 3))
nnet_trainControl <- trainControl( method = 'cv', number = 5, verboseIter = TRUE)

set.seed(1401)
nnet_model <- train(Class ~.,  data = trainDataSMOTE_RFE, method = 'nnet', tuneGrid = nnetGrid, trControl = nnet_trainControl,
                    preProcess= c('center', 'scale'), trace = FALSE)
nnet_model
plot(nnet_model)

nnet_preds <- predict(nnet_model, testDataRFE)
nnet_cm <- confusionMatrix(nnet_preds, testDataRFE$Class) 
nnet_cm

## MODEL 6 :  Logistic Regression 
## df : Sensitivity : 0.8242  
## df : Specificity : 0.7895
set.seed(123) 
lr_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE ) 

set.seed(1401)
lr_model <- train(Class ~ ., data = trainDataSMOTE_RFE, method = "glm", family = "binomial", trControl = lr_trainControl)
lr_model

lr_preds <- predict(lr_model, testDataRFE)
lr_cm <- confusionMatrix(lr_preds, testDataRFE$Class)
lr_cm 

## EXTRA MODEL 7 : STACKED ENSEMBLE METHOD 
## df : Sensitivity : 0.9575   
## df : Specificity : 0.3474  
library(caretEnsemble) 
## Approach 1 :   
algorithmList <- c("rpart", "naive_bayes", "nnet", "knn", "svmRadial")

set.seed(123)
stacked_trainControl <- trainControl(method = "cv", number = 10, savePredictions=TRUE, classProbs = TRUE)
base_models <- caretList(Class ~ ., data = trainDataSMOTE_RFE, trControl = stacked_trainControl,  methodList = algorithmList)
summary(resamples(base_models))

results <- resamples(base_models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)

set.seed(1401)
stackGlm_model <- caretStack(base_models, method="glm", metric="Accuracy", trControl=stacked_trainControl)
stackGlm_model

plot(stackGlm_model)

stackGlm_preds <- predict(stackGlm_model,  testDataRFE)
stackGlm_preds <- colnames(stackGlm_preds)[apply(stackGlm_preds, 1, which.max)]
stackGlm_preds <- factor(stackGlm_preds, levels = levels(testDataRFE$Class))

stackGlm_cm <- confusionMatrix(stackGlm_preds, testDataRFE$Class)
stackGlm_cm 

## ** ----------  STEP 6.3:  Feature Selection Technique: Information Gain  (Only df) ---------- **  
library(FSelectorRcpp) 

set.seed(123)
infoGain <- information_gain(Class ~ ., trainDataSMOTE)
infoGain$Attribute <- rownames(infoGain)
rownames(infoGain) <- NULL

names(infoGain)[names(infoGain) == "importance"] <- "Info_Gain"

sorted_infoGain <- infoGain[order(-infoGain$Info_Gain), ]
infoGain_Features <- sorted_infoGain[1:12, ]

print(infoGain_Features)                  # Print infoGain_Features 
nrow(infoGain_Features)                   # 12 infoGain_Features found

infoGain_Features <- infoGain_Features$attributes

trainDataSMOTE_infoGain <- trainDataSMOTE[, c(infoGain_Features, "Class")]
testData_infoGain <- testData[, c(infoGain_Features, "Class")] 

table(trainDataSMOTE_infoGain$Class)      # Class distribution trainDataSMOTE_infoGain No: 2801 and Yes: 2664 
dim(trainDataSMOTE_infoGain)              # trainDataSMOTE_infoGain dimensions 5465   13 

table(testData_infoGain$Class)            # Class distribution testData_infoGain No: 1200 and Yes: 95   
dim(testData_infoGain)                    # testData_infoGain dimensions 1295   13 

## ** ----------  STEP 6.3.1:  SMOTE- Information Gain - Best Models ---------- **   
## MODEL 1 : Balanced Random Forest  
## df : Sensitivity : 0.9308
## df : Specificity : 0.4842 
library(randomForest) 

set.seed(1401)
rf_model <- randomForest(Class ~ ., data = trainDataSMOTE_infoGain, ntree = 2000, importance = TRUE, classwt = c(1, 15), maxnodes = 400, replace = FALSE) 
rf_model

rf_preds <- predict(rf_model, testData_infoGain)
rf_cm <- confusionMatrix(rf_preds, testData_infoGain$Class)
rf_cm  

## MODEL 2 : Support Vector Machine: 
## df : Sensitivity : 0.7917    
## df : Specificity : 0.8211  
library(e1071) 

set.seed(1401) 
svm_model <- svm(Class ~ ., data = trainDataSMOTE_infoGain,  kernel = "radial", 
                 cost = c(0.1, 0.5, 1, 10, 40,80,100,150), gamma = c(0.01, 0.1, 1, 10, 15), degree = 5, type = "C-classification",
                 tolerance = 0.0001, class.weights = c("No"=1, "Yes"=2.5),
                 probability = TRUE) 
svm_model

svm_preds <- predict(svm_model, testData_infoGain, probability = TRUE)
svm_cm <- confusionMatrix(svm_preds, testData_infoGain$Class) 
svm_cm

## MODEL 3 : kNN: 
## df : Sensitivity : 0.8483 
## df : Specificity : 0.4632 
set.seed(123) 
knnGrid  <- expand.grid(k=c(6,7,8,9,11,13,14)) 
knn_trainControl  <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE)

set.seed(1401)
knn_model <- train(Class~.,  data = trainDataSMOTE_infoGain, method = 'knn', tuneGrid = knnGrid,
                   trControl = knn_trainControl, tuneLength = 10)
knn_model
plot(knn_model) 

knn_preds <- predict(knn_model, testData_infoGain)
knn_cm <- confusionMatrix(knn_preds, testData_infoGain$Class) 
knn_cm

## MODEL 4 : Naives Bayes: 
## df : Sensitivity : 0.8492  
## df : Specificity : 0.7158   
library(e1071)    

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), laplace=c(0,1), adjust = c(1, 2, 3, 4, 5,6,7,8,9))
nb_trainControl <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)

set.seed(1401)
nb_model <- train(Class ~.,  data = trainDataSMOTE_infoGain, method = 'naive_bayes', tuneGrid = nbGrid, trControl = nb_trainControl)
nb_model  

plot(nb_model)

nb_preds <- predict(nb_model, testData_infoGain, probability = TRUE)
nb_cm <- confusionMatrix(nb_preds, testData_infoGain$Class) 
nb_cm

## MODEL 5 : Logistic Regression 
## df : Sensitivity : 0.7808  
## df : Specificity : 0.8000  
set.seed(123) 
lr_trainControl <- trainControl( method = 'cv', number = 5, verboseIter = TRUE )  

set.seed(1401)
lr_model <- train(Class ~ ., data = trainDataSMOTE_infoGain, method = "glm", family = "binomial", trControl = lr_trainControl)
lr_model

lr_preds <- predict(lr_model, testData_infoGain)
lr_cm <- confusionMatrix(lr_preds, testData_infoGain$Class)
lr_cm 

## MODEL 6 : Neural Networks  
##  df : Sensitivity : 0.7600    
##  df : Specificity : 0.7263   
set.seed(123) 
nnetGrid <- expand.grid(size = c(4), decay = c(0.3, 0.4))
nnet_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE )

set.seed(1401)
nnet_model <- train(Class ~.,  data = trainDataSMOTE_infoGain, method = 'nnet', tuneGrid = nnetGrid, trControl = nnet_trainControl,
                    trace = FALSE)
nnet_model
plot(nnet_model)

nnet_preds <- predict(nnet_model, testData_infoGain)
nnet_cm <- confusionMatrix(nnet_preds, testData_infoGain$Class) 
nnet_cm

## ** ----------  STEP 7: Undersampling Technique : TOMEK (df and df1) ---------- ** 
library(UBL)
set.seed(123)
trainData_Tomek <- TomekClassif(Class ~ ., trainData)
trainData_Tomek <- trainData_Tomek[[1]] 

trainData_Tomek <- RandUnderClassif(Class ~ ., trainData_Tomek, C.perc = "balance")
table(trainData_Tomek$Class)        # df : Class distribution trainData_Tomek No: 137 and Yes: 137

set.seed(123)
trainData_Tomek_df1 <- TomekClassif(Class ~ ., trainData_df1)
trainData_Tomek_df1 <- trainData_Tomek_df1[[1]] 

trainData_Tomek_df1 <- RandUnderClassif(Class ~ ., trainData_Tomek_df1, C.perc = "balance")
table(trainData_Tomek_df1$Class)    # df1 : Class distribution trainData_Tomek_df1 No: 134 and Yes: 134

## ** ----------  STEP 7.1: Feature Selection Technique: BORUTA (df and df1) ---------- **
library(Boruta)
set.seed(123)
bor <- Boruta(Class ~ ., data = trainData_Tomek, doTrace = 2)
borFeatures <- getSelectedAttributes(bor, withTentative = TRUE)

print(borFeatures)                            # df : Print borFeatures 
length(borFeatures)                           # df : 21 borFeatures found

trainDataTomek_BORUTA <- trainData_Tomek[, c(borFeatures, "Class")]
testData_BORUTA <- testData[, c(borFeatures, "Class")] 

table(trainDataTomek_BORUTA$Class)            # df : Class distribution trainDataTomek_BORUTA No: 137 and Yes: 137 
dim(trainDataTomek_BORUTA)                    # df : trainDataTomek_BORUTA dimensions 274   22 

table(testData_BORUTA$Class)                  # df : Class distribution testData_BORUTA No: 1200 and Yes: 95  
dim(testData_BORUTA)                          # df : testData_BORUTA dimensions 1295   22 

set.seed(123)
bor_df1 <- Boruta(Class ~ ., data = trainData_Tomek_df1, doTrace = 2)
borFeatures_df1 <- getSelectedAttributes(bor_df1, withTentative = TRUE)

print(borFeatures_df1)                        # df1 : Print borFeatures 
length(borFeatures_df1)                       # df1 : 21 borFeatures found

trainDataTomek_BORUTA_df1 <- trainData_Tomek_df1[, c(borFeatures_df1, "Class")]
testData_BORUTA_df1 <- testData_df1[, c(borFeatures_df1, "Class")] 

table(trainDataTomek_BORUTA_df1$Class)        # df1 : Class distribution trainDataTomek_BORUTA_df1 No: 134 and Yes: 134 
dim(trainDataTomek_BORUTA_df1)                # df1 : trainDataTomek_BORUTA_df1 dimensions 268   22 

table(testData_BORUTA_df1$Class)              # df1 : Class distribution testData_BORUTA_df1 No: 1200 and Yes: 95  
dim(testData_BORUTA_df1)                      # df1 : testData_BORUTA_df1 dimensions 1295   22 

## ** ----------  STEP 7.1.1:  TOMEK-BORUTA - Best Models (Only df) ---------- **   
## MODEL 1 : Balanced Random Forest 
## df: Sensitivity : 0.8375      
## df : Specificity : 0.7474        
library(UBL)
library(caret)

set.seed(123)
brf_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5))
brf_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
brf_model <- randomForest(Class ~ .,  data = trainDataTomek_BORUTA, tuneGrid = brfGrid, trControl = brf_trainControl, ntree = 500)
brf_model

brf_preds <- predict(brf_model, testData_BORUTA)
brf_cm <- confusionMatrix(brf_preds, testData_BORUTA$Class)
brf_cm 

### Alternate cutoffs : 
## df : Sensitivity : 0.7733
## df : Specificity : 0.8316   
library(pROC)
brf_probs <- predict(brf_model, testData_BORUTA, type = "prob")
roc_obj <- roc(testData_BORUTA$Class, brf_probs[, "No"])
coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
plot(roc_obj)

brf_probs <- predict(brf_model, testData_BORUTA, type = "prob")
brf_preds <- ifelse(brf_probs[, "No"] >= 0.618, "No", "Yes")
brf_preds <- factor(brf_preds, levels = levels(testData_BORUTA$Class))

brf_cm <- confusionMatrix(brf_preds, testData_BORUTA$Class)
brf_cm

## MODEL 2 : Support Vector Machine:
## df : Sensitivity : 0.8000 
## df : Specificity : 0.8000    
set.seed(123)
svmGrid <- expand.grid(C = 16, sigma = 0.03125) 
svm_trainControl <- trainControl(method = "cv",  number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)

set.seed(1401)
svm_model <- train( Class ~ ., data = trainDataTomek_BORUTA, metric = "ROC", method = "svmRadial", trControl = svm_trainControl, tuneGrid = svmGrid , preProcess = c("center", "scale"))
svm_model

svm_model$results %>% arrange(desc(ROC)) %>% head(100)

svm_preds <- predict(svm_model, testData_BORUTA)
svm_cm <- confusionMatrix(svm_preds, testData_BORUTA$Class)
svm_cm 

### Class-weighted SVM
## df : Sensitivity : 0.8300      
## df : Specificity : 0.7684 
library(kernlab)
set.seed(123)
sigma <- sigest(Class ~ ., data =  trainDataTomek_BORUTA , frac = .75)
names(sigma) <-  NULL 
SVMwtsGrid <- data.frame(.sigma = sigma[2],  .C = 2^seq(-5, 1, length = 30))
SVMwts_trainControl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)

set.seed(1401)
SVMwts_model <- train(Class ~ .,
                      data = trainDataTomek_BORUTA, method = "svmRadial", tuneGrid = SVMwtsGrid, preProc = c("center", "scale"), class.weights = c(Yes = 15, No = 17.2),
                      metric = "ROC",  trControl = SVMwts_trainControl)
SVMwts_model
plot(SVMwts_model)

SVMwts_preds <- predict(SVMwts_model, testData_BORUTA)
SVMwt_cm <- confusionMatrix(SVMwts_preds, testData_BORUTA$Class)
SVMwt_cm

## MODEL 3 : kNN: 
## df : Sensitivity : 0.7858  
## df : Specificity : 0.8105    
library(caret)

set.seed(123)
knnGrid <- expand.grid(k = seq(3, 50, by = 2)) 
knn_trainControl <- trainControl(method = "repeatedcv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
knn_model <- train(Class ~ ., data = trainDataTomek_BORUTA, method = "knn", trControl = knn_trainControl, tuneGrid = knnGrid, preProcess = c("center", "scale"))
knn_model

plot(knn_model)

knn_preds <- predict(knn_model, testData_BORUTA)
knn_test_cm <- confusionMatrix(knn_preds, testData_BORUTA$Class)
knn_test_cm

knn_model$bestTune
knn_model$results

## MODEL 4 : Naives Bayes:
## df : Sensitivity : 0.7708 
## df : Specificity : 0.8316 
library(e1071)  

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), fL = c(0, 0.5, 1, 1.05, 2 , 2.05 ), adjust = c(0.5, 1, 2 , 3 , 4 , 5))
nb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
nb_model <- train(Class ~ ., data = trainDataTomek_BORUTA,  method = "nb",  trControl = nb_trainControl, tuneGrid = nbGrid)
nb_model  

plot(nb_model)

nb_preds <- predict(nb_model, testData_BORUTA)
nb_cm <- confusionMatrix(nb_preds, testData_BORUTA$Class)
nb_cm

## MODEL 5 : AdaBoosting 
#install.packages("adabag", dependencies = TRUE)
## df : Sensitivity : 0.8383   
## df : Specificity : 0.7368
library(adabag)
library(caret)
modelLookup("AdaBoost.M1")

set.seed(123)
boostingGrid <- expand.grid( mfinal = seq(50, 500, 50), maxdepth = c(1,5), coeflearn = "Freund")
boosting_trainControl <- trainControl( method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)

set.seed(1401)
boosting_model <- train( Class ~ ., data = trainDataTomek_BORUTA, method = "AdaBoost.M1", metric = "ROC", tuneGrid = boostingGrid, trControl = boosting_trainControl)
boosting_model

plot(boosting_model)

boosting_preds <- predict(boosting_model, testData_BORUTA)
boosting_cm <- confusionMatrix(boosting_preds, testData_BORUTA$Class)
boosting_cm

## MODEL 6 : xgb  
## df : Sensitivity : 0.8292
## df : Specificity : 0.7579
library(caret)
library(xgboost)
modelLookup("xgbTree")

set.seed(123)
xgbGrid <- expand.grid( nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3, 4),
                        gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
xgb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
xgb_model <- train(Class ~ ., data = trainDataTomek_BORUTA, method = "xgbTree", tuneGrid = xgbGrid, verbosity = 0, trControl = xgb_trainControl)
xgb_model

plot(xgb_model)

xgb_preds <- predict(xgb_model, testData_BORUTA)
xgb_cm <- confusionMatrix(xgb_preds, testData_BORUTA$Class)
xgb_cm

## EXTRA MODEL 7 : Stacked Ensemble Method 
## df : Sensitivity : 0.8283 
## df : Specificity : 0.7895 
library(caretEnsemble) 

## Approach 1 :   
algorithmList <- c("rpart", "naive_bayes", "nnet", "knn", "svmRadial")

set.seed(123)
stacked_trainControl <- trainControl(method = "repeatedcv", number = 10, savePredictions=TRUE, classProbs = TRUE)
base_models <- caretList(Class ~ ., data = trainDataTomek_BORUTA, trControl = stacked_trainControl,  methodList = algorithmList)
summary(resamples(base_models))

results <- resamples(base_models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)

set.seed(1401)
stackGlm_model <- caretStack(base_models, method="glm", metric="Accuracy", trControl=stacked_trainControl)
stackGlm_model

plot(stackGlm_model)

stackGlm_preds <- predict(stackGlm_model,  testData_BORUTA)
stackGlm_preds <- colnames(stackGlm_preds)[apply(stackGlm_preds, 1, which.max)]
stackGlm_preds <- factor(stackGlm_preds, levels = levels(testData_BORUTA$Class))

stackGlm_cm <- confusionMatrix(stackGlm_preds, testData_BORUTA$Class)
stackGlm_cm 

## ** ----------  STEP 7.2: Feature Selection Technique: RFE (Only df) ---------- **
set.seed(123)
X_balanced <- trainData_Tomek[, -which(names(trainData_Tomek) == "Class")]
X_balanced <- as.data.frame(lapply(X_balanced, as.numeric))  
Y_balanced <- trainData_Tomek$Class

set.seed(123)
rfetrainControl <- rfeControl(functions = rfFuncs,  method = "cv", number = 10)
rfeResults <- rfe(X_balanced, Y_balanced, sizes = c(10, 20, 30), rfeControl = rfetrainControl)

varImp_RFE <- varImp(rfeResults)
rfe_Features <- predictors(rfeResults)

print(rfe_Features)                   # df : Print rfe_Features      
length(rfe_Features)                  # df : 10 rfe_Features found 

trainData_Tomek_RFE <- trainData_Tomek[, c(rfe_Features, "Class")]    
testData_RFE <- testData[, c(rfe_Features, "Class")]

table(trainData_Tomek_RFE$Class)      # df : Class distribution trainData_Tomek_RFE No: 137 and Yes: 137  
dim(trainData_Tomek_RFE)              # df : trainData_Tomek_RFE dimensions 274   11  

table(testData_RFE$Class)             # df : Class distribution testData_RFE No: 1200 and Yes: 95  
dim(testData_RFE)                     # df : testData_RFE dimensions 1295   11  

## ** ----------  STEP 7.2.1:  TOMEK-RFE - Best Models (Only df) ---------- **   
## MODEL 1 : Balanced Random Forest 
## df : Sensitivity : 0.7917 
## df : Specificity : 0.8105 
library(UBL)
library(caret)

set.seed(123)
brf_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5))
brf_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

brf_model <- randomForest(Class ~ .,  data = trainData_Tomek_RFE, tuneGrid = brfGrid, trControl = brf_trainControl, ntree = 500, classwt = c(1, 2))
brf_model

brf_preds <- predict(brf_model, testData_RFE)
brf_cm <- confusionMatrix(brf_preds, testData_RFE$Class)
brf_cm 

## MODEL 2 : Support Vector Machine:
## df : Sensitivity : 0.8075 
## df : Specificity : 0.8000 
set.seed(123)
svmGrid <-  expand.grid(sigma = seq(0.01, 0.1, by = 0.01), C = seq(0.1, 1.0, by = 0.1))
svm_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)

set.seed(1401)
svm_model <- train( Class ~ ., data = trainData_Tomek_RFE, metric = "ROC", method = "svmRadial", trControl = svm_trainControl, tuneGrid = svmGrid , preProcess = c("center", "scale","pca"))
svm_model

plot(svm_model)

svm_preds <- predict(svm_model, testData_RFE)
svm_cm <- confusionMatrix(svm_preds, testData_RFE$Class)
svm_cm

## MODEL 3 : kNN: 
## df : Sensitivity : 0.8008 
## df : Specificity : 0.8105 
library(caret)

set.seed(123)
knnGrid <- expand.grid(k = seq(3, 25, by = 2)) 
knn_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
knn_model <- train(Class ~ ., data = trainData_Tomek_RFE, method = "knn", trControl = knn_trainControl, tuneGrid = knnGrid, preProcess = c("center", "scale"))
knn_model

plot(knn_model)

knn_preds <- predict(knn_model, testData_RFE)
knn_test_cm <- confusionMatrix(knn_preds, testData_RFE$Class)
knn_test_cm

knn_model$bestTune
knn_model$results

## MODEL 4 : Naives Bayes:
## df : Sensitivity : 0.7675 
## df : Specificity : 0.8526  
library(e1071)  

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), fL = c(0, 0.5, 1), adjust = c(0.5, 1, 2))
nb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
nb_model <- train(Class ~ ., data = trainData_Tomek_RFE,  method = "nb",  trControl = nb_trainControl, tuneGrid = nbGrid)
nb_model  

plot(nb_model)

nb_preds <- predict(nb_model, testData_RFE)
nb_cm <- confusionMatrix(nb_preds, testData_RFE$Class)
nb_cm

## MODEL 5 : Logistic Regression  
## df : Sensitivity : 0.8033  
## df : Specificity : 0.7789 
set.seed(123) 
lr_trainControl <- trainControl( method = 'cv', number = 10, verboseIter = TRUE ) 

set.seed(1401)
lr_model <- train(Class ~ ., data = trainData_Tomek_RFE, method = "glm", family = "binomial", trControl = lr_trainControl)
lr_model

lr_preds <- predict(lr_model, testData_RFE)
lr_cm <- confusionMatrix(lr_preds, testData_RFE$Class)
lr_cm 

## MODEL 6 : xgb 
## df : Sensitivity : 0.8167  
## df : Specificity : 0.7789 
library(caret)
library(xgboost)

set.seed(123)
#xgbGrid <- expand.grid( nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2),
#  colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
xgbGrid <- expand.grid( nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3, 4),
                        gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
xgb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)
set.seed(1401)
xgb_model <- train(Class ~ ., data = trainData_Tomek_RFE, method = "xgbTree", tuneGrid = xgbGrid, verbosity = 0, trControl = xgb_trainControl)
xgb_model

plot(xgb_model)

xgb_preds <- predict(xgb_model, testData_RFE)
xgb_cm <- confusionMatrix(xgb_preds, testData_RFE$Class)
xgb_cm

## EXTRA MODEL 7 : RPART 
## df : Sensitivity : 0.8058 
## df : Specificity : 0.7579 
library(rpart)
library(rpart.plot)

set.seed(123)
rpartGrid <- expand.grid(cp = seq(0.001, 0.05, by = 0.005))
rpart_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
rpart_model <- train(Class ~ ., data = trainData_Tomek_RFE, method = "rpart", trControl = rpart_trainControl, 
                     tuneGrid = rpartGrid, metric = "Accuracy" , parms = list(split = "gini"))
rpart_model

plot(rpart_model)
prp(rpart_model$finalModel, type = 2, extra = 104, fallen.leaves = TRUE, varlen = 0)

rpart_preds <- predict(rpart_model, testData_RFE)
rpart_cm <- confusionMatrix(rpart_preds, testData_RFE$Class)
rpart_cm

## ** ----------  STEP 7.3: Feature Selection Technique: CORR (Only df1) ---------- **
library(GGally)
library(dplyr)  
library(ggplot2)
library(patchwork) 

corr <- trainData_Tomek_df1
corr$Class_numeric <- ifelse(corr$Class == "Yes", 1, 0)

predictors <- corr %>% select(where(is.numeric)) %>% select(-Class_numeric) 
correlations <- sapply(predictors, function(x) cor(x, corr$Class_numeric, use = "complete.obs"))

corrDf <- data.frame(corrFeature = names(correlations), Correlation = correlations)
corrDf <- corrDf[order(abs(corrDf$Correlation), decreasing = TRUE), ] 

ggplot(corrDf, aes(x = reorder(corrFeature, Correlation), y = Correlation)) + geom_col() + coord_flip() +
  labs( title = "Correlation with Class (Yes = 1, No = 0)", x = "corrFeature", y = "Correlation" ) +
  theme_minimal() 

corrYes <- dplyr::slice( dplyr::arrange( dplyr::filter(corrDf, Correlation > 0), desc(Correlation)), 1:3)
corrYes 

corrNo <- dplyr::slice( dplyr::arrange( dplyr::filter(corrDf, Correlation < 0), (Correlation)), 1:3)
corrNo 

corrTopFeatures <- c(corrYes$corrFeature, corrNo$corrFeature)    
corrTopFeatures                       # df1 : Print corrTopFeatures  
length(corrTopFeatures)               # df1 : 6 corrTopFeatures found  

pairwise_data <- trainData_Tomek_df1 %>% select(all_of(corrTopFeatures), Class)
ggpairs(pairwise_data, upper = list(continuous = wrap("cor", size = 2)), diag = list(continuous = wrap("densityDiag")), lower = list(continuous = wrap("points", alpha = 0.6))) +
  theme_minimal()

corrMap <- setNames(corrDf$Correlation, corrDf$corrFeature)
plots <- lapply(corrTopFeatures, function(feature) { corr_val <- round(corrMap[feature], 2)

ggplot(trainData_Tomek_df1, aes_string(x = "Class", y = feature)) + geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = paste0(feature, " (r = ", corr_val, ")"), x = NULL, y = NULL) + theme_minimal() })
wrap_plots(plots, ncol = 2) + plot_annotation(title = "Scatterplots: Features vs. Class")

trainData_Tomek_CORR_df1 <- trainData_Tomek_df1[, c(corrTopFeatures, "Class")]
testData_CORR_df1 <- testData_df1[, c(corrTopFeatures, "Class")]

table(trainData_Tomek_CORR_df1$Class)     # df : Class distribution trainData_Tomek_CORR_df1 No: 134 and Yes: 134 
dim(trainData_Tomek_CORR_df1)             # df : trainData_Tomek_CORR_df1 dimensions 268   7 

table(testData_CORR_df1$Class)            # df : Class distribution testData_CORR_df1 No: 1200 and Yes: 95 
dim(testData_CORR_df1)                    # df : testData_CORR_df1 dimensions 1295   7 

## ** ----------  STEP 7.3.1:  TOMEK-CORR - Best Models (Only df1) ---------- **   
## MODEL 1 :  Random Forest 
## df1 : Sensitivity : 0.7817 
## df1 : Specificity : 0.8105 
library(UBL)
library(caret)

set.seed(123)
brfGrid <- expand.grid(mtry = c(1, 2, 3, 4, 5, 6 , 7 , 8 , 9, 10))
brf_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
brf_model <- randomForest(Class ~ .,  data = trainData_Tomek_CORR_df1, tuneGrid = brfGrid, trControl = brf_trainControl, ntree = 500)
brf_model

brf_preds <- predict(brf_model, testData_CORR_df1)
brf_cm <- confusionMatrix(brf_preds, testData_CORR_df1$Class)
brf_cm 

## MODEL 2 : Support Vector Machine:
## df1: Sensitivity : 0.8025 
## df1 : Specificity : 0.8105 
set.seed(123)
svmGrid <-  expand.grid(sigma = seq(0.01, 0.1, by = 0.01), C = seq(0.1, 1.0, by = 0.1))
#svmGrid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.05, 0.1))
svm_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)

set.seed(1401)
svm_model <- train( Class ~ ., data = trainData_Tomek_CORR_df1, metric = "ROC", method = "svmRadial", trControl = svm_trainControl, tuneGrid = svmGrid , preProcess = c("center", "scale"))
svm_model

plot(svm_model)

svm_preds <- predict(svm_model, testData_CORR_df1)
svm_cm <- confusionMatrix(svm_preds, testData_CORR_df1$Class)
svm_cm

### Alternate cutoffs
## df1 : Sensitivity : 0.8142 
## df1 : Specificity : 0.8105 
library(pROC)
svm_probs <- predict(svm_model, testData_CORR_df1, type = "prob")
roc_obj <- roc(testData_CORR_df1$Class, svm_probs[, "No"])
coords(roc_obj, "best", best.method = "closest.topleft")
plot(roc_obj)

svm_probs <- predict(svm_model, testData_CORR_df1, type = "prob")
svm_preds <- ifelse(svm_probs[, "No"] >= 0.4441497, "No", "Yes")
svm_preds <- factor(svm_preds, levels = levels(testData_CORR_df1$Class))

svm_cm <- confusionMatrix(svm_preds, testData_CORR_df1$Class)
svm_cm 

### Class-weighted SVM
## df1 : Sensitivity : 0.7925   
## df1 : Specificity : 0.8105 
library(kernlab)
set.seed(123)
sigma <- sigest(Class ~ ., data =  trainData_Tomek_CORR_df1 , frac = .75)
names(sigma) <-  NULL 
SVMwtsGrid <- data.frame(.sigma = sigma[2],  .C = 2^seq(-6, 1, length = 15))
SVMwts_trainControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)

set.seed(1401)
SVMwts_model <- train(Class ~ .,
                      data = trainData_Tomek_CORR_df1, method = "svmRadial", tuneGrid = SVMwtsGrid, preProc = c("center", "scale"), class.weights = c(Yes = 15, No = 17.2),
                      metric = "ROC",  trControl = SVMwts_trainControl)
SVMwts_model

plot(SVMwts_model)

SVMwts_preds <- predict(SVMwts_model, testData_CORR_df1)
SVMwt_cm <- confusionMatrix(SVMwts_preds, testData_CORR_df1$Class)
SVMwt_cm

## MODEL 3 : kNN: 
## df1 : Sensitivity : 0.7450 
## df1 : Specificity : 0.8632 
library(caret)

set.seed(123)
knnGrid <- expand.grid(k = seq(3, 25, by = 2)) 
knn_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
knn_model <- train(Class ~ ., data = trainData_Tomek_CORR_df1, method = "knn", trControl = knn_trainControl, tuneGrid = knnGrid, preProcess = c("center", "scale"))
knn_model

plot(knn_model)

knn_preds <- predict(knn_model, testData_CORR_df1)
knn_test_cm <- confusionMatrix(knn_preds, testData_CORR_df1$Class)
knn_test_cm

knn_model$bestTune
knn_model$results

## MODEL 4 : Naives Bayes:
## df1 : Sensitivity : 0.7942 
## df1 : Specificity : 0.8105 
library(e1071)  

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), fL = c(0, 0.5, 1), adjust = c(0.5, 1, 2))
nb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
nb_model <- train(Class ~ ., data = trainData_Tomek_CORR_df1,  method = "nb",  trControl = nb_trainControl, tuneGrid = nbGrid)
nb_model  

plot(nb_model)

nb_preds <- predict(nb_model, testData_CORR_df1)
nb_cm <- confusionMatrix(nb_preds, testData_CORR_df1$Class)
nb_cm

## MODEL 5 : Logistic Regression  
## df1 : Sensitivity : 0.7625 
## df1 : Specificity : 0.8105 
set.seed(123) 
lr_trainControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)

set.seed(1410)
lr_model <- train(Class ~ ., data = trainData_Tomek_CORR_df1, method = "glm", metric = "ROC" , trControl = lr_trainControl) 
lr_model

lr_preds <- predict(lr_model, testData_CORR_df1)
lr_cm <- confusionMatrix(lr_preds, testData_CORR_df1$Class)
lr_cm

## MODEL 6 : xgb 
## df1 : Sensitivity : 0.7850 
## df1 : Specificity : 0.8105 
library(caret)
library(xgboost)

set.seed(123)
xgbGrid <- expand.grid( nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3, 4),
                        gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
xgb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
xgb_model <- train(Class ~ ., data = trainData_Tomek_CORR_df1, method = "xgbTree", tuneGrid = xgbGrid, verbosity = 0, trControl = xgb_trainControl)
xgb_model

plot(xgb_model)

xgb_preds <- predict(xgb_model, testData_CORR_df1)
xgb_cm <- confusionMatrix(xgb_preds, testData_CORR_df1$Class)
xgb_cm

## EXTRA MODEL 7 : ADABOOST 
## df1 : Sensitivity : 0.7833  
## df1 : Specificity : 0.8000 
library(adabag)
library(caret)

set.seed(123)
boostingGrid <- expand.grid( mfinal = seq(50, 500, 50), maxdepth = c(1,3), coeflearn = "Freund")
boosting_trainControl <- trainControl( method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)

set.seed(1401)
boosting_model <- train( Class ~ ., data = trainData_Tomek_CORR_df1, method = "AdaBoost.M1", metric = "ROC", tuneGrid = boostingGrid, trControl = boosting_trainControl)
boosting_model

plot(boosting_model)

boosting_preds <- predict(boosting_model, testData_CORR_df1)
boosting_cm <- confusionMatrix(boosting_preds, testData_CORR_df1$Class)
boosting_cm 


## ** ----------  EXTRA FEATURE SELECTION TECHNIQUE : INFORMATION GAIN (Only df1) ---------- **
#install.packages("FSelectorRcpp") 
library(FSelectorRcpp) 

info_gain <- information_gain(Class ~ ., trainData_Tomek_df1)
info_gain$Attribute <- rownames(info_gain)
rownames(info_gain) <- NULL

names(info_gain)[names(info_gain) == "importance"] <- "Info_Gain"

sorted_info_gain <- info_gain[order(-info_gain$Info_Gain), ]
infoGainFeatures <- sorted_info_gain[1:6, ]

print(infoGainFeatures)               # Print infoGainFeatures 
nrow(infoGainFeatures)                # 6 infoGainFeatures found

infoGainFeatures <- infoGainFeatures$attributes

trainData_Tomek_InfoGain_df1 <- trainData_Tomek_df1[, c(infoGainFeatures, "Class")]
testDataInfoGain_df1 <- testData_df1[, c(infoGainFeatures, "Class")] 

table(trainData_Tomek_InfoGain_df1$Class)   # Class distribution trainData_Tomek_InfoGain_df1 No: 134 and Yes: 134 
dim(trainData_Tomek_InfoGain_df1)           # trainData_Tomek_InfoGain_df1 dimensions 268   7 

table(testDataInfoGain_df1$Class)           # Class distribution testDataInfoGain_df1 No: 1200 and Yes: 95   
dim(testDataInfoGain_df1)                   # testDataInfoGain_df1 dimensions 1295   7 

## ** ----------  STEP 7.3.1:  TOMEK-INFORMATION GAIN - Best Models (Only df1) ---------- **   
## MODEL 1 :  Random Forest 
## df1 : Sensitivity : 0.8008   
## df1 : Specificity : 0.8105 
library(UBL)
library(caret)

set.seed(123)
brfGrid <- expand.grid(mtry = c(1, 2, 3, 4, 5, 6 , 7 , 8 , 9 , 10))
brf_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
brf_model <- randomForest(Class ~ .,  data = trainData_Tomek_InfoGain_df1, tuneGrid = brfGrid, trControl = brf_trainControl, ntree = 500)
brf_model

brf_preds <- predict(brf_model, testDataInfoGain_df1)
brf_cm <- confusionMatrix(brf_preds, testDataInfoGain_df1$Class)
brf_cm 

## MODEL 2 : Support Vector Machine:
## df1 : Sensitivity : 0.8333 
## df1 : Specificity : 0.7684 
set.seed(123)
svmGrid <-  expand.grid(sigma = seq(0.01, 0.1, by = 0.01), C = seq(0.1, 1.0, by = 0.1))
#svmGrid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.05, 0.1))
svm_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)

set.seed(1401)
svm_model <- train( Class ~ ., data = trainData_Tomek_InfoGain_df1, metric = "ROC", method = "svmRadial", trControl = svm_trainControl, tuneGrid = svmGrid , preProcess = c("center", "scale"))
svm_model

plot(svm_model)

svm_preds <- predict(svm_model, testDataInfoGain_df1)
svm_cm <- confusionMatrix(svm_preds, testDataInfoGain_df1$Class)
svm_cm 

## MODEL 3 : kNN:
## df1 : Sensitivity : 0.7625 
## df1 : Specificity : 0.8105  
library(caret)

set.seed(123)
knnGrid <- expand.grid(k = seq(3, 30, by = 2)) 
knn_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
knn_model <- train(Class ~ ., data = trainData_Tomek_InfoGain_df1, method = "knn", trControl = knn_trainControl, tuneGrid = knnGrid, preProcess = c("center", "scale"))
knn_model

plot(knn_model)

knn_preds <- predict(knn_model, testDataInfoGain_df1)
knn_test_cm <- confusionMatrix(knn_preds, testDataInfoGain_df1$Class)
knn_test_cm

knn_model$bestTune
knn_model$results

## MODEL 4 : Naives Bayes:
## df1 : Sensitivity : 0.7292  
## df1 : Specificity : 0.8526 
library(e1071)  

set.seed(123)
nbGrid <- expand.grid(usekernel = c(TRUE, FALSE), fL = c(0, 0.5, 1, 2, 3 , 4), adjust = c(0.5, 1, 2))
nb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
nb_model <- train(Class ~ ., data = trainData_Tomek_InfoGain_df1,  method = "nb",  trControl = nb_trainControl, tuneGrid = nbGrid)
nb_model  

plot(nb_model)

nb_preds <- predict(nb_model, testDataInfoGain_df1)
nb_cm <- confusionMatrix(nb_preds, testDataInfoGain_df1$Class)
nb_cm

## MODEL 5 : RPART 
## df1 : Sensitivity : 0.8300
## df1 : Specificity : 0.7263  
library(rpart)
library(rpart.plot)

set.seed(123)
rpartGrid <- expand.grid(cp = seq(0.001, 0.05, by = 0.005))
rpart_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
rpart_model <- train(Class ~ ., data = trainData_Tomek_InfoGain_df1, method = "rpart", trControl = rpart_trainControl, 
                     tuneGrid = rpartGrid, metric = "Accuracy" , parms = list(split = "gini"))
rpart_model

plot(rpart_model)
prp(rpart_model$finalModel, type = 2, extra = 104, fallen.leaves = TRUE, varlen = 0)

rpart_preds <- predict(rpart_model, testDataInfoGain_df1)
rpart_cm <- confusionMatrix(rpart_preds, testDataInfoGain_df1$Class)
rpart_cm

## MODEL 6 : Logistic Regression 
## df1 : Sensitivity : 0.8025
## df1 : Specificity : 0.7789 
set.seed(123) 
lr_trainControl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)

set.seed(1410)
lr_model <- train(Class ~ ., data = trainData_Tomek_InfoGain_df1, method = "glm", metric = "ROC" , trControl = lr_trainControl) 
lr_model

plot(lr_model)

lr_preds <- predict(lr_model, testDataInfoGain_df1)
lr_cm <- confusionMatrix(lr_preds, testDataInfoGain_df1$Class)
lr_cm 

## EXTRA MODEL 7 : xgboost 
## df1 : Sensitivity : 0.7992 
## df1 : Specificity : 0.8000 
library(caret)
library(xgboost)

set.seed(123)
xgbGrid <- expand.grid( nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3, 4),
                        gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
xgb_trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

set.seed(1401)
xgb_model <- train(Class ~ ., data = trainData_Tomek_InfoGain_df1, method = "xgbTree", tuneGrid = xgbGrid, verbosity = 0, trControl = xgb_trainControl)
xgb_model

plot(xgb_model)

xgb_preds <- predict(xgb_model, testDataInfoGain_df1)
xgb_cm <- confusionMatrix(xgb_preds, testDataInfoGain_df1$Class)
xgb_cm



















