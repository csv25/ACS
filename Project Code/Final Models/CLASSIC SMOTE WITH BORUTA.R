# Working directory - Setting/Getting
getwd()
# Set working directory
# setwd('/Users/melanieloaiza/Desktop/BU - Data Science /Spring 2025/MET CS699 - Data Mining/project_assignment')

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)

##### ##### CLASSIC SMOTE: 
library(smotefamily) 

##### ##### BORUTA: 
library(Boruta)

# 1. RANDOM FOREST 
library(randomForest) 

# 2. LOGISTIC REGRESSION 

# 3: SMV
library(e1071)

# 4 : NAIVE BAYES 

# 5 :   NEURAL NETWORKS
library(nnet)

# 6.  KNN
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

##### ##### ##### ##### MISSING VALUES :  
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

##### ##### ##### ##### LOW VARIANCE :  
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
library(smotefamily) 

# SMOTE
set.seed(123)
X <- trainData[, -which(names(trainData) == "Class")]
X <- as.data.frame(lapply(X, as.numeric))

Y <- trainData$Class
Y <- trainData$Class

smote_result <- SMOTE(X, Y, K = 5)

trainData_SMOTE <- smote_result$data
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"

trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
table(trainData_SMOTE$Class)


##################################################

## BORUTA  
library(Boruta)
set.seed(123)

boruta_model <- Boruta(Class ~ ., data = trainData_SMOTE, doTrace = 0)
selected_features_boruta <- getSelectedAttributes(boruta_model, withTentative = FALSE)
print(selected_features_boruta)


testData <- testData %>% mutate(across(-Class, as.numeric))
testData[var_numeric_test] <- scale(testData[var_numeric_test])
testData_selected <- testData[, c(selected_features_boruta, "Class")] 

#####################################################
# Random Forests
set.seed(123)
# Does not meet minimum requirements
# Sensitivity : 0.8700          
# Specificity : 0.6737
rf_model <- randomForest(Class ~ ., 
                         data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                         ntree = 2000, 
                         importance = TRUE, 
                         classwt = c(1, 15),
                         maxnodes = 250,
                         replace = FALSE,
                         sampsize = c(500, 500))
rf_model


rf_test_predictions <- predict(rf_model, testData_selected)
rf_test_cm <- confusionMatrix(rf_test_predictions, testData_selected$Class)
rf_test_cm 

###############################################################

# 2: Logistic Regression
# Does not meet the Minimum Requirement
# Sensitivity : 0.8600          
# Specificity : 0.7263
lr_model <- train(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")],
                  method = "glm",
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 5),
                  weights = ifelse(trainData_SMOTE$Class == 0, 1.5, 1))

lr_model
lr_test_predictions <- predict(lr_model, testData_selected)
lr_test_cm <- confusionMatrix(lr_test_predictions, testData_selected$Class)
lr_test_cm 

#################################################################

# 3 : SVM
set.seed(123)
# Meets Minimum Requirements
# Sensitivity : 0.8083          
# Specificity : 0.8316

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                 kernel = "radial", 
                 cost = c(0.1, 1, 10, 50, 100,150),
                 gamma = c(0.001, 0.01, 0.1, 1),
                 probability = TRUE,
                 tolerance = 0.05,
                 class.weights = c("0"=1, "1"=1.3))

svm_model

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

#################################################################
# 4 : Naive Bayes
set.seed(125)
# Meets minimum requirements
# Sensitivity : 0.7925         
# Specificity : 0.8316 

nb.grid <- expand.grid(usekernel = c(TRUE, FALSE),
                       laplace=c(0,1),
                       adjust = c(1, 2, 3, 4))

train.control <- trainControl(method = 'cv',
                              number = 5,
                              verboseIter = TRUE)

nb_model <- train(Class ~., 
                  data = trainData_SMOTE[, c(selected_features_boruta, "Class")],
                  method = 'naive_bayes',
                  tuneGrid = nb.grid,
                  trControl = train.control)
nb_model

nb_test_predictions <- predict(nb_model, testData_selected, probability = TRUE)
nb_test_cm <- confusionMatrix(nb_test_predictions, testData_selected$Class) 
nb_test_cm

################################################################

# 5:Neural Networks
# Does not meet minimum Requirements
# Sensitivity : 0.8567          
# Specificity : 0.6421 

set.seed(125)

nnet.grid <- expand.grid(size = c(1, 2,3),
                         decay = c(0.01, 0.1, 0.5,1,2,3))

nnet.trainControl <- trainControl(
  method = 'cv',
  number = 5,
  verboseIter = TRUE
)

nnet.model <- train(Class ~., 
                    data = trainData_SMOTE[, c(selected_features_boruta, "Class")],
                    method = 'nnet',
                    tuneGrid = nnet.grid,
                    trControl = nnet.trainControl,
                    preProcess= c('center', 'scale'),
                    trace = FALSE)

nnet.model

nnet_test_predictions <- predict(nnet.model, testData_selected, probability = TRUE)
nnet_test_cm <- confusionMatrix(nnet_test_predictions, testData_selected$Class) 
nnet_test_cm


##############################################
# 6 : KNN
# Does not meet requirements
# Sensitivity : 0.8367          
# Specificity : 0.5895 
knn.grid <- expand.grid(k=c(4,5,6,7))

knn.control <- trainControl(
  method = 'cv',
  number = 10,
  verboseIter = TRUE
)

knn.model <- train(Class ~., 
                   data = trainData_SMOTE[, c(selected_features_boruta, "Class")],
                   method = 'knn',
                   tuneGrid = knn.grid,
                   trControl = knn.control,
                   preProcess = c('center', 'scale'))
knn.model

knn_test_predictions <- predict(knn.model, testData_selected, probability = TRUE)
knn_test_cm <- confusionMatrix(knn_test_predictions, testData_selected$Class) 
knn_test_cm

