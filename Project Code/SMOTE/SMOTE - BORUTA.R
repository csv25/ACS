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

##### ##### ##### ##### ##### #####  BORUTA  
library(Boruta)
set.seed(123)

boruta_model <- Boruta(Class ~ ., data = trainData_SMOTE, doTrace = 0)
selected_features_boruta <- getSelectedAttributes(boruta_model, withTentative = FALSE)
print(selected_features_boruta)


testData <- testData %>% mutate(across(-Class, as.numeric))
testData[var_numeric_test] <- scale(testData[var_numeric_test])
testData_selected <- testData[, c(selected_features_boruta, "Class")] 

#print(testData_selected)
#dim(testData_selected)

##### #####  1. RANDOM FOREST 
library(randomForest) 

# 1 . 
rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                         ntree = 500,  importance = TRUE)

# 2. 
rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")],
                         ntree = 1000, importance = TRUE, classwt = c(1, 5))  

# 3. 
rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                         ntree = 1500, importance = TRUE, classwt = c(1, 10))

# 4. 
rf_model <- randomForest(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                         ntree = 1000, importance = TRUE, sampsize = c(500, 500))  

print(rf_model)
rf_train_predictions <- predict(rf_model, trainData_SMOTE[, c(selected_features_boruta, "Class")])
rf_train_cm <- confusionMatrix(rf_train_predictions, trainData_SMOTE$Class)
rf_train_cm 

rf_test_predictions <- predict(rf_model, testData_selected)
rf_test_cm <- confusionMatrix(rf_test_predictions, testData_selected$Class)
rf_test_cm 

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

# 1. 
rf_balanced_model <- ranger(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
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

rf_balanced_train_predictions <- predict(rf_balanced_model, trainData_SMOTE[, c(selected_features_boruta, "Class")])
rf_balanced_train_cm <- confusionMatrix(rf_balanced_train_predictions, trainData_SMOTE$Class)
rf_balanced_train_cm

rf_balanced_test_predictions <- predict(rf_balanced_model, testData_selected)
rf_balanced_test_cm <- confusionMatrix(rf_balanced_test_predictions, testData_selected$Class)
rf_balanced_test_cm 

##### #####  4.  SUPPORT VECTOR MACHINE 
library(e1071)

svm_model <- svm(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                 kernel = "radial", 
                 cost = 1, 
                 probability = TRUE)

print(svm_model)

svm_train_predictions <- predict(svm_model, trainData_SMOTE[, c(selected_features_boruta, "Class")])
svm_train_cm <- confusionMatrix(svm_train_predictions, trainData_SMOTE$Class)
svm_train_cm

svm_test_predictions <- predict(svm_model, testData_selected, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_selected$Class) 
svm_test_cm

##### #####  5.  NAIVE BAYES 
library(e1071)

# 1. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")])

# 2. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], usekernel = TRUE)

# 3. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], laplace = 1)

print(nb_model)

nb_train_predictions <- predict(nb_model, trainData_SMOTE[, c(selected_features_boruta, "Class")], type = "class")
nb_train_cm <- confusionMatrix(nb_train_predictions, trainData_SMOTE$Class)
nb_train_cm

nb_test_predictions <- predict(nb_model, testData_selected)
nb_test_cm <- confusionMatrix(nb_test_predictions, testData_selected$Class)
nb_test_cm

##### #####  6.  NEURAL NETWORKS 
library(nnet)

nn_model <- nnet(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                 size = 10, decay = 0.01, maxit = 200)
print(nn_model)

nn_train_predictions <- predict(nn_model, trainData_SMOTE[, c(selected_features_boruta, "Class")])
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

cart_model <- rpart(Class ~ ., data = trainData_SMOTE[, c(selected_features_boruta, "Class")], 
                    method = "class", control = rpart.control(cp = 0.01))
print(cart_model)

cart_train_predictions <- predict(cart_model, trainData_SMOTE[, c(selected_features_boruta, "Class")], type = "class")
cart_train_cm <- confusionMatrix(cart_train_predictions, trainData_SMOTE$Class)
cart_train_cm

cart_test_predictions <- predict(cart_model, testData_selected, type = "class")
cart_test_cm <- confusionMatrix(cart_test_predictions, testData_selected$Class)
cart_test_cm

##### #####  8.  KNN
library(class)

knn_predictions <- knn(train = trainData_SMOTE[, selected_features_boruta], 
                       test = testData_selected[, selected_features_boruta], 
                       cl = trainData_SMOTE$Class, k = 5)

knn_test_cm <- confusionMatrix(knn_predictions, testData_selected$Class)
knn_test_cm

##### #####  9.  XGBoost (Extreme Gradient Boosting)
library(xgboost)

train_matrix <- xgb.DMatrix(data = as.matrix(trainData_SMOTE[, selected_features_boruta]), 
                            label = as.numeric(trainData_SMOTE$Class) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(testData_selected[, selected_features_boruta]))

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