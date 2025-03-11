# Working directory - Setting/Getting
getwd()
# Set working directory
setwd('/Users/melanieloaiza/Desktop/BU - Data Science /Spring 2025/MET CS699 - Data Mining/project_assignment')

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

df1 <- df1[, !names(df1) %in% c("SERIALNO", "SPORDER" , "RT")]

dim(df1) 
summary(df1) 
str(df1)

n_miss(df1) 
prop_miss(df1) 
miss_var_summary(df1) %>% print(n = Inf)

##### ##### ##### ##### LOW VARIANCE :  
#zerovar <- nearZeroVar(df1, names = TRUE) 
#print(zerovar) 

#near_zerovar <- nearZeroVar(df1, saveMetrics = TRUE)
#near_zerovar

#var_near_zerovar <- rownames(near_zerovar[near_zerovar$zeroVar == TRUE, ])
#var_near_zerovar

zerovar_1 <- nearZeroVar(df1, names = TRUE, uniqueCut = 6)
zerovar_1

df1 <- df1 %>% select(-all_of(zerovar_1))  

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

##### ##### SMOTE AND FEATURE SELECTION : 
library(smotefamily) 

# SMOTE
set.seed(123)
X <- trainData[, -which(names(trainData) == "Class")]
X <- as.data.frame(lapply(X, as.numeric))

Y <- trainData$Class
Y <- trainData$Class

smote_result <- SMOTE(X, Y, K = 3)
trainData_SMOTE <- smote_result$data
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
table(trainData_SMOTE$Class)  

# 2 : 
set.seed(123)
rfe_control <- rfeControl(functions = rfFuncs, method = "repeatedcv",  number = 7,  repeats = 3,  
                          returnResamp = "all", verbose = TRUE) 
rfe_model <- rfe(trainData_SMOTE[, -ncol(trainData_SMOTE)], trainData_SMOTE$Class, sizes = seq(5, 15, by = 5),  rfeControl = rfe_control, 
                 ntree = 100) 
selected_features_rfe <- predictors(rfe_model) 
selected_features_rfe

importance_scores <- varImp(rfe_model) 
str(importance_scores) 

importance_df <- as.data.frame(importance_scores) 
str(importance_df)

top_features <- rownames(importance_df[importance_df$Overall > 5, , drop = FALSE])
top_features

testData <- testData %>% mutate(across(-Class, as.numeric))
testData_selected <- testData[, c(top_features, "Class")] 
testData_selected

##### #####  5.  NAIVE BAYES 
library(e1071)

# 1. 
nb_model <- naiveBayes(Class ~ ., data = trainData_SMOTE[, c(top_features, "Class")] , laplace = 1 , usekernel = TRUE) ### BEST MODEL  
print(nb_model)

nb_train_predictions <- predict(nb_model, trainData_SMOTE[, c(selected_features_rfe, "Class")], type = "class")
nb_train_cm <- confusionMatrix(nb_train_predictions, trainData_SMOTE$Class)
nb_train_cm

nb_test_predictions <- predict(nb_model, testData_selected)
nb_test_cm <- confusionMatrix(nb_test_predictions, testData_selected$Class)
nb_test_cm

nb_test_predictions_1 <- predict(nb_model, testData_selected)
nb_test_cm_1  <- confusionMatrix(nb_test_predictions, testData_selected$Class, positive = '1')
nb_test_cm_1

str(nb_test_cm)

## Printing the results 
extract_measures <- function(conf_matrix) {
  by_class <- conf_matrix$byClass
  mcc <- ifelse("MCC" %in% names(by_class), by_class["MCC"], NA)
  
  measures <- c(
    by_class["Sensitivity"],  
    1 - by_class["Specificity"],  
    by_class["Pos Pred Value"],  
    by_class["Sensitivity"],  
    by_class["F1"],  
    conf_matrix$overall["Accuracy"],  
    mcc,  # MCC
    conf_matrix$overall["Kappa"] 
  )
  
  return(measures)
}

class_no_measures <- extract_measures(nb_test_cm)
class_yes_measures <- extract_measures(nb_test_cm_1)

total_instances <- sum(nb_test_cm$table)
class_no_weight <- sum(nb_test_cm$table[1, ])
class_yes_weight <- sum(nb_test_cm$table[2, ])

wt_avg <- (class_no_measures * class_no_weight + class_yes_measures * class_yes_weight) / total_instances

performance_measures_df <- data.frame(
  Class = c("Class No", "Class Yes", "Wt. Average"),
  TPR = c(class_no_measures[1], class_yes_measures[1], wt_avg[1]),
  FPR = c(class_no_measures[2], class_yes_measures[2], wt_avg[2]),
  Precision = c(class_no_measures[3], class_yes_measures[3], wt_avg[3]),
  Recall = c(class_no_measures[4], class_yes_measures[4], wt_avg[4]),
  F_measure = c(class_no_measures[5], class_yes_measures[5], wt_avg[5]),
  ROC = c(class_no_measures[6], class_yes_measures[6], wt_avg[6]),
  MCC = c(class_no_measures[7], class_yes_measures[7], wt_avg[7]),
  Kappa = c(class_no_measures[8], class_yes_measures[8], wt_avg[8])
)

print(performance_measures_df)








