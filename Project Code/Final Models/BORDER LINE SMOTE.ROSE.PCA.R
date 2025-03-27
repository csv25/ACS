# Working directory - Setting/Getting
getwd()
# Set working directory

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### PACKAGES :   
library(naniar)

#Helping with running ML Models
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

##Naive Bayes
library(naivebayes)


#Neural Networks
library(nnet)

#KNN
library(class)

# SVM Radial
library(e1071)

#Logistic Regression

#Random Forest
library(randomForest)

#OverSampling
library(smotefamily)

#Under Sampling
library(ROSE) 

## PCA 
library(FactoMineR)
library(factoextra)


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
#Start of the Document
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

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# MODEL TRAINING :  

df3 <- df2 

df3$Class <- ifelse(df3$Class == "Yes", 1, 0)
df3$Class <- as.factor(df3$Class)
table(df3$Class)

set.seed(123)
trainIndex <- createDataPartition(df3$Class, p = 0.7, list = FALSE)

#Breaking training data
trainData <- df3[trainIndex, ]
dim(trainData)
#Breaking Testing Data
testData <- df3[-trainIndex, ]
dim(testData)

print(table(trainData$Class))
print(table(testData$Class)) 

##### ##### ##### ##### ##### ##### ##### ##### 

## HYBRID APPROACH ON TRAIN DATA 
# not having class for the training data
set.seed(123)
X <- trainData[, -which(names(trainData) == "Class")]
X <- as.data.frame(lapply(X, as.numeric))

# This is the Class variable from the training data
Y <- trainData$Class
Y <- as.factor(Y)
length(Y)

X <- as.data.frame(scale(X)) 
str(X)
dim(X)
length(X)

###############################################
# SMOTE Border Line
borderline_smote_result <- BLSMOTE(X, Y, K = 5, C = 5, dupSize = 0, method = "type1")
trainData_SMOTE <- borderline_smote_result$data
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
table(trainData_SMOTE$Class)
##############################################
#ROSE
trainData_SMOTE.ROSE <- ovun.sample(Class ~ ., 
                                    data = trainData_SMOTE, 
                                    method = "under", 
                                    N = sum(trainData_SMOTE$Class == 1) * 2,
                                    seed = 123)$data
table(trainData_SMOTE.ROSE$Class)
dim(trainData_SMOTE.ROSE)

## Preparing training data for PCA

X_balanced <- trainData_SMOTE.ROSE[,-which(names(trainData_SMOTE.ROSE)== 'Class')]
dim(X_balanced)
Y_balanced <- trainData_SMOTE.ROSE$Class
length(Y_balanced)

###############################################
#Adding PCA to the Model
#Creating new dataframe for our training data
df_smote_rose_pca <- PCA(X_balanced, graph = FALSE)
df_smote_rose_pca

#SRP = smote + Rose + PCA
trainDataSRP <- data.frame(df_smote_rose_pca$ind$coord, 
                                       Class = as.factor(Y_balanced))

trainDataSRP
table(trainDataSRP$Class)

set.seed(123)
#### Setting the testing dataframe to be ran with PCA 

x_test <- testData[,-which(names(testData)== 'Class')]
x_test <- as.data.frame(lapply(x_test, as.numeric))

Y <- testData$Class
Y <- as.factor(Y)

x_test <- as.data.frame(scale(x_test))

## Converting test dataset to PCA format

testData_PCA <- PCA(x_test, graph =FALSE)
testData_PCA <- data.frame(testData_PCA$ind$coord, Class = as.factor(Y))
testData_PCA
table(testData_PCA$Class)

############################################

#Checking structure of training data with SMOTE + Rose + PCA

trainDataSRP
dim(trainDataSRP)
summary(trainDataSRP)

testData_PCA
dim(testData_PCA)
summary(testData_PCA)

## Re-naming Columns to have a clearer name for interpretation 

colnames(trainDataSRP)[1:5] <- paste0("PC", 1:5)
colnames(trainDataSRP)
colnames(testData_PCA)[1:5] <- paste0("PC", 1:5)
colnames(testData_PCA)

#############################################################

# 1: K-NN algorithm

train.control <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE)

knn.grid <- expand.grid(k=c(3,4,5,6,7,8,9,11,13,14))

knn.model <- train(Class~., 
                   data = trainDataSRP,
                   method = 'knn',
                   tuneGrid = knn.grid,
                   trControl = train.control,
                   preProcess = c('center', 'scale'),
                   tuneLength = 10)

knn.model

knn.pred <- predict(knn.model, newdata = testData_PCA)
knn.pred

cm <- table(testData_PCA$Class, knn.pred)
cm

confusionMatrix(cm)


#############################################################
# KNN without using the Caret Package to perform Cross Validation

################################
#Finding the best K through the elbow method

######### FINDING OPTIMAL K 
k_values <- seq(1, 20, by = 2)
accuracy_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_predictions <- knn(train = trainDataSRP[, !names(trainDataSRP) %in% "Class"], 
                         test = testData_PCA[, !names(testData_PCA) %in% "Class"], 
                         cl = trainDataSRP$Class, 
                         k = k)
  
  accuracy_scores[i] <- sum(knn_predictions == testData_PCA$Class) / length(testData_PCA$Class)
}
accuracy_df <- data.frame(K = k_values, Accuracy = accuracy_scores)

print(accuracy_df[accuracy_df$K <= 20, ])

ggplot(accuracy_df, aes(K, Accuracy)) +
  geom_point(size=3) +
  geom_line() +
  xlab("k") + ylab("accuracy") +
  theme_minimal()

###############################################################
#Placing the best K found from the Elbow Method
#Best K: 3 gives higher sensitivity and specificity

knn_predictions <- knn(train = trainDataSRP, 
                       test = testData_PCA, 
                       cl = trainDataSRP$Class, 
                       k = 3)

plot(knn_predictions)

knn_test_cm <- confusionMatrix(knn_predictions, testData_PCA$Class)
knn_test_cm

################################################################
# 2: Naive Bayes
set.seed(123)

nb.grid <- expand.grid(usekernel = c(TRUE, FALSE),
                       laplace=c(0,1),
                       adjust = c(0.25, 0.50, 0.75, 1))

nb.tr.control <- trainControl(method = 'cv',
                              number = 10,
                              verboseIter = TRUE)

nb.model <- train(Class ~., 
                  data = trainDataSRP,
                  method = 'naive_bayes',
                  tuneGrid = nb.grid,
                  trControl = nb.tr.control)
nb.model
## Testing our model with our testing data
nb.pred <- predict(nb.model, newdata= testData_PCA)
nb.pred

cm <- table(testData_PCA$Class, nb.pred)
cm

confusionMatrix(cm)

###################################################################
# 3: Support Vector Machine (SVM)
set.seed(123)
svm_model <- svm(Class ~ ., data = trainDataSRP, 
                 kernel = "radial", 
                 cost = c(0.1, 1, 10, 100,150),
                 gamma = c(0.001,0.01, 0.1, 1, 10),
                 degree = 3,
                 type = "C-classification",
                 tolerance = 0.0005,
                 class.weights = c("0"=1, "1"=1.5),
                 probability = TRUE)

svm_model
## Testing the model with our unseen data
svm_test_predictions <- predict(svm_model, testData_PCA, probability = TRUE)
svm_test_cm <- confusionMatrix(svm_test_predictions, testData_PCA$Class) 
svm_test_cm

#########################################################
# 4: Neural Networks NNET

set.seed(125)

nnet.grid <- expand.grid(size = c(0.05, 0.07, 1, 2),
                         decay = c(0.0001,0.001, 0.01, 0.1, 0.5,1,2))

nnet.trainControl <- trainControl(
  method = 'cv',
  number = 10,
  verboseIter = TRUE
)

nnet.model <- train(Class ~., 
                    data = trainDataSRP,
                    method = 'nnet',
                    tuneGrid = nnet.grid,
                    trControl = nnet.trainControl,
                    preProcess= c('center', 'scale'),
                    trace = FALSE)

nnet.model

#Running the model on the test data
nnet_pred <- predict(nnet.model, newdata = testData_PCA)
nnet_test_cm <- confusionMatrix(nnet_pred, testData_PCA$Class) 
nnet_test_cm

####################################################################
# 5: LOGISTIC REGRESSION

lr_model <- train(Class ~ ., 
                  data = trainDataSRP,
                  method = "glm",
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 10))
lr_model

lr_test_predictions <- predict(lr_model, testData_PCA)
lr_test_cm <- confusionMatrix(lr_test_predictions, testData_PCA$Class)
lr_test_cm 

#######################################################################
# 6: RANDOM FORESTS

rf_model <- randomForest(Class ~ ., data = trainDataSRP,
                         ntree = 1500, 
                         importance = TRUE, 
                         classwt = c(1, 10),
                         maxnodes = 300,
                         replace = FALSE)

rf_model

rf_test_predictions <- predict(rf_model, testData_PCA)
rf_test_cm <- confusionMatrix(rf_test_predictions, testData_PCA$Class)
rf_test_cm 

# Random Search
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3, 
                        search="grid",
                        verboseIter = TRUE)
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:15))

rf_gridsearch <- train(Class~., 
                       data=trainDataSRP, 
                       method="rf", 
                       metric= 'Kappa', 
                       tuneGrid=tunegrid, 
                       trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)




















