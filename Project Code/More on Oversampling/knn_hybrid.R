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

##### ##### ##### ##### ##### ##### ##### ##### HYBRID APPROACH ON TRAIN DATA 
set.seed(123)
X <- trainData[, -which(names(trainData) == "Class")]
X <- as.data.frame(lapply(X, as.numeric))

Y <- trainData$Class
Y <- as.factor(Y)

X <- as.data.frame(scale(X)) 

## SMOTE BORDERLINE 
library(smotefamily) 
borderline_smote_result <- BLSMOTE(X, Y, K = 5, C = 5, dupSize = 0, method = "type1")
trainData_SMOTE <- borderline_smote_result$data
colnames(trainData_SMOTE)[ncol(trainData_SMOTE)] <- "Class"
trainData_SMOTE$Class <- as.factor(trainData_SMOTE$Class)
table(trainData_SMOTE$Class)

## ROSE  
#install.packages("ROSE")
library(ROSE)  
trainData_SMOTE <- ovun.sample(Class ~ ., data = trainData_SMOTE, method = "under", N = sum(trainData_SMOTE$Class == 1) * 2, seed = 123)$data
table(trainData_SMOTE$Class)  

## PCA 
library(FactoMineR)
library(factoextra)

X_balanced <- trainData_SMOTE[, -which(names(trainData_SMOTE) == "Class")]
Y_balanced <- trainData_SMOTE$Class

pca_smote <- PCA(X_balanced, graph = FALSE)
trainData_SMOTE <- data.frame(pca_smote$ind$coord, Class = as.factor(Y_balanced))

ggplot(trainData_SMOTE, aes(x = Dim.1, y = Dim.2, color = Class)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Resampling Dataset (AFTER SMOTE AND ROSE)", x = "PC1", y = "PC2") +
  theme_minimal()

table(trainData_SMOTE$Class)
#dim(trainData_SMOTE)
#str(trainData_SMOTE)

##### ##### ##### ##### ##### ##### ##### ##### HYBRID APPROACH ON TRAIN DATA 

##### ##### ##### ##### ##### ##### ##### ##### TESTING DATA 
table(testData$Class)

set.seed(123)
x_test <- testData[, -which(names(testData) == "Class")]
x_test <- as.data.frame(lapply(x_test, as.numeric))

Y <- testData$Class
Y <- as.factor(Y)

x_test <- as.data.frame(scale(x_test)) 

testData_PCA <- PCA(x_test, graph = FALSE)
testData_PCA <- data.frame(testData_PCA$ind$coord, Class = as.factor(Y))

ggplot(testData_PCA, aes(x = Dim.1, y = Dim.2, color = Class)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Resampled Dataset (AFTER SMOTE AND ROSE)", x = "PC1", y = "PC2") +
  theme_minimal()
##### ##### ##### ##### ##### ##### ##### ##### TESTING DATA 

##### ##### KNN
library(class)

print(dim(trainData_SMOTE))  
print(dim(testData_PCA)) 

colnames(trainData_SMOTE)[1:5] <- paste0("PC", 1:5)
colnames(testData_PCA)[1:5] <- paste0("PC", 1:5)

summary(trainData_SMOTE)
summary(testData_PCA)

knn_predictions <- knn(train = trainData_SMOTE, 
                       test = testData_PCA, 
                       cl = trainData_SMOTE$Class, 
                       k = 5)

plot(knn_predictions)

knn_test_cm <- confusionMatrix(knn_predictions, testData_PCA$Class)
knn_test_cm

knn_test_cm <- confusionMatrix(knn_predictions, testData_PCA$Class, positive = '1')
knn_test_cm 

######### FINDING OPTIMAL K 
k_values <- seq(1, 20, by = 2)
accuracy_scores <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_predictions <- knn(train = trainData_SMOTE[, !names(trainData_SMOTE) %in% "Class"], 
                         test = testData_PCA[, !names(testData_PCA) %in% "Class"], 
                         cl = trainData_SMOTE$Class, 
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






