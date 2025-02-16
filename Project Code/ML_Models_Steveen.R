# ########################################################################
# Running models:
#reading file
# Load necessary package
library(caret)

df <- read.csv("df_cleaned.csv", header = TRUE, sep = ",")
df7$Class <- as.factor(df7$Class)
df <- subset(df, select = -c(INTP))
dim(df)

# Set a random seed for reproducibility
set.seed(42)

# Split data: 70% training, 30% testing
split_index <- createDataPartition(df$Class, p = 0.7, list = FALSE)

# Create training and testing datasets
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

# Print dimensions to verify
print(dim(train_data))  # Should be ~70% of df
print(dim(test_data))   # Should be ~30% of df

# Train the SVM model
svm_model <- svm(Class ~ ., data = train_data, kernel = "linear", cost = 1)

# Print model details
print(svm_model)

# Make predictions on the test set
svm_predictions <- predict(svm_model, newdata = test_data)

# Print first few predictions
head(svm_predictions)

# Load caret package (if not already loaded)
library(caret)

# Create confusion matrix
conf_matrix <- confusionMatrix(svm_predictions, test_data$Class)

# Print confusion matrix
print(conf_matrix)


#trying out BORUTA from class
bone.marrow.boruta <- Boruta(Class ~ ., data = df, doTrace = 2, maxRuns = 50)
bone.marrow.boruta
getSelectedAttributes(bone.marrow.boruta)
plot(bone.marrow.boruta)
bone.marrow.boruta <- TentativeRoughFix(bone.marrow.boruta)
print(getSelectedAttributes(bone.marrow.boruta))

#running melanies code
# df7_cleaned.csv

df7 <- read.csv("df7_cleaned.csv", header = TRUE, sep = ',')
# df7

df7$Class <- as.factor(df7$Class)

bone.marrow.boruta <- Boruta(Class ~ ., data = df7, doTrace = 2, maxRuns = 50)
bone.marrow.boruta
getSelectedAttributes(bone.marrow.boruta)
plot(bone.marrow.boruta)
bone.marrow.boruta <- TentativeRoughFix(bone.marrow.boruta)
print(getSelectedAttributes(bone.marrow.boruta))