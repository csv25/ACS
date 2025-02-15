#reading file
# Load necessary package
library(caret)

df <- read.csv("cleaned_data.csv", header = TRUE, sep = ",")
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

# Train logistic regression model
log_model <- glm(df$Class ~ ., data = train_data, family = binomial)

# Print model summary
summary(log_model)


