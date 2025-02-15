###################################### DATA : 
# Working directory - Setting/Getting
getwd()
# Set working directory
#setwd('/Users/melanieloaiza/Desktop/BU - Data Science /Spring 2025/MET CS699 - Data Mining/project_assignment')

# PACKAGES : 
##install.packages("dplyr")  
library(dplyr)

##install.packages("caret")  
library(caret)

##install.packages("tidyverse")  
library(tidyr)

library(ggplot2)

library(gridExtra)

library(ggcorrplot)

df <- read.csv('project_data.csv') 
head(df)  

dim(df)
str(df)
summary(df) 

colSums(is.na(df))

# Step 1 :  check and remove duplicates df1 
df1 <- df 
df1 <- df[!duplicated(df), ] 
print(nrow(df1_duplicates)) # No duplicates found   

# Step 2 : check and remove missing values  
df2 <- df1  

colSums(is.na(df2))
sum(is.na(df2)) # total NA 141 635  

pct_na <- sum(is.na(df1)) / (nrow(df1) * ncol(df1)) * 100
pct_na  # percentage of missing values 20.03 % 

pct_na_col <- colSums(is.na(df)) / nrow(df) * 100
pct_na_col <- sort(pct_na_col[pct_na_col > 0], decreasing=TRUE)  

pct_na_col_50 <- pct_na_col[pct_na_col > 50] # columns with 50 % missing values 
print(pct_na_col_50)

df2 <- df2 %>% select(-which(colMeans(is.na(df2)) > 0.50)) # remove columns with 50 % missing values 

# Step 3 : check and remove remove zero variance 
df3 <- df2  
dim(df3)  # columns were reduced from 117 to 83 

zerovar_col <- nearZeroVar(df3, names = TRUE) # columns with zero variance 
print(zerovar_col) # columns with zero variance  

df3 <- df3 %>% select(-all_of(zerovar))  
dim(df3) # columns were reduced from 83 to 52  
str(df3)

# Step 4 : convert numerical and categorical variables 
df4 <- df3 

df4_var_numerical <- c("SPORDER", "PWGTP" , "MARHYP", "WAGP" , "WKHP" , "PERNP", "PINCP" , "POVPIP") 
df4[df4_var_numerical] <- lapply(df4[df4_var_numerical], as.numeric)

#Steveen: I would like a little bit more of an explanation of what is happening here.
df4[setdiff(names(df4), df4_var_numerical)] <- lapply(df4[setdiff(names(df4), df4_var_numerical)], as.factor)
summary(df4) 



# Step 5 : check and remove outliers 
df5 <- df4 
dim(df5)

df5_var_numerical <- names(df5)[sapply(df5, is.numeric)]

df5_plot <- df5 %>%
  pivot_longer(cols = all_of(df5_var_numerical), names_to = "variables", values_to = "values")

ggplot(df5_plot, aes(x = variables, y = values, fill = variables)) +
  geom_boxplot(outlier.size = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.5), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = "none") 

### func 1 : remove outliers  
func_outlier <- function(f1) {   
  
  q1 <- quantile(f1, 0.25, na.rm = TRUE)   
  q3 <- quantile(f1, 0.75, na.rm = TRUE)   
  iqr <- q3 - q1   
  lower_bound <- q1 - 1.5 * iqr   
  upper_bound <- q3 + 1.5 * iqr   
  
  return(ifelse(is.na(f1), FALSE, f1 < lower_bound | f1 > upper_bound))  
}

#Steveen: This looks interesting, we are using the function whose column is equal to 0?
# Please, can you elaborate more on the logic here? gracias!
df5 <- df5[rowSums(sapply(df5_var_numerical, function(col) func_outlier(df5[[col]]))) == 0, ]

colSums(is.na(df5))
sum(is.na(df5))
dim(df5) # rows were reduced from 4318 to 3077 
summary(df5)



# Step 6 : Imputation for NA values
df6 <- df5 
sum(is.na(df6))
colSums(is.na(df6)) 

func_median <- function(f1) {replace(f1, is.na(f1), median(f1, na.rm = TRUE))}


#The function is replacing the max with the mode?
func_mode <- function(f2) {
  mode <- names(which.max(table(na.omit(f2))))  
  replace(f2, is.na(f2), mode)
}

#this got a little bit tricky, could you elaborate what is happening below?
df6_var_numerical <- names(df6)[sapply(df6, is.numeric)]
df6_var_categorical <- setdiff(names(df6), df6_var_numerical)
df6[df6_var_categorical] <- lapply(df6[df6_var_categorical], as.factor)

df6[df6_var_numerical] <- lapply(df6[df6_var_numerical], func_median)
df6[df6_var_categorical] <- lapply(df6[df6_var_categorical], func_mode)

sum(is.na(df6))  # NANs were reduced to 0 after imputation  
colSums(is.na(df6))   

######################## EXPLORATORY DATA ANALYSIS : 
df7 <- df6

summary(df7)
dim(df7)
str(df7)

df7 <- df7[, !names(df7) %in% "SERIALNO"] ## DROP UNIQUE IDENTIFIER 
dim(df7)

################ NUMERICAL DATA : 
df7_names <- names(df7)[sapply(df7, is.numeric)]  
print(df7_names)

df7_plot <- lapply(df7_names, function(var) { 
  ggplot(df7, aes(x = .data[[var]])) + 
    geom_histogram(bins = 30, fill = "blue", alpha = 0.6) + 
    ggtitle(paste("Histogram of", var)) + 
    theme_minimal()
})

grid.arrange(grobs = df7_plot, ncol = 2)

cor_matrix <- cor(df6[df6_var_numerical])
print(cor_matrix)

ggcorrplot(cor_matrix, lab = TRUE, colors = c("blue", "white", "red"))

dim(df7)

# Export df7 as a CSV file
write.csv(df7, "df7_cleaned.csv", row.names = TRUE)


