############################################### APPROACH #1

#libraries imported

library(tidyverse)
library(ggplot2)
library(dplyr)
library(GGally)



# ###################################################################
# Start of the analysis
# Get content into a data frame
df <- read.csv("project_data.csv", header=TRUE, sep = ",")

#Method 1: Remove irrelevant attributes 
#(e.g., tuple id, sample date)
# Getting number of original columns
columns <- ncol(df)
cat("Number of original columns:", columns, "\n")

# Remove specific columns
df <- subset(df, select = -c(RT, 
                             SERIALNO,
                             DIVISION, 
                             REGION, 
                             STATE, 
                             ADJINC))


# Get number of columns in the new dataframe
columns_2 <- ncol(df)
cat("Number of columns in the new df:", "\n")
dim(df)
#Method 2: -	Remove columns that have more than 50% of ther values missing

missing_counts <- colSums(is.na(df))
sort(missing_counts, decreasing=TRUE)

#creating threshold of 50% 
threshold <- nrow(df)/2
threshold

#removing columns that do not meet the threshold
df <- df[,missing_counts <= threshold]
columns_3 <- ncol(df)
cat("Number of columns in the new df:", columns_3, "\n")
dim(df)

sorted <- sort(colSums(is.na(df)), decreasing=TRUE)
# sorted

# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)

#method 3: Removing values that have near zero variance
# in the data set apart from the ones that we have already removed

# df_zv <- nearZeroVar(df, saveMetrics = TRUE)
# df_zv

#now let's sort the variables that show true for Zerovar and nzv
#meaning that they have no variance or little variance.

df_zv_sorted <- df_zv[order(-df_zv$zeroVar, -df_zv$nzv),]
df_zv_sorted

#Deleting the variables that were selected for deletion:
# Remove specific columns
df <- subset(df, select = -c(RACNH,HINS5,HINS7,MARHD,NWRE,OIP,PAP, SEMP, SSIP, HICOV, HISP,RACAIAN,RACPI))

# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)

#Sum of the missing values in each column
missing_values <- sort(colSums(is.na(df)), decreasing=FALSE)

print(missing_values)

# The Rest of the variables that need deletion:
#because of our previous analysis in part1

df <- subset(df, select = -c(SPORDER, COW, GCL, WRK, OC, PERNP, POVPIP, RC, INDP, OCCP,WKHP, WKWN, MARHM, MARHT, MARHW, MARHYP, JWTRNS, POWPUMA, POWSP))
# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)

#Sum of the missing values in each column
sort(colSums(is.na(df)), decreasing=TRUE)


#method 4: Handle missing values in ramaining Columns
sort(colnames(df))
sort(colSums(is.na(df)))

# NWAB     NWAV     NWLA     NWLK      ESR      MIL 
# 58       58       58       58       58      108

#checking mil
table(df$MIL)
# checking the mode of values
names(sort(-table(df$MIL)))[1]
summary(df$MIL)
#Mil is a variable that is not time sensitive
#Using the mode is the safest assumption for imputation
df$MIL <- ifelse(is.na(df$MIL), 4, df$MIL)
# sort(colSums(is.na(df)))

#ESR
table(df$ESR)
#finding the mode of the variable
names(sort(-table(df$ESR)))[1]
summary(df$ESR)

#let's look at this by using the MIL results
#if ESR is empty and MIL is equal to 4, then let's fill it with 1 Civilian employee
df$ESR[is.na(df$ESR) & df$MIL %in% c(4)] <-1
#if ESR is empty and MIL is equal to 1,2,3, then let's fill it with 3 #unemployed
df$ESR[is.na(df$ESR) & df$MIL %in% c(1,2,3)] <- 3
#if we still have remaining empty rows, let's fill it with the mode
df$ESR[is.na(df$ESR)] <- 6
summary(df$ESR)

#NWLK
table(df$NWLK)
summary(df$NWLK)
#finding the mode of the variable
names(sort(-table(df$NWLK)))[1]

table(df$ESR[is.na(df$NWLK)])  # Checking employment status (ESR) for missing NWLK
table(df$MIL[is.na(df$NWLK)])  # Checking military status (MIL) for missing NWLK

#We can see that esr = 1, there are 58 which means they are currently employed 
# MIL = 4 which means they never served the military, there are 58 missing values as well
#both already have a job, so they decided not to report it
# we can safely say that they were not looking for a job = 2
df$NWLK[is.na(df$NWLK)] <- 2 # not looking for a job
summary(df$NWLK)
# no missing values

sort(colSums(is.na(df)))

# looking at NWLA
summary(df$NWLA)
#finding the mode of the variable
names(sort(-table(df$NWLK)))[1]

table(df$NWLA, df$NWLK, useNA = "always")

#summary:
# By analyzing the data, we discovered that all 58 missing values 
# belong to people who are NOT looking for work (NWLK = 2). 
# This means these individuals never reported how many weeks 
# they were looking for work because they weren’t job searching in the first place.


df$NWLA[is.na(df$NWLA)] <- 2
summary(df$NWLA)

#let's work with NWAV
sort(colSums(is.na(df)))

#let's work the connection between esr and nwav

table(df$ESR[is.na(df$NWAV)])

# ESR 
# Meaning:
# Employment status recode
# b .N/A (less than 16 years old)
# 1 .Civilian employed, at work

# we can see that the missing values come from a civilian
# who is employed and does not need to look for work
# let's impute it with 5 which means did not report


df$NWAV[is.na(df$NWAV)] <- 5
summary(df$NWAV)

#nwab
summary(df$NWAB)

table(df$ESR[is.na(df$NWAB)])
table(df$NWLK[is.na(df$NWAB)])
table(df$NWLA[is.na(df$NWAB)])

# Temporary absence from work (UNEDITED - See 'Employment Status Recode' (ESR))
# b .N/A (less than 16 years old/at work/on layoff)
# 1 .Yes
# 2 .No
# 3 .Did not report

# it makes sense to fill it with 2 which is a no 

df$NWAB[is.na(df$NWAB)] <- 2

#let's work with NWAV
sort(colSums(is.na(df)))
dim(df)
#perfect, we now have a strong and clean dataset

# method 5: Checking for duplicate columns:
sum(duplicated(df))
# we have zero duplicated variables




# method 6: checking data types to see numeric and cathegorical 
#so it's easier to remove outliers in numerical variables

str(df)
summary(df)
outliers_present <- data.frame(df)

#numeric chosen and reviwed by the dictionary 
# numeric_vars <- c(PWGTP, INTP, RETP, SSP, WAGP, PINCP)

remove_outliers_by_iqr <- function(df, columns){
  for (col in columns){
    q1 <- quantile(df[[col]], 0.20, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.80, na.rm = TRUE)
    
    #checking the IQR
    iqr <- q3-q1
    
    #setting upper and lower bounds
    upper_bound = q3 +(1.5*iqr)
    lower_bound = q1 -(1.5*iqr)
    
    #removing rows that do not meet the threshold
    df <- df[df[[col]] >= lower_bound & df[[col]]<= upper_bound,]
  }
  
  return(df)
}
numeric_vars <- c("PWGTP", "WAGP", "PINCP")

#removed SSP and RETP because I don't wan it to remove the actual numbers
#that were reported when they reviece money 
plot(df$RETP)
plot(df$SSP)
plot(df$WAGP)
plot(df$PINCP)

#sending the variables to the function
df <- remove_outliers_by_iqr(df, numeric_vars)
# df commented this to not have so much content in the console


plot(df$PWGTP)
# plot(df$INTP)
plot(df$RETP)
plot(df$SSP)
plot(df$WAGP)
plot(df$PINCP)
summary(df)
dim(df)
#variables to look at
# SPORDER 
# INTP

#method 7: make class a factor so we can use in our ML algorithms to 
#test precision
df$Class <- ifelse(df$Class == "Yes", 1,0)
df$Class <- factor(df$Class)
is.factor(df$Class)
levels(df$Class)

dim(df)
colnames(df)

write.csv(df, "df_cleaned.csv", row.names = TRUE)



###########################################################################
# Visualizations:



vars_with_outliers <- outliers_present %>%
  select(where(is.numeric)) %>%  # Keep only numeric columns
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(vars_with_outliers, aes(x=Variable, y=Value))+
  geom_boxplot(fill="steelblue", alpha=0.7, outlier.color = 'red')+
  coord_flip() + #to see and read the values better
  theme_minimal()+
  labs(title="Summary of Numerical Variables with Outliers", x="Variable", y="Value")



all_vars_summary <- df %>%
  select(where(is.numeric)) %>%  # Keep only numeric columns
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(all_vars_summary, aes(x=Variable, y=Value))+
  geom_boxplot(fill="steelblue", alpha=0.7, outlier.color = 'red')+
  coord_flip() + #to see and read the values better
  theme_minimal()+
  labs(title="Summary of Numerical Variables No Outliers", x="Variable", y="Value")

#Vizualizing correlation for all numeric variables
# ggpairs(df %>% select(where(is.numeric)) %>% select(1:5))  # First 5 numeric columns
# 
# ggpairs(df %>% select(where(is.numeric)))

# library(corrplot)
# # (1). Show the correlation matrix (with all four variables).
# cor_data = cor(df %>% select(where(is.numeric)))
# head(round(M,2))
# 
# print("Correlation Matrix (with all four variables):")
# print(cor_data)
# 
# # as numbers
# corrplot(cor_data, method="number")
# 
# 
# # as colour
# corrplot(cor_data, method="color")
# 




