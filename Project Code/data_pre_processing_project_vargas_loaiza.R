######################################## APPROACH # 1 : 

###### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)

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

df_zv <- nearZeroVar(df, saveMetrics = TRUE)
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
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    
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

########################
#(.20,.80) ratios in dimensions
# [1] 3818   45
#rows lost: 12%
#(.25,.75) ratios in dimensions
# 3632   45
#rows lost: 16%


#method 7: make class a factor so we can use in our ML algorithms to 
#test precision
df$Class <- ifelse(df$Class == "Yes", 1,0)
df$Class <- factor(df$Class)
is.factor(df$Class)
levels(df$Class)

dim(df)
colnames(df)

write.csv(df, "df_cleaned.csv", row.names = TRUE)

######################################## APPROACH # 2 :

df <- read.csv("project_data.csv", header=TRUE, sep = ",")
head(df_approach2)  

## Initial data exploration : 
dim(df) 
str(df)

n_miss(df) 
prop_miss(df)

summary(df) 

# Step 1 - Identifying and Reducing Low-Variance Variables:
df1 <- df  
zerovar <- nearZeroVar(df1, names = TRUE) 
print(zerovar) 

df1 <- df1 %>% select(-all_of(zerovar))  

dim(df1)  
str(df1)
summary(df1) 

# Step 2 - Checking for Duplicates:
df2 <- df1 
df2_duplicates <- df2[duplicated(df2), ]

dim(df2)
print(nrow(df2_duplicates))  

# Step 3 - Removal of Non-Informative Variables:
df3 <- df2
df3 <- df3[, !names(df3) %in% c("SERIALNO", "SPORDER")]
dim(df3)

miss_var_summary(df3) %>%
  arrange(desc(pct_miss)) %>%  
  slice_head(n = 50) %>%       
  print(n = 50)

# Step 4 - Handling Missing Values:  
# 4.1 Exploring the dataset: 
n_miss(df3)  
prop_miss(df3) 

miss_var_summary(df3) %>%
  arrange(desc(pct_miss)) %>%  
  slice_head(n = 50) %>%       
  print(n = 50)

gg_miss_var(df3) +
  theme_minimal() +  
  theme(axis.text.y = element_text(size = 5)) 

# 4.2 . Strategies for Handling Missing Values: 
## Constant Imputation for character-type (low-cardinality):   
df4 <- df3
df4$GCM[is.na(df4$GCM)] <- "0"
df4$SFR[is.na(df4$SFR)] <- "0"
df4$GCR[is.na(df4$GCR)] <- "0"
df4$ESP[is.na(df4$ESP)] <- "0"
df4$NOP[is.na(df4$NOP)] <- "0"

zerovar <- nearZeroVar(df4, names = TRUE) 
print(zerovar) 

df4 <- df4 %>% select(-c(GCM, SFR, GCR, ESP, NOP)) 

## Standardized category assignments based on similarity:
df4$MLPA[is.na(df4$MLPA)] <- "0"  
df4$MLPA[df4$MLPA == "0"] <- "1"  
df4$MLPA[df4$MLPA == "1"] <- "2"

df4$MLPB[is.na(df4$MLPB)] <- "0"
df4$MLPB[df4$MLPB == "0"] <- "1"
df4$MLPB[df4$MLPB == "1"] <- "2"

df4$MLPE[is.na(df4$MLPE)] <- "0"
df4$MLPE[df4$MLPE == "0"] <- "1"
df4$MLPE[df4$MLPE == "1"] <- "2"

df4$MLPCD[is.na(df4$MLPCD)] <- "0"
df4$MLPCD[df4$MLPCD == "0"] <- "1"
df4$MLPCD[df4$MLPCD == "1"] <- "2"

df4$MLPFG[is.na(df4$MLPFG)] <- "0"
df4$MLPFG[df4$MLPFG == "0"] <- "1"
df4$MLPFG[df4$MLPFG == "1"] <- "2"

zerovar <- nearZeroVar(df4, names = TRUE) 
print(zerovar) ## [1] "MLPA"  "MLPB"  "MLPCD" "MLPE"  "MLPFG"

df4 <- df4 %>% select(-c(MLPA, MLPB, MLPCD, MLPE, MLPFG)) 

## Reclassification of numeric variables as categorical:
df4$POVPIP <- ifelse(is.na(df4$POVPIP), 0, 
                     ifelse(df4$POVPIP <= 500, 1, 2)) 

df4$YOEP <- ifelse(is.na(df4$YOEP), 0, 
                   ifelse(df4$YOEP <= 1938, 2, 3)) 

df4$MARHYP <- ifelse(is.na(df4$MARHYP), 0,  
                     ifelse(df4$MARHYP <= 1944, 1, 2)) 

df4$CITWP <- ifelse(is.na(df4$CITWP), 0,  
                    ifelse(df4$CITWP <= 1947, 1, 2))

df4$MIGPUMA <- ifelse(is.na(df4$MIGPUMA), 0, 
                      ifelse(df4$MIGPUMA == "00001", 1, 
                             ifelse(df4$MIGPUMA == "00002", 2, 3))) 

## Assigning missing values to existing categories:
df4$INDP[is.na(df4$INDP)] <- 9920 
df4$OCCP[is.na(df4$OCCP)] <- 9920 
df4$LANP[is.na(df4$LANP)] <- 9999  

## Handling missing values in numeric variables:
df4$WKHP[is.na(df4$WKHP)] <- "0" 
df4$JWRIP[is.na(df4$JWRIP)] <- "0"  
df4$JWMNP[is.na(df4$JWMNP)] <- "0"   
df4$PERNP[is.na(df4$PERNP)] <- "0"   

## Imputation for character-type (medium-cardinality) :
df4$VPS[is.na(df4$VPS)] <- "0" 
df4$ESR[is.na(df4$ESR)] <- "0"
df4$ENG[is.na(df4$ENG)] <- "0" 
df4$FER[is.na(df4$FER)] <- "0" 
df4$COW[is.na(df4$COW)] <- "0" 
df4$WRK[is.na(df4$WRK)] <- "0"

df4$NWAB[is.na(df4$NWAB)] <- "0"  
df4$NWLA[is.na(df4$NWLA)] <- "0"  
df4$NWLK[is.na(df4$NWLK)] <- "0"
df4$SCHG[is.na(df4$SCHG)] <- "0" 
df4$PAOC[is.na(df4$PAOC)] <- "0" 
df4$JWAP[is.na(df4$JWAP)] <- "0" 
df4$JWDP[is.na(df4$JWDP)] <- "0" 

df4$MARHT[is.na(df4$MARHT)] <- "0" 
df4$FOD2P[is.na(df4$FOD2P)] <- "0" 
df4$FOD1P[is.na(df4$FOD1P)] <- "0" 

df4$JWTRNS[is.na(df4$JWTRNS)] <- "0" 
df4$DECADE[is.na(df4$DECADE)] <- "0" 

df4$POWPUMA[is.na(df4$POWPUMA)] <- "0" 
df4$DRIVESP[is.na(df4$DRIVESP)] <- "0" 
df4$SCIENGP[is.na(df4$SCIENGP)] <- "0" 

df4$SCIENGRLP[is.na(df4$SCIENGRLP)] <- "0" 


zerovar_7 <- nearZeroVar(df4, names = TRUE) 
print(zerovar_7) ## [1] "FOD1P" "FOD2P" "JWAP"  "VPS"

df4 <- df4 %>% select(-c(FOD1P, FOD2P, JWAP, VPS)) 

dim(df4)
miss_var_summary(df4) %>%
  arrange(desc(pct_miss)) %>%  
  slice_head(n = 50) %>%       
  print(n = 50)

gg_miss_var(df4) +
  theme_minimal() +  
  theme(axis.text.y = element_text(size = 5))  

str(df4)
dim(df4)

# Step 5 - Data Transformation:  
df5 <- df4

## Converting Numeric and Categorical Variables:
var_numeric <- c("PWGTP", "WAGP", "PERNP", "PINCP", "WKHP", "JWMNP", "JWRIP") 
df5 <- df5 %>%
  mutate(across(all_of(var_numeric), as.numeric)) %>%
  mutate(across(-all_of(var_numeric), as.factor)) 

## Data exploration for categorical variables (Variables with more than 5 categories excluded) : 
var_categorical_excluded <- df5 %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "variables", values_to = "count") %>%
  filter(count > 5) %>%
  pull(variables)

var_categorical <- df5 %>% select(-all_of(var_categorical_excluded))

var_categorical <- var_categorical %>%
  select(where(is.factor)) %>%
  pivot_longer(cols = everything(), names_to = "variables", values_to = "count")

ggplot(var_categorical, aes(x = count)) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(~variables, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(x = "Categories", y = "Frequency")

summary(df5)

# Step 6 - Handling Outliers:  
# 6.1 Detecting Outliers: 
df6 <- df5 

func_outlier <- function(f1) {  
  q1 <- quantile(f1, 0.25, na.rm = TRUE)   
  q3 <- quantile(f1, 0.75, na.rm = TRUE)   
  iqr <- q3 - q1   
  lower_bound <- q1 - 1.5 * iqr   
  upper_bound <- q3 + 1.5 * iqr   
  return(ifelse(is.na(f1), FALSE, f1 < lower_bound | f1 > upper_bound)) }

## Outliers proportion , count and total : 
df6_var_numerical <- names(df6)[sapply(df6, is.numeric)]

outliers_summary <- df6 %>%
  summarise(across(all_of(df6_var_numerical), ~ sum(func_outlier(.), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "count") %>%
  mutate(pct_outlier = (count / nrow(df6)) * 100) %>%
  filter(count > 0) %>%
  arrange(desc(pct_outlier))

print(outliers_summary)
outliers <- sum(outliers_summary$count)
print(outliers)

## Data exploration for numerical variables : 
df6_plot <- df6 %>% pivot_longer(cols = all_of(df6_var_numerical), names_to = "variables", values_to = "count")
ggplot(df6_plot, aes(x = variables, y = count, fill = variables)) +
  geom_boxplot(outlier.size = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6.5), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = "none") 

## Apply f1 func_outlier delete outliers:  
df6 <- df6[rowSums(sapply(df6_var_numerical, function(col) func_outlier(df6[[col]]))) == 0, ]

### IMPORTANT : AFTER RUNNING THE F1 FUNCTION - RE RUN PREVIOUS STEP TO PLOT(df6_plot) THE NEW FIGURE. 

# Step 8 - Binary Encoding for Target Variable (Class):  
df7 <- df6
df7$Class <- ifelse(df7$Class == "Yes", 1, 0)

## FINAL DATA SET : 
dim(df7) 
summary(df7)
str(df7)
colnames(df7) 




