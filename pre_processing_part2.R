# Get content into a data frame
df <- read.csv("project_data.csv", header=TRUE, sep = ",")

# Printing content of Text File
#head(df, 5)


#Sum of the missing values in each column
# sort(colSums(is.na(df)), decreasing=TRUE)


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
cat("Number of columns in the new df:", columns_2, "\n")

#Method 2: -	Remove columns that have more than 50% of ther values missing

missing_counts <- colSums(is.na(df))
missing_counts

#creating threshold of 50% 
threshold <- nrow(df)/2
threshold

#removing columns that do not meet the threshold
df <- df[,missing_counts <= threshold]
columns_3 <- ncol(df)
cat("Number of columns in the new df:", columns_3, "\n")

sorted <- sort(colSums(is.na(df)), decreasing=TRUE)
sorted

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

df <- subset(df, select = -c(COW, GCL, WRK, OC, PERNP, POVPIP, RC, INDP, OCCP,WKHP, WKWN, MARHM, MARHT, MARHW, MARHYP, JWTRNS, POWPUMA, POWSP))
# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)

#Sum of the missing values in each column
sort(colSums(is.na(df)), decreasing=TRUE)


#method 4: Handle missing values in ramaining Columns
sort(colnames(df))
sort(colSums(is.na(df)))

#checking mil
table(df$MIL)
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
#perfect, we now have a strong and clean dataset

# method 5: Checking for duplicate columns:
sum(duplicated(df))
# we have zero duplicated variables
# 
# method 6: checking data types to see numeric and cathegorical 
# and see how we can scale them or normalize them

str(df)
summary(df)

df$Class <- ifelse(df$Class=="Yes", 1,0)
table(df$Class)
str(df)
summary(df)


#numeric chosen and reviwed by the dictionary 
# numeric_vars <- c(PWGTP, INTP, RETP, SSP, WAGP, PINCP)

df$PWGTP <- scale(df$PWGTP)
df$INTP <- scale(df$INTP)
df$RETP <- scale(df$RETP)
df$SSP <- scale(df$SSP)
df$WAGP <- scale(df$WAGP)
df$PINCP <- scale(df$PINCP)

summary(df)

write.csv(df,"scaled_data.csv", row.names = FALSE, col.names = TRUE)
