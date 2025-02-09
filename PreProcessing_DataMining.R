# R program to read a csv file

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

df_zv <- nearZeroVar(df, saveMetrics = TRUE)
df_zv

#now let's sort the variables that show true for Zerovar and nzv
#meaning that they have no variance or little variance.

df_zv_sorted <- df_zv[order(-df_zv$zeroVar, -df_zv$nzv),]
df_zv_sorted

# Variables to analyze if the should be deleted, kept and inputed
# > df_zv_sorted

# freqRatio percentUnique zeroVar   nzv
# RACNH       0.000000    0.02315887    TRUE  TRUE

table(df$RACNH)
#decision: Delete, there is zero variance in this variable

# GCL        38.647059    0.04631774   FALSE  TRUE

table(df$GCL)
# Output:
# 
# 1    2 
# 85 3285
# Decision: Keep, it seems like the values are skekewed, but 
# the data is consistent

# HINS5      88.958333    0.04631774   FALSE  TRUE


table(df$HINS5)
# 
# Output: 
#   1    2 
# 48  4270 




# HINS6      62.500000    0.04631774   FALSE  TRUE
  
table(df$HINS6)  
# 
# output:
#   1    2 
# # 68  4250
# decision: Keep, we can see how skeewed things are
  
# HINS7     862.600000    0.04631774   FALSE  TRUE

table(df$HINS7)

# 1    2 
# 5 4313
# 
# We can honestly delete this one, it does not say anything 
# with these margings
#decision: delete



# INTP      118.548387    4.30754979   FALSE  TRUE

table(df$INTP)
#we could use it create ranges, but that is about it
# we should review the name and see what it stands for
#decision: keep

# MARHD     110.040000    0.04631774   FALSE  TRUE
table(df$MARHD)
#decision: delete



# MARHM      38.657143    0.04631774   FALSE  TRUE

table(df$MARHM)
# decision: Keep for now

# MARHW      94.724138    0.04631774   FALSE  TRUE

table(df$MARHM)
#decision: Keep for now

# MIL        23.017341    0.09263548   FALSE  TRUE

table(df$MIL)

# #what does mil stand for? 
# decision: Keep it for now

# NWAV       29.189781    0.09263548   FALSE  TRUE
table(df$NWAV)
#Decision: Keep it for now

# NWRE       22.583333    0.06947661   FALSE  TRUE

table(df$NWRE)
#decision: delete, this does not tell me much


# OIP       593.428571    2.17693377   FALSE  TRUE
table(df$OIP)
# 
# decision: delete



# PAP      1062.750000    1.08846688   FALSE  TRUE
table(df$PAP)
# decision: delete



# RETP      226.437500    6.16025938   FALSE  TRUE
table(df$RETP)
#decision: keep, we can use it to create new ranges
#let's look up what it means

# SEMP      311.230769    2.73274664   FALSE  TRUE
table(df$SEMP)
# decision: Delete




# SSIP      418.000000    1.73691524   FALSE  TRUE
table(df$SSIP)
#desision: delete


# SSP        97.823529    6.69291339   FALSE  TRUE
table(df$SSP)
# decision: keep for now

# WKWN       32.117647    1.20426123   FALSE  TRUE
table(df$WKWN)
#decision: keep for now

# HICOV      46.977778    0.04631774   FALSE  TRUE
table(df$HICOV)
# decision: delete




# HISP       30.106870    0.46317740   FALSE  TRUE
table(df$HISP)
#decision: delete



# OC         28.954887    0.04631774   FALSE  TRUE
table(df$OC)
#decision: Keep for now

# POWSP      80.516129    0.27790644   FALSE  TRUE
table(df$POWSP)
# decision: keep for now

# RACAIAN    69.786885    0.04631774   FALSE  TRUE
table(df$RACAIAN)
# decision: delete



# RACPI    1438.333333    0.04631774   FALSE  TRUE
table(df$RACPI)
#decision: delete

# RC         26.287671    0.04631774   FALSE  TRUE
table(df$RC)
#decision: Keep for now


#Deleting the variables that were selected for deletion:
# Remove specific columns
df <- subset(df, select = -c(RACNH,HINS5,HINS7,MARHD,NWRE,OIP,PAP, SEMP, SSIP, HICOV, HISP,RACAIAN,RACPI))

# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)


#let's export this dataframe to excel to see patterns and variables

write.csv(df,"filtered_data.csv", row.names = FALSE, col.names = TRUE)

#Sum of the missing values in each column
missing_values <- sort(colSums(is.na(df)), decreasing=FALSE)

print(missing_values)

# NWAB     NWAV     NWLA     NWLK 
# 58       58       58       58

table(df$NWAB)

# Temporary absence from work (UNEDITED - See 'Employment Status Recode' (ESR))
# b .N/A (less than 16 years old/at work/on layoff)
# 1 .Yes
# 2 .No
# 3 .Did not report

# 1    2    3 
# 41 1428 2791 

# We can just edit it to did not report since we have that as an option

table(df$NWAV)
# NWAV
# 58  

# Available for work (UNEDITED - See 'Employment Status Recode' (ESR))
# b .N/A (less than 16 years/at work/not looking)
# 1 .Yes
# 2 .No, temporarily ill
# 3 .No, other reasons
# 4 .No, unspecified
# 5 .Did not report

# > table(df$NWAV)
# 
# 1    2    3    5 
# 137   21  103 3999


table(df$NWLA)
# NWLA     
# 58 missing

# Meaning:
# On layoff from work (UNEDITED - See 'Employment Status Recode' (ESR))
# b .N/A (less than 16 years old/at work)
# 1 .Yes
# 2 .No
# 3 .Did not report

# > table(df$NWLA)
# 
# 1    2    3 
# 25 1519 2716 

table(df$NWLK)
# NWLK 
# 58 missing:

# Meaning:
# Looking for work (UNEDITED - See 'Employment Status Recode' (ESR))
# b .N/A (less than 16 years old/at work/temporarily .absent/informed of recall)
# 1 .Yes
# 2 .No
# 3 .Did not report
# 
# > table(df$NWLK)
# 
# 1    2    3 
# 125 1391 2744 


table(df$ESR)
# ESR 
# 58 missing

# Meaning:
# Employment status recode
# b .N/A (less than 16 years old)
# 1 .Civilian employed, at work
# 2 .Civilian employed, with a job but not at work
# 3 .Unemployed
# 4 .Armed forces, at work
# 5 .Armed forces, with a job but not at work
# 6 .Not in labor force

# > table(df$ESR)
# 
# 1       2    3    4    6 
# 2583   42   85    6 1544 


table(df$PERNP)
# PERNP
# 58 missing

#Meaning: 
  
# PERNP
# Total person's earnings (use ADJINC to adjust to constant dollars)
# bbbbbbb .N/A (less than 16 years old)
# 0 .No earnings
# -10000 .Loss of $10000 or more (Rounded and bottom- .coded components)
# -9999 to -1 .Loss $1 to $9999 (Rounded components)
# 1 to 1999998 .$1 to $1999998
  
# Decision: DELETE THIS, it doesnt tell us much


table(df$MIL)
# MIL
# 108

# Meaning:
# Military service
# b .N/A (less than 17 years old)
# 1 .Now on active duty
# 2 .On active duty in the past, but not now
# 3 .Only on active duty for training
# 4 .Never served in the military
  
# > table(df$MIL)
# 
# 1    2    3    4 
# 5  173   50 3982


# POVPIP 
# 259 



# OC       RC      WRK 
# 334      334      615 
# GCL      COW     INDP 
# 948     1077     1077 
# OCCP     WKHP     WKWN 
# 1077     1437     1437 
# MARHM    MARHT    MARHW 
# 1542     1542     1542 
# MARHYP   JWTRNS  POWPUMA 
# 1542     1729     1729 
# POWSP 
# 1729 




#method 4: Finding the variables that are highly correlated with each other



