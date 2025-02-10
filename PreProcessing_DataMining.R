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

# df_zv <- nearZeroVar(df, saveMetrics = TRUE)
# df_zv

#now let's sort the variables that show true for Zerovar and nzv
#meaning that they have no variance or little variance.

df_zv_sorted <- df_zv[order(-df_zv$zeroVar, -df_zv$nzv),]
df_zv_sorted

# Variables to analyze if the should be deleted, kept and inputed
# > df_zv_sorted

# freqRatio percentUnique zeroVar   nzv
# RACNH       0.000000    0.02315887    TRUE  TRUE

table(df$RACNH)
#decision: DELETE
#There is zero variance in this variable

# GCL        38.647059    0.04631774   FALSE  TRUE

table(df$GCL)

# Grandparents living with grandchildren
# b .N/A (less than 30 years/institutional GQ)
# 1 .Yes
# 2. NO

# Output:
# 
# 1    2 
# 85 3285

# Decision: Keep, it seems like the values are skekewed, but 
# the data is consistent


table(df$HINS5)
# HINS5      88.958333    0.04631774   FALSE  TRUE
# 
# Output: 
#   1    2 
# 48  4270 

# TRICARE or other military health care
# 1 .Yes
# 2 .No





  
table(df$HINS6)  
# HINS6      62.500000    0.04631774   FALSE  TRUE
# 
# output:
#   1    2 
# # 68  4250
# decision: Keep, we can see how skeewed things are
# 
# VA (enrolled for VA health care)
# 1 .Yes
# 2 .No
  


table(df$HINS7)
# HINS7     862.600000    0.04631774   FALSE  TRUE

# 1    2 
# 5 4313
# 
# We can honestly delete this one, it does not say anything 
# with these margings
#decision: DELETE
# Indian Health Service
# 1 .Yes
# 2 .No



# INTP      118.548387    4.30754979   FALSE  TRUE

table(df$INTP)
#we could use it create ranges, but that is about it
# we should review the name and see what it stands for

#decision: DELETE

# Interest, dividends, and net rental income past 12 months (use ADJINC to adjust to constant dollars)
# bbbbbb     .N/A (less than 15 years old)
# 0          .None
# -10000..-4 .Loss of $4 to $10000 (Rounded and bottom-coded)
# 4..999999 .$4 to $999999 (Rounded and top-coded)


table(df$MARHD)
# MARHD     110.040000    0.04631774   FALSE  TRUE
#decision: delete


# MARHM      38.657143    0.04631774   FALSE  TRUE

table(df$MARHM)
# decision: Keep for now
# Married in the past 12 months
# b .N/A (age less than 15 years; never married)
# 1 .Yes
# 2 .No


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

df$NWAB <- ifelse(is.na(df$NWAB), 3, df$NWAB)

table(df$NWAB)


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
# decision: Keep

df$NWAV <- ifelse(is.na(df$NWAV), 5, df$NWAV)

table(df$NWAV)


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

df$NWLA <- ifelse(is.na(df$NWLA), 3, df$NWLA)
table(df$NWLA)

# Decision: Keep

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

# Decision: Keep
df$NWLK <- ifelse(is.na(df$NWLA), 3, df$NWLA)

table(df$NWLK)



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

# Decision: Keep

df$ESR <- ifelse(is.na(df$ESR), 6, df$ESR)
table(df$ESR)


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

# decision: keep

df$MIL <- ifelse(is.na(df$MIL), 4, df$MIL)
table(df$MIL)


table(df$POVPIP)
# POVPIP 
# 259 
# Meaning:
# Income-to-poverty ratio recode
# bbb      .N/A (individuals who are under 15 and are either living .in a housing unit but are unrelated to the householder .or are living in select group quarters)
# 0..500   .Below 501 percent
# 501      .501 percent or more
# DECISION: DELETE






table(df$OC)
#OC 
#334

# Meaning: 
# Own child
# b .N/A (in GQ)
# 0 .No
# 1 .Yes

# > table(df$OC)
# 
# 0    1 
# 3851  133

#DECISION: Delete this one

table(df$RC)
# RC      
# 334    
# Meaning:
# Related child
# b .N/A (in GQ)
# 0 .No
# 1 .Yes

# > table(df$RC)
# 
# 0     1 
# 3838  146 
#DECISION DELETE



table(df$WRK)
#WRK
#615

# > table(df$WRK)
# 
# 1    2 
# 2331 1372 

# Worked last week
# b .N/A (not reported)
# 1 .Worked
# 2 .Did not work

# DECISION: KEEP

df$WRK <- ifelse(is.na(df$WRK), 2, df$WRK)
table(df$WRK)


table(df$GCL)
# Ask Melanie what she thinks
# GCL
# 948

# Grandparents living with grandchildren
# b .N/A (less than 30 years/institutional GQ)
# 1 .Yes
# 2 .No

#DECISION: KEEP

df$GCL <- ifelse(is.na(df$GCL), 9, df$COW)


table(df$COW)
# # COW     
# 1077
# 
# Class of worker
# b .N/A (less than 16 years old/NILF who last worked more than 5 .years ago or never worked)
# 1 .Employee of a private for-profit company or business, or of an .individual, for wages, salary, or commissions
# 2 .Employee of a private not-for-profit, tax-exempt, or .charitable organization
# 3 .Local government employee (city, county, etc.)
# 4 .State government employee
# 5 .Federal government employee
# 6 .Self-employed in own not incorporated business, professional .practice, or farm
# 7 .Self-employed in own incorporated business, professional .practice or farm
# 8 .Working without pay in family business or farm
# 9 .Unemployed and last worked 5 years ago or earlier or never .worked

# > table(df$COW)
# 
# 1     2    3    4     5    6    7    8     9  
# 1969  501  252  126   58  209  107   11    8

#imputation process
df$COW <- ifelse(is.na(df$COW), 9, df$COW)

table(df$COW)

table(df$INDP)
# INDP
# 1077

# decision: Delete





# The Rest of the variables that need deletion:

df <- subset(df, select = -c(OC, PERNP, POVPIP, RC, INDP, OCCP,WKHP, WKWN, MARHM, MARHT, MARHW, MARHYP, JWTRNS, POWPUMA, POWSP))
# Total number of rows & columns
dimensions=dim(df)
cat("Number of rows & columns: ", dimensions)

#Sum of the missing values in each column
sort(colSums(is.na(df)), decreasing=TRUE)







#method 4: Finding the numerical variables that are highly correlated with each other



