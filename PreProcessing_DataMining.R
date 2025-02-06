# R program to read a csv file

# Get content into a data frame
df <- read.csv("project_data.csv", header=TRUE, sep = ",")

# Printing content of Text File
#head(df, 5)

# find location of missing values
print("Position of missing values ")
#which(is.na(df))

# count total missing values 
print("Count of total missing values  ")
sum(is.na(df))

#Sum of the missing values in each column
sort(colSums(is.na(df)), decreasing=TRUE)

#Method 1: Remove irrelevant attributes (e.g., tuple id, sample date)
# Getting number of original columns
columns <- ncol(df)
cat("Number of original columns:", columns, "\n")

# Remove specific columns
df_1 <- subset(df, select = -c(RT, SERIALNO))

# Get number of columns in the new dataframe
columns_2 <- ncol(df_1)
cat("Number of columns in the new df:", columns_2, "\n")

##columns that need to be dropped: 
# 
# GCM       SFN       SFR       GCR       ESP       NOP      MLPA 
# 4292      4244      4244      4233      4177      4177      4140 
# MLPB     MLPCD      MLPE     MLPFG      MLPH     MLPIK      MLPJ 
# 4140      4140      4140      4140      4140      4140      4140 
# VPS     FOD2P     CITWP   MIGPUMA     MIGSP      SCHG      YOEP 
# 4140      4058      3846      3803      3803      3691      3439 
# DECADE       ENG      LANP       FER     JWRIP   DRIVESP     FOD1P 
# 3439      3381      3381      3179      2549      2549      2349 
# SCIENGP SCIENGRLP      PAOC     JWMNP      JWAP      JWDP    JWTRNS 
# 2349      2349      2273      2188      2188      2188

