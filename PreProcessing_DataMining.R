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

#Learning the summary of some of the important variables
#this is an editing to test the version control

#this is a new test again