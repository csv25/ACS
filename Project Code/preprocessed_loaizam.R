######################################## APPROACH # 2 : 
###### PACKAGES :   
library(naniar)  
library(caret)
library(dplyr)

library(tidyr)
library(ggplot2)
library(gridExtra)

df <- read.csv('project_data.csv') 
head(df)  

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
