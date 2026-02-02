# Load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(skimr)
library(mice)
library(ggplot2)
library(naniar)
library(corrplot)
library(purrr)
library(broom)
library(VIM)


# Import Excel files and add year column
AHA_22 <- read_excel("C:/Users/Jiahui/Documents/Loop/AHA/2024_12_FY22.xlsx", sheet = "FY22") %>%
  mutate(Year = 2022)

AHA_23 <- read_excel("C:/Users/Jiahui/Documents/Loop/AHA/2024_12_FY23.xlsx", sheet = "FY23") %>%
  mutate(HHEGTKFC = as.numeric(HHEGTKFC), Year = 2023)

# Define numeric variables
var_numeric <- c('PSYBD', 'HOSPBD', 'EXPTOT', 'ADMTOT', 'IPDTOT', 'MCRIPD', 'MCDIPD',
                 'SPTIP', 'THRTIP', 'VEM', 'FTMDTF', 'FTRNTF', 'FTPHR', 'ADC', 'FTAPRN', 'FTPHRN', 
                 'PLNTA', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM')

# remove the variable 
remove_cate_cleaning <- c('COMMTYC', 'PSCBD', # Delete them due to high missing volumne
                          'COMMTY', # highly correlated with CHC
                          'MEDADHOS', 'MMCHOS', 'OTHIMHOS', 'HLINHOS', 'OSMGOTH', # this is the subvar. for IINSPT
                          'SCFOD', 'SCTRN', 'SCIOS','SCOTH', 'SCBH', 'SCBH (not available in FY23)' # this is the subvar. for SCNED
) 

# Get all column names
all_columns_22 <- setdiff(names(AHA_22), remove_cate_cleaning) # Get names and remove variables
all_columns_23 <- setdiff(names(AHA_23), remove_cate_cleaning) # Get names and remove variables

# Identify categorical variables
var_string_22 <- setdiff(all_columns_22, var_numeric)
var_string_23 <- setdiff(all_columns_23, var_numeric)

# Convert column types
AHA_22_cleaned <- AHA_22[all_columns_22] %>%
  mutate(across(all_of(var_string_22), as.character)) %>%
  mutate(across(all_of(var_numeric), ~ suppressWarnings(as.numeric(.))))

AHA_23_cleaned <- AHA_23[all_columns_23] %>%
  mutate(across(all_of(var_string_23), as.character)) %>%
  mutate(across(all_of(var_numeric), ~ suppressWarnings(as.numeric(.))))


# Merge datasets
merged_AHA <- bind_rows(AHA_22_cleaned, AHA_23_cleaned)

# Define the binary list
cate_common_list <- c('ID', 'CNTYNAME','CBSANAME','CBSACODE', # Common variables 
                      'TRAUML90', 'SCNED', 'CLUSTER', 'BSC', "CBSATYPE"   # This is multiple levels
)
binary_list <- setdiff(names(merged_AHA[var_string_23]), cate_common_list)

var_categorical <- c('TRAUML90', 'SCNED', 'CLUSTER', 'BSC', "CBSATYPE")


# All varaible names 
all_variable <- c(var_categorical, var_numeric, binary_list)

# Convert the variable types
merged_AHA_clean <- merged_AHA %>% 
  mutate(across(all_of(var_categorical), as.factor)) %>%
  mutate(across(all_of(binary_list), as.factor)) %>%
  mutate(across(all_of(var_numeric), ~ suppressWarnings(as.numeric(.))))

# Default methods chosen by mice
#methods <- make.method(merged_AHA_clean[all_variable])

# Set method manually
#methods[var_continuous] <- "pmm"          # predictive mean matching (for continuous, skewed)
#methods[binary_list] <- "logreg"     # logistic regression (binary)
#methods[var_categorical] <- "polyreg"
#methods[Nonmissing_list] <- ""           # no missing

#methods
high_corr_vars <- c('BHHLT', 'KSCHCLI', 'OTLSCLI', 'LORGCLI', 'HHEGTKFC', 'WFAIART', 'WFAIOACW', 
                    'DHER', 'PSYEDHOS', 'ONCOLHOS', 'PALHOS', 'HLTHCHOS', 'HLTHSHOS') # The list to exclude 

impute_list <- setdiff(all_variable, high_corr_vars)
predM <- quickpred(merged_AHA_clean[impute_list], mincor = 0.1)
# View or edit predictor matrix
# predM <- make.predictorMatrix(merged_AHA_clean[all_variable])



# Run multiple imputation
imp <- mice(merged_AHA_clean[impute_list], method = 'rf', predictorMatrix = predM, m = 5, maxit = 15, seed = 500)
## rf is randome forest, as the similar pattern/corrlinearity in missing data, pmm, logreg and polyreg have issues with singular matrix error 
## m is the number of datasets 
## maxit specifies the maximum number of iterations (or "cycles") that the mice algorithm will perform for each imputed dataset.
## predictorMatrix defines which variables are used as predictors (independent variables) for imputing missing values in other variables (dependent variables).

# Quick look at imputations
summary(imp)

# Get datasets
## dataset of thoese comon variables 
common_df <- merged_AHA_clean[Id_list]
complete_data_1 <- cbind(common_df,complete(imp, 1))
complete_data_2 <- cbind(common_df,complete(imp, 2))
complete_data_3 <- cbind(common_df,complete(imp, 3))
complete_data_4 <- cbind(common_df,complete(imp, 4))
complete_data_5 <- cbind(common_df,complete(imp, 5))

# Output the data to csv
# folder_path <- "C:/Users/Jiahui/Documents/Loop/AHA/ImputedData"
# write.csv(complete_data_1, file = file.path(folder_path, "complete_data_1.csv"), row.names = FALSE)
# write.csv(complete_data_2, file = file.path(folder_path, "complete_data_2.csv"), row.names = FALSE)
# write.csv(complete_data_3, file = file.path(folder_path, "complete_data_3.csv"), row.names = FALSE)
# write.csv(complete_data_4, file = file.path(folder_path, "complete_data_4.csv"), row.names = FALSE)
# write.csv(complete_data_5, file = file.path(folder_path, "complete_data_5.csv"), row.names = FALSE)

# Test on the imputated data 
plot(imp) # visualize the trace lines for the means and standard deviations of the imputed values across iterations for each variable. You want to see these lines mixing well and converging to a stable range, indicating that the imputation algorithm has stabilized
dev.off()
densityplot(imp) # compare the distribution of the observed data with the imputed data for each variable. For continuous variables, the imputed data's density should broadly follow the observed data's density.
dev.off()
stripplot(imp) #imputed values for each variable across different imputations, allowing you to spot outliers or unusual patterns.
