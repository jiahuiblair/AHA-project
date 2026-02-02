# Load packages
library(readxl)
library(dplyr)
library(tidyverse)
library(skimr)
library(mice)
library(miceadds)
library(ggplot2)
library(naniar)
library(corrplot)
library(purrr)
library(broom)
library(VIM)
library(vcd)
library(reshape2)
library(caret)
library(car)

# Import Excel files and add year column
AHA_22 <- read_excel("C:/Users/Jiahui/Documents/Loop/AHA/2024_12_FY22.xlsx", sheet = "FY22") %>%
  mutate(Year = 2022)

AHA_23 <- read_excel("C:/Users/Jiahui/Documents/Loop/AHA/2024_12_FY23.xlsx", sheet = "FY23") %>%
  mutate(HHEGTKFC = as.numeric(HHEGTKFC), Year = 2023)

# Define numeric variables
Keep_numeric <- c('HOSPBD', 'EXPTOT',  'VEM', 'FTMDTF', 'FTRNTF', 'ADC', 
                  'PLNTA', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM') 
Remove_numeric <- c('PSYBD', 'ADMTOT', 'IPDTOT', 'MCRIPD', 'MCDIPD', 'SPTIP', 'THRTIP','FTPHR', 'FTAPRN', 'FTPHRN') 
var_numeric <- c(Keep_numeric, Remove_numeric)

# remove the variable 
remove_cate_cleaning <- c('COMMTYC', 'PSCBD', # Delete them due to high missing volumne
                          'COMMTY', # highly correlated with CHC
                          'MEDADHOS', 'MMCHOS', 'OTHIMHOS', 'HLINHOS', 'OSMGOTH', # this is the subvar. for IINSPT
                          'SCFOD', 'SCTRN', 'SCIOS','SCOTH', 'SCBH', 'SCBH (not available in FY23)' # this is the subvar. for SCNED
) 

Keep_cate <- c('ID', 'CNTYNAME','CBSANAME','CBSACODE',
               'CHC', 'MAPP1', 'MAPP18', 'MAPP20', 'IINSPT', 'CMRPAY', 'FAMADV',
               'COUTRHOS', 'FITCHOS', 'HLTHSHOS', 'HLTRHOS', 'EMDEPHOS',
               'NUTRPHOS', 'ONCOLHOS', 'PALHOS', 'SOCEHR', 'OUTMTX', 'WFAIPPD', 'COLLCLI',
               'TRAUML90', 'SCNED', 'CLUSTER',
               'Year')
Missing_cate <- c('IINSPT', 'CMRPAY', 'FAMADV',
                  'COUTRHOS', 'EMDEPHOS', 'FITCHOS', 'HLTHSHOS', 'HLTRHOS',
                  'NUTRPHOS', 'ONCOLHOS', 'PALHOS', 'SOCEHR', 'OUTMTX', 'WFAIPPD', 'COLLCLI',
                 'TRAUML90', 'SCNED', 'CLUSTER')
Missing_con <- c('PLNTA', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM')

complete_con <- c('HOSPBD', 'EXPTOT',  'VEM', 'FTMDTF', 'FTRNTF', 'ADC')
complete_cat <- c('CHC', 'MAPP1', 'MAPP18', 'MAPP20')


# Define the binary list
cate_common_list <- c('ID', 'CNTYNAME','CBSANAME','CBSACODE') # Common variables 
cate_list <- c('TRAUML90', 'SCNED', 'CLUSTER')
binary_list <- setdiff(Keep_cate, c(cate_common_list,cate_list))
bianry_cate_list <- c(cate_list, binary_list)

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

merged_AHA$TRAUML90 <- ifelse(merged_AHA$TRAUML90 == 5, 4, merged_AHA$TRAUML90)

merged_AHA$CHC[merged_AHA$CHC == 2] <- 0
merged_AHA$MAPP1[merged_AHA$MAPP1 == 2] <- 0
merged_AHA$MAPP18[merged_AHA$MAPP18 == 2] <- 0
merged_AHA$MAPP20[merged_AHA$MAPP20 == 2] <- 0

###########################################################3
merged_AHA$Missing <- ifelse(is.na(merged_AHA$COUTRHOS), 1, 0)

######
# Reshape to long format for faceted plotting
df_long <- merged_AHA %>%
  select(Missing, all_of(complete_con)) %>%
  pivot_longer(
    cols = all_of(complete_con),
    names_to = "Variable",
    values_to = "Value"
  )

# Create boxplots for each variable, grouped by Missing
ggplot(df_long, aes(x = factor(Missing), y = Value, fill = factor(Missing))) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    x = "Missing (0 = Not Missing, 1 = Missing)",
    y = "Value",
    title = "Distribution of Variables by Missing Status"
  ) +
  theme_bw() +
  theme(legend.position = "none")
#####

# Reshape to long format
df_long_cat <- merged_AHA %>%
  select(Missing, all_of(complete_cat)) %>%
  pivot_longer(
    cols = all_of(complete_cat),
    names_to = "Variable",
    values_to = "Value"
  )

# Bar plot: distribution of binary variable values grouped by Missing
ggplot(df_long_cat, aes(x = factor(Value), fill = factor(Missing))) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    x = "Value of Binary Variable",
    y = "Count",
    fill = "Missing",
    title = "Distribution of Binary Variables by Missing Status"
  ) +
  theme_bw()

########################################## Missing Value###############################
# View structure
# glimpse(merged_AHA)
skim(merged_AHA[Keep_cate])
skim(merged_AHA[Keep_numeric])

######################################## Missing value pattern with EXPTOT ##########################
all_missing_vars <- c(Missing_cate, Missing_con)

# 1. Create missingness indicator variables
for (var in all_missing_vars) {
  merged_AHA[[paste0(var, "_miss")]] <- ifelse(is.na(merged_AHA[[var]]), 1, 0)
}

# 2. Run ANOVA tests and store results
results <- data.frame(
  variable = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in all_missing_vars) {
  miss_var <- paste0(var, "_miss")
  
  # ANOVA: EXPTOT ~ missing indicator
  fit <- aov(EXPTOT ~ as.factor(merged_AHA[[miss_var]]), data = merged_AHA)
  pval <- summary(fit)[[1]][["Pr(>F)"]][1]
  
  results <- rbind(results, data.frame(variable = var, p_value = pval))
}

# 3. Check significant associations (e.g., p < 0.05)
sig_vars <- results %>% filter(p_value < 0.05)
print(sig_vars)

# 4. Plot EXPTOT distribution for significant variables
for (var in sig_vars$variable) {
  miss_var <- paste0(var, "_miss")
  
  # Compute group means and SE
  summary_df <- merged_AHA %>%
    group_by(!!sym(miss_var)) %>%
    summarise(
      mean_exptot = mean(EXPTOT, na.rm = TRUE),
      se_exptot = sd(EXPTOT, na.rm = TRUE) / sqrt(n())
    )
  
  p <- ggplot(summary_df, aes(x = as.factor(!!sym(miss_var)), y = mean_exptot, fill = as.factor(!!sym(miss_var)))) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(ymin = mean_exptot - se_exptot, ymax = mean_exptot + se_exptot),
                  width = 0.2, color = "black") +
    labs(
      title = paste("Mean EXPTOT by Missingness in", var),
      x = "Missing Indicator (0 = observed, 1 = missing)",
      y = "Mean EXPTOT"
    ) +
    scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
    theme_minimal()
  
  print(p)
}

############################# Imputation #########################################
# Convert the variable types
merged_AHA_clean <- merged_AHA %>% 
  mutate(across(all_of(bianry_cate_list), as.factor)) %>%
  mutate(across(all_of(Keep_numeric), ~ suppressWarnings(as.numeric(.))))

Keep_numeric <- Keep_numeric[Keep_numeric != "EXPTOT"]

#predM <- quickpred(merged_AHA_clean[c(bianry_cate_list, Keep_numeric)], mincor = 0.1)

# Run multiple imputation
imp <- mice(merged_AHA_clean[c(bianry_cate_list, Keep_numeric)], method = 'rf', m = 5, maxit = 25, seed = 500) #predictorMatrix = predM,

## rf is randome forest, as the similar pattern/corrlinearity in missing data, pmm, logreg and polyreg have issues with singular matrix error 
## m is the number of datasets 
## maxit specifies the maximum number of iterations (or "cycles") that the mice algorithm will perform for each imputed dataset.
## predictorMatrix defines which variables are used as predictors (independent variables) for imputing missing values in other variables (dependent variables).

# Quick look at imputations
summary(imp)


# Get datasets
EXPTOT_set <- merged_AHA[c("EXPTOT")]

## dataset of thoese comon variables 
common_df <- merged_AHA_clean[cate_common_list]
complete_data_1 <- cbind(common_df,complete(imp, 1),EXPTOT_set) 
complete_data_2 <- cbind(common_df,complete(imp, 2),EXPTOT_set)
complete_data_3 <- cbind(common_df,complete(imp, 3),EXPTOT_set)
complete_data_4 <- cbind(common_df,complete(imp, 4),EXPTOT_set)
complete_data_5 <- cbind(common_df,complete(imp, 5),EXPTOT_set)

## dataset 
imputed_list <- lapply(1:5, function(i) complete(imp, i))

# Create list of combined datasets
#complete_data_list <- lapply(1:5, function(i) {
#  cbind(common_df, complete(imp, i), EXPTOT_set)
#})

Keep_numeric_ml <- c('HOSPBD', 'VEM', 'FTMDTF', 'FTRNTF', 'ADC', 
                  'PLNTA', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM') 

complete_data_list <- lapply(1:5, function(i) {
  df <- bind_cols(common_df, complete(imp, i), EXPTOT_set)  # stays as data.frame
  
  # Apply log1p to numeric features
  df[Keep_numeric_ml] <- lapply(df[Keep_numeric_ml], log1p)
  
  df
})

complete_data_list <- lapply(1:5, function(i) {
  df <- bind_cols(common_df, complete(imp, i), EXPTOT_set)  # stays as data.frame
  
  # Apply log1p to numeric features
  df[Keep_numeric_ml] <- lapply(df[Keep_numeric_ml], log1p)
  
  df
})

# Define your features
features <- c('HOSPBD', 'VEM', 'FTMDTF', 'FTRNTF', 'ADC', 'PLNTA', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM',
              'CHC', 'MAPP1', 'MAPP18', 'MAPP20', 'IINSPT', 'CMRPAY', 'FAMADV',
              'COUTRHOS', 'FITCHOS', 'HLTHSHOS', 'HLTRHOS', 'EMDEPHOS',
              'NUTRPHOS', 'ONCOLHOS', 'PALHOS', 'SOCEHR', 'OUTMTX', 'WFAIPPD', 'COLLCLI',
              'TRAUML90', 'SCNED', 'CLUSTER','Year')

# Create the formula
formula <- as.formula(paste("log(EXPTOT) ~", paste(features, collapse = " + ")))

# Fit linear models on each dataset in the list
lm_models <- lapply(complete_data_list, function(df) {
  lm(formula, data = df)
})

# Check VIF 
model_vif <- lapply(lm_models, vif)
model_vif[[1]]
model_vif[[2]]
model_vif[[3]]
model_vif[[4]]
model_vif[[5]]

###############
# Define your features
features2 <- c('ADC','FTMDTF','CEAMT', 'VIDVZ', 'PRPM','FTRNTF', 'GFEET', 'PLNTA', 
               'CHC', 'MAPP1', 'MAPP18', 'MAPP20', 'IINSPT', 'CMRPAY', 'FAMADV',
               'COUTRHOS', 'FITCHOS', 'HLTHSHOS', 'HLTRHOS', 
               'NUTRPHOS', 'ONCOLHOS', 'PALHOS', 'SOCEHR', 'OUTMTX', 'WFAIPPD', 'COLLCLI',
               'TRAUML90', 'SCNED', 'CLUSTER','Year')
#  'FTRNTF', 'GFEET', 'PLNTA', 
# Create the formula
formula2 <- as.formula(paste("log(EXPTOT) ~", paste(features2, collapse = " + ")))

# Fit linear models on each dataset in the list
lm_models2 <- lapply(complete_data_list, function(df) {
  lm(formula2, data = df)
})

# Get summaries for all models
model_summaries <- lapply(lm_models2, summary)

# To view the summary of each model:
model_summaries[[1]]
model_summaries[[2]]
model_summaries[[3]]
model_summaries[[4]]
model_summaries[[5]]

# Extract coefficients (Estimate, Std. Error, t value, Pr(>|t|))
coefficients_list <- lapply(model_summaries, function(s) s$coefficients)
coefficients_list[[1]] # Coefficients for the first model

# Extract confidence intervals
conf_intervals_list <- lapply(lm_models, confint)
conf_intervals_list[[1]] # Confidence intervals for the first model

####################### Rubin rule code####################

# Fit regression models
lm_models2 <- lapply(complete_data_list, function(df) {
  lm(log(EXPTOT) ~ ADC + FTMDTF + CEAMT + VIDVZ + PRPM +
       FTRNTF + GFEET + PLNTA + CHC + MAPP1 + MAPP18 +
       MAPP20 + IINSPT + CMRPAY + FAMADV + COUTRHOS +
       FITCHOS + HLTHSHOS + HLTRHOS + NUTRPHOS + ONCOLHOS +
       PALHOS + SOCEHR + OUTMTX + WFAIPPD + COLLCLI +
       TRAUML90 + SCNED + CLUSTER + Year,
     data = df)
})

# 2. Extract coefficients and covariance matrices using lapply
betas_list <- lapply(lm_models2, FUN = coef)
cov_matrices_list <- lapply(lm_models2, FUN = vcov)

# 3. Combine the results
pooled_results <- miceadds::pool_mi(qhat = betas_list, u = cov_matrices_list)

summary(pooled_results)
###################################################################

# Loop through the models and generate 4-in-1 diagnostic plots
# Loop over each imputed regression model
for (i in 1:5) {
  
  model <- lm_models2[[i]]
  df <- complete_data_list[[i]]
  
  # Extract fitted values and residuals
  fitted_vals <- fitted(model)
  residuals_vals <- resid(model)
  
  # Open PNG device
  png(filename = paste0("C:/Users/Jiahui/Documents/Loop/AHA/Residual_Plots_LM2_", i, ".png"),
      width = 1000, height = 900)
  
  # Set layout and margins
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2), oma = c(0, 0, 3, 0), cex = 1.4)
  
  # Plot 1: Residuals vs Fitted
  plot(fitted_vals, residuals_vals,
       main = "Residuals vs Fitted",
       xlab = "Fitted values", ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)
  
  # Plot 2: Normal Q-Q
  qqnorm(residuals_vals, main = "Normal Q-Q")
  qqline(residuals_vals, col = "red")
  
  # Plot 3: Scale-Location
  sqrt_abs_res <- sqrt(abs(residuals_vals))
  plot(fitted_vals, sqrt_abs_res,
       main = "Scale-Location",
       xlab = "Fitted values", ylab = expression(sqrt("|Residuals|")))
  abline(h = 0, col = "red", lty = 2)
  
  # Plot 4: Histogram of Residuals
  hist(residuals_vals,
       main = "Residuals Histogram",
       xlab = "Residuals", breaks = 30, col = "gray")
  
  # Add overall title
  mtext(paste("Residual Diagnostics - Linear Model - Dataset", i),
        outer = TRUE, cex = 1.5, font = 2)
  
  # Close PNG device
  dev.off()
}


####Calculate the metrics 
# Initialize lists to store results
mse_list <- c()
mae_list <- c()
r2_list <- c()

# Loop through each model
for (i in 1:5) {
  model <- lm_models2[[i]]
  df <- complete_data_list[[i]]
  
  print(df)
  # True values and predictions
  actual <- log(df$EXPTOT)
  predicted <- predict(model, newdata = df)
  
  # Calculate metrics
  mse_val <- mean((actual - predicted)^2)
  mae_val <- mean(abs(actual - predicted))
  r2_val <- summary(model)$r.squared
  
  # Store metrics
  mse_list[i] <- mse_val
  mae_list[i] <- mae_val
  r2_list[i] <- r2_val
}

# Create a summary data frame
metrics_df <- data.frame(
  Dataset = 1:5,
  MSE = round(mse_list, 4),
  MAE = round(mae_list, 4),
  R2  = round(r2_list, 4)
)

# Print the result
print(metrics_df)

# Output the data to csv
folder_path <- "C:/Users/Jiahui/Documents/Loop/AHA/ImputedData"
write.csv(complete_data_1, file = file.path(folder_path, "complete_data_NoEXP_1.csv"), row.names = FALSE)
write.csv(complete_data_2, file = file.path(folder_path, "complete_data_NoEXP_2.csv"), row.names = FALSE)
write.csv(complete_data_3, file = file.path(folder_path, "complete_data_NoEXP_3.csv"), row.names = FALSE)
write.csv(complete_data_4, file = file.path(folder_path, "complete_data_NoEXP_4.csv"), row.names = FALSE)
write.csv(complete_data_5, file = file.path(folder_path, "complete_data_NoEXP_5.csv"), row.names = FALSE)

# Test on the imputated data 
plot(imp) # visualize the trace lines for the means and standard deviations of the imputed values across iterations for each variable. You want to see these lines mixing well and converging to a stable range, indicating that the imputation algorithm has stabilized
dev.off()
densityplot(imp) # compare the distribution of the observed data with the imputed data for each variable. For continuous variables, the imputed data's density should broadly follow the observed data's density.
dev.off()
stripplot(imp) #imputed values for each variable across different imputations, allowing you to spot outliers or unusual patterns.

