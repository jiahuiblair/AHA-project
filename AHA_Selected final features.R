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

# Data cleaning on special values in the dataset: Using 0 to represent No instead of 2
merged_AHA$CHC[merged_AHA$CHC == 2] <- 0
merged_AHA$MAPP1[merged_AHA$MAPP1 == 2] <- 0
merged_AHA$MAPP18[merged_AHA$MAPP18 == 2] <- 0
merged_AHA$MAPP20[merged_AHA$MAPP20 == 2] <- 0

########################################## Missing Value###############################
# View structure
# glimpse(merged_AHA)
skim(merged_AHA[Keep_cate])
skim(merged_AHA[Keep_numeric])

###################################### Desciptive ####################################################
# Summarize the variable by type
# Summarize the categorical variables in freq and relative frequency
freq_tables <- map(bianry_cate_list, function(var) {
  merged_AHA %>%
    count(Level = .data[[var]]) %>%
    mutate(
      Rel_Freq = n / sum(n),
      Variable = var
    ) %>%
    ungroup()
})

# Summarize the numerical variables by year in difference inferences
numeric_summary <- map_dfr(var_categorical, function(cat_var) {
  
  this_cat <- cat_var
  
  map_dfr(var_numeric, function(num_var) {
    merged_AHA %>%
      group_by(Year, Category = .data[[this_cat]]) %>%
      summarise(
        Mean   = mean(.data[[num_var]], na.rm = TRUE),
        SD     = sd(.data[[num_var]], na.rm = TRUE),
        Median = median(.data[[num_var]], na.rm = TRUE),
        Min = min(.data[[num_var]], na.rm = TRUE),
        Max = max(.data[[num_var]], na.rm = TRUE),
        Count  = n(),
        .groups = "drop"
      ) %>%
      mutate(
        Cat_Var = this_cat,
        Num_Var = num_var
      )
  })
})

# Response variable plot
# Basic violin plot
ggplot(merged_AHA, aes(x = "", y = EXPTOT)) +
  geom_violin(fill = "skyblue", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +
  labs(title = "Violin Plot of EXPTOT",
       y = "EXPTOT",
       x = "") +
  theme_minimal()

# Basic violin plot log
ggplot(merged_AHA, aes(x = "", y = log(EXPTOT))) +
  geom_violin(fill = "skyblue", color = "black", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +
  labs(title = "Violin Plot of EXPTOT",
       y = "EXPTOT",
       x = "") +
  theme_minimal()

hist(merged_AHA$EXPTOT)
# Missing plot
#aggr(merged_AHA[Missing_cate], numbers=TRUE, prop=FALSE, sortVars=TRUE)  # nicer plot
# Categorical 
aggr_plot <- aggr(merged_AHA[Missing_cate], 
                  numbers = TRUE, 
                  prop = FALSE, 
                  sortVars = TRUE,
                  col = c("skyblue", "red"),
                  cex.axis = 0.55,
                  gap = 2,
                  ylab = c("Missing data pattern", "Count"))

# Add legend manually (optional)
legend(x=0.30, y =1, 
       legend = c("Present", "Missing"), 
       col = c("skyblue", "red"), 
       pch = 15, 
       bty = "n", 
       cex = 0.8)

# continuous 
aggr_plot <- aggr(merged_AHA[Missing_con], 
                  numbers = TRUE, 
                  prop = FALSE, 
                  sortVars = TRUE,
                  col = c("skyblue", "red"),
                  cex.axis = 0.55,
                  gap = 2,
                  ylab = c("Missing data pattern", "Count"))

# Combined plot for all missing variables 
library(tidyverse)
library(naniar)

# Step 1: Compute missingness percentage
miss_pct <- colMeans(is.na(merged_AHA[c(Missing_cate, Missing_con)]))

# Step 2: Create ordered variable names (most missing first)
ordered_vars <- names(sort(miss_pct, decreasing = TRUE))

# Step 3: Pivot data to long format for manual ggplot
long_missing <- merged_AHA %>%
  select(all_of(ordered_vars)) %>%
  mutate(row = row_number()) %>%
  pivot_longer(
    -row,
    names_to = "variable",
    values_to = "value",
    values_transform = list(value = as.character)  # 👈 THIS FIXES THE TYPE ERROR
  ) %>%
  mutate(
    missing = is.na(value),
    variable = factor(variable, levels = ordered_vars)  # ensure correct axis order
  )


# Step 4: Plot manually
ggplot(long_missing, aes(x = variable, y = row, fill = missing)) +
  geom_tile() +
  scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "red"), 
                    labels = c("Not Missing", "Missing"),
                    name = "") +
  scale_x_discrete(
    labels = function(x) paste0(x, "\n(", round(miss_pct[x] * 100, digits = 1), "%)")
  ) +
  scale_y_continuous(breaks = seq(0, max(long_missing$row), by = 500))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, size = 8, face = "bold", hjust = 0.5),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(x = "Variable", y = "")



################################# Continuous Correlation Matrix ##########################
# 1. Compute the full correlation matrix
cor_matrix <- cor(merged_AHA[Keep_numeric], use = "pairwise.complete.obs")

# 2. Create a masked version for the numbers (NA if abs(r) < 0.7)
cor_numbers <- cor_matrix
cor_numbers[abs(cor_numbers) < 0.7] <- NA

# 3. Plot background using 'color' method — no numbers
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.8,
         tl.srt = 45,
         cl.pos = "r",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE,
         addCoef.col = NA,       # Do not show any numbers yeth
         number.cex = 0.8)

############################ Categorical correlation matrix ###########################
# Correlation .
n <- length(bianry_cate_list)
cramer_mat <- matrix(NA, nrow = n, ncol = n)
rownames(cramer_mat) <- colnames(cramer_mat) <- bianry_cate_list

for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    temp_df <- merged_AHA[, c(bianry_cate_list[i], bianry_cate_list[j])] %>% na.omit()
    
    if (nrow(temp_df) > 5 &&
        length(unique(temp_df[[1]])) > 1 &&
        length(unique(temp_df[[2]])) > 1) {
      
      tbl <- table(temp_df[[1]], temp_df[[2]])
      if (min(dim(tbl)) >= 2) {
        stats <- tryCatch(assocstats(tbl), error = function(e) NULL)
        if (!is.null(stats)) {
          cramer_val <- stats$cramer
          cramer_mat[i, j] <- cramer_val
          cramer_mat[j, i] <- cramer_val
        }
      }
    }
  }
}
diag(cramer_mat) <- 1  # set diagonal to 1

# Plot
# Melt to long format
cramer_df <- melt(cramer_mat, varnames = c("Var1", "Var2"), value.name = "CramersV")
cramer_df_sort <- cramer_df %>%
  mutate(CramersV = round(CramersV, 2)) %>%     # Round to 2 decimal places
  arrange(desc(CramersV)) 

# Convert to factor with consistent order
cramer_df$Var1 <- factor(cramer_df$Var1, levels = bianry_cate_list)
cramer_df$Var2 <- factor(cramer_df$Var2, levels = bianry_cate_list)

# Keep only upper triangle values (i < j)
cramer_df_upper <- cramer_df %>%
  filter(as.numeric(Var1) <= as.numeric(Var2))

ggplot(cramer_df_upper, aes(Var1, Var2, fill = CramersV)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(CramersV, 2)), color = "black", size = 2.5) +
  scale_fill_gradient2(low = "white", high = "red", mid = "orange",
                       midpoint = 0.5, limit = c(0, 1), name = "Cramér’s V") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Upper Triangle of Cramér’s V Heatmap",
       x = NULL, y = NULL)


################################ Correlation between cont. and cate. #####
correlation_ratio <- function(categories, measurements) {
  fcat <- as.factor(categories)
  means <- tapply(measurements, fcat, mean, na.rm = TRUE)
  count <- tapply(measurements, fcat, length)
  overall_mean <- mean(measurements, na.rm = TRUE)
  between_var <- sum(count * (means - overall_mean)^2)
  total_var <- sum((measurements - overall_mean)^2, na.rm = TRUE)
  return(sqrt(between_var / total_var))
}

# Create empty matrix
eta_matrix <- matrix(NA, nrow = length(bianry_cate_list), ncol = length(Keep_numeric),
                     dimnames = list(bianry_cate_list, Keep_numeric))

# Fill matrix with eta values
for (cat in bianry_cate_list) {
  for (num in Keep_numeric) {
    eta <- correlation_ratio(merged_AHA[[cat]], merged_AHA[[num]])
    eta_matrix[cat, num] <- eta
  }
}

# Convert to long format
eta_df <- melt(eta_matrix, varnames = c("Categorical", "Numeric"), value.name = "Eta")

# Plot
ggplot(eta_df, aes(x = Numeric, y = Categorical, fill = Eta)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", high = "darkred", midpoint = 0.3, na.value = "grey90") +
  geom_text(data = subset(eta_df),
            aes(label = round(Eta, 2)), size = 3, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Ratio (Eta) Between Binary Categorical and Numeric Variables")


############################# Imputation #########################################
# Convert the variable types
merged_AHA_clean <- merged_AHA %>% 
  mutate(across(all_of(bianry_cate_list), as.factor)) %>%
  mutate(across(all_of(Keep_numeric), ~ suppressWarnings(as.numeric(.))))

Keep_numeric <- Keep_numeric[Keep_numeric != "EXPTOT"]

predM <- quickpred(merged_AHA_clean[c(bianry_cate_list, Keep_numeric)], mincor = 0.1)
# View or edit predictor matrix
# predM <- make.predictorMatrix(merged_AHA_clean[all_variable])

# Run multiple imputation
imp <- mice(merged_AHA_clean[c(bianry_cate_list, Keep_numeric)], method = 'rf', predictorMatrix = predM, m = 5, maxit = 15, seed = 500)

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
complete_data_1 <- cbind(common_df,complete(imp, 1),EXPTOT_set) # This is a list
complete_data_2 <- cbind(common_df,complete(imp, 2),EXPTOT_set)
complete_data_3 <- cbind(common_df,complete(imp, 3),EXPTOT_set)
complete_data_4 <- cbind(common_df,complete(imp, 4),EXPTOT_set)
complete_data_5 <- cbind(common_df,complete(imp, 5),EXPTOT_set)

## dataset 
imputed_list <- lapply(1:5, function(i) complete(imp, i))

# Create list of combined datasets
complete_data_list <- lapply(1:5, function(i) {
  cbind(common_df, complete(imp, i), EXPTOT_set)
})


#### Run the Variance Inflation Factor (VIF) to identify features causing Collinearity problem 
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

# Check VIF (Variance Inflation Factor (VIF)) If VIF > 5 (or 10), consider removing or combining variables.
model_vif <- lapply(lm_models, vif)
model_vif[[1]]
model_vif[[2]]
model_vif[[3]]
model_vif[[4]]
model_vif[[5]]

#### Run the regression to predict the hosptial cost 
# Re-Define your features after removing features identified with vif
features2 <- c('VEM', 'FTMDTF', 'ADC', 'GFEET', 'CEAMT', 'VIDVZ', 'PRPM',
              'CHC', 'MAPP1', 'MAPP18', 'MAPP20', 'IINSPT', 'CMRPAY', 'FAMADV',
              'COUTRHOS', 'FITCHOS', 'HLTHSHOS', 'HLTRHOS', 'EMDEPHOS',
              'NUTRPHOS', 'ONCOLHOS', 'PALHOS', 'SOCEHR', 'OUTMTX', 'WFAIPPD', 'COLLCLI',
              'TRAUML90', 'SCNED', 'CLUSTER','Year')

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

# Extract coefficients and covariance matrices using lapply
betas_list <- lapply(lm_models2, FUN = coef)
cov_matrices_list <- lapply(lm_models2, FUN = vcov)

# Combine the results (Pool with Rubin's Rule)
pooled_results <- miceadds::pool_mi(qhat = betas_list, u = cov_matrices_list)

summary(pooled_results)

# Loop through the models and generate 4-in-1 diagnostic plots
# Loop over each imputed regression model
for (i in 1:5) {
  
  model <- lm_models2[[i]]
  df <- imputed_list_2[[i]]
  
  # Extract fitted values and residuals
  fitted_vals <- fitted(model)
  residuals_vals <- resid(model)
  
  # Open PNG device
  png(filename = paste0("C:/Users/Jiahui/Documents/Loop/AHA/Residual_Plots_LM_", i, ".png"),
      width = 1000, height = 900)
  
  # Set layout and margins
  par(mfrow = c(2, 2), mar = c(4, 4, 4, 2), oma = c(0, 0, 3, 0))
  
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

