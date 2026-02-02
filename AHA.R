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
cate_list <- c('TRAUML90', 'SCNED', 'CLUSTER', 'BSC', "CBSATYPE")


########################################## Missing Value###############################
# View structure of variables with missing values 
glimpse(merged_AHA)
skim(merged_AHA)

# remove the categorical variable with many levels or too much missing value
remove_cate = c('ID', 'CNTYNAME','CBSANAME','CBSACODE','OSMGOTH','SCOTH', 'SCBH (not available in FY23)')
var_categorical = setdiff(var_string_23, remove_cate)
print(var_categorical)

# Visual summary of missing values
Nonmissing_cat_list <- c('CHC', 'BSC', 'COMMTY', 'CBSATYPE', 'MAPP1', 'MAPP18',
                     'MAPP20', 'Year')
Nonmissing_con_list <- c('EXPTOT', 'ADMTOT', 'IPDTOT', 
                         'MCRIPD', 'MCDIPD', 'SPTIP', 'THRTIP', 'VEM', 'FTMDTF',
                         'FTRNTF', 'FTPHR', 'ADC', 'HOSPBD')
Missing_cat_list <- c('IINSPT','CMRPAY', 'FAMADV','PSYHOS', 'DHER','DHACAR',  'SCNED','SOCEHR',
                      'OUTMTX', 'WFAIPPD', 'HHEGTKFC', 'LORGCLI', 'OTLSCLI',
                      'KSCHCLI', 'COLLCLI', 'CMHLT', 'BHHLT', 'CLUSTER'
                       ) # 'MEDADHOS', 'MMCHOS','HLINHOS', 'OTHIMHOS','OSMGOTH' from IINSPT
                                        # 'SCFOD','SCBH',for SCNED
Missing_con_list <- c('PSCBD', 'FTAPRN', 'FTPHRN', 'PLNTA', 'GFEET', 'CEAMT',
                      'VIDVZ', 'PRPM')

################################### Missing data plot ####################################
merged_AHA[Missing_cat_list] <- lapply(merged_AHA[Missing_cat_list], as.character)
merged_AHA[Missing_con_list] <- lapply(merged_AHA[Missing_con_list], as.numeric)
Missing_list <- c(Missing_cat_list, Missing_con_list)

md.pattern(merged_AHA[Missing_con_list]) # create the missing value plot 
aggr(merged_AHA[Missing_cat_list], numbers=TRUE, prop=FALSE, sortVars=TRUE)  # nicer plot

################################# Continuous Correlation Matrix ##########################

# 1. Compute the full correlation matrix
cor_matrix <- cor(merged_AHA[var_numeric], use = "pairwise.complete.obs")

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
         addCoef.col = NA,       # Do not show any numbers yet
         number.cex = 0.8)

############################ Categorical correlation matrix ###########################
# Correlation .
bianry_cate_list <- c(cate_list, binary_list)
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

ggplot(cramer_df, aes(Var1, Var2, fill = CramersV)) +
  geom_tile(color = "white") +
  geom_text(data = subset(cramer_df, CramersV >= 0.5 & CramersV < 1),
            aes(label = round(CramersV, 2)), color = "black", size = 2.5) +
  scale_fill_gradient2(low = "white", high = "red", mid = "orange",
                       midpoint = 0.5, limit = c(0, 1), name = "Cramér’s V") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Cramér’s V Heatmap of Binary Variables",
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
eta_matrix <- matrix(NA, nrow = length(bianry_cate_list), ncol = length(var_numeric),
                     dimnames = list(bianry_cate_list, var_numeric))

# Fill matrix with eta values
for (cat in bianry_cate_list) {
  for (num in var_numeric) {
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
  geom_text(data = subset(eta_df, Eta >= 0.14 & Eta < 1),
            aes(label = round(Eta, 2)), size = 3, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Ratio (Eta) Between Binary Categorical and Numeric Variables")



##########################################################################################
# Summarize the variable by type
# Summarize the categorical variables by year in freq and relative frequency
freq_tables <- map(var_categorical, function(var) {
  merged_AHA %>%
    count(Year, Level = .data[[var]]) %>%
    group_by(Year) %>%
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

#folder_path <- "C:/Users/Jiahui/Documents/Loop/AHA"
#write.csv(numeric_summary,
#          file = file.path(folder_path, "AHA_Desc_Sum.csv"),
#          row.names = FALSE)

###################################################################################
# Hypothesis test
# Placeholder for results
results <- data.frame(var1 = character(),
                      var2 = character(),
                      test = character(),
                      statistic = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop through all combinations for missing values_ANOVA
for (i in seq_along(Nonmissing_con_list)) {
  for (j in seq_along(Missing_cat_list)) {
    
    x <- Nonmissing_con_list[i]
    y <- Missing_cat_list[j]
    print("This is a new test")
    print(x)
    print(y)
    
    
    df_temp <- merged_AHA %>% 
      select(all_of(c(x, y))) #%>% 
     # drop_na()
    
    df_temp[[y]] <- as.character(df_temp[[y]])
    df_temp[["Missing"]][is.na(df_temp[[y]])] <- "Missing"
    df_temp[["Missing"]][is.na(df_temp[["Missing"]])] <- "Non-Missing"
    df_temp[["Missing"]] <- as.factor(df_temp[["Missing"]])
    
    
    if (nrow(df_temp) > 10 && n_distinct(df_temp[["Missing"]]) > 1) {
      # ANOVA: Continuous vs Categorical
      anova_model <- aov(df_temp[[x]] ~ as.factor(df_temp[["Missing"]]))
      tidy_res <- tidy(anova_model)
      p <- tidy_res$p.value[1]
      stat <- tidy_res$statistic[1]
      
      if (!is.na(p) && p < 0.05) {
        results <- rbind(results, data.frame(var1 = x,
                                             var2 = y,
                                             test = "ANOVA",
                                             statistic = stat,
                                             p_value = p))
      }
    }
  }
}

# Categorical vs Categorical → Chi-squared
for (i in 1:(length(Nonmissing_cat_list))) {
  for (j in 1:length(Missing_cat_list)) {
    x <- Nonmissing_cat_list[i]
    y <- Missing_cat_list[j]
    
    df_temp <- merged_AHA %>% 
                   select(all_of(c(x, y))) 
    df_temp[[y]] <- as.character(df_temp[[y]])
    df_temp[["Missing"]][is.na(df_temp[[y]])] <- "Missing"
    df_temp[["Missing"]][is.na(df_temp[["Missing"]])] <- "Non-Missing"
    df_temp[["Missing"]] <- as.factor(df_temp[["Missing"]])
    
    if (nrow(df_temp) > 10 &&
        n_distinct(df_temp[[x]]) > 1 &&
        n_distinct(df_temp[["Missing"]]) > 1) {
      
      tbl <- table(df_temp[[x]], df_temp[["Missing"]])
      if (all(dim(tbl) >= 2)) {
        test <- suppressWarnings(chisq.test(tbl))
        p <- test$p.value
        stat <- test$statistic
        
        if (!is.na(p) && p < 0.05) {
          results <- rbind(results, data.frame(var1 = x,
                                               var2 = y,
                                               test = "Chi-squared",
                                               statistic = stat,
                                               p_value = p))
        }
      }
    }
  }
}

## Reformat the results

# Get all unique values from both columns
all_unique_values <- unique(c(results$var1, results$var2))

# Create an empty matrix with unique values as row and column names
co_occurrence_matrix <- matrix(0,
                               nrow = length(all_unique_values),
                               ncol = length(all_unique_values),
                               dimnames = list(all_unique_values, all_unique_values))

# Iterate through each row of the dataframe and update the matrix
for (i in 1:nrow(results)) { # Note: Use your dataframe name "results"
  val1 <- results$var1[i]
  val2 <- results$var2[i]
  
  # Mark the intersection of val1 and val2 as 1
  co_occurrence_matrix[val1, val2] <- 1
  # You might want to uncomment the line below if the relationship is symmetric
  # co_occurrence_matrix[val2, val1] <- 1
}


###################################################################################
## Plot 
# Make sure the grouping variables are factors

df_summary <- numeric_summary %>%
  mutate(
    Category = factor(Category),
    Year = factor(Year),
    Cat_Var = as.factor(Cat_Var),
    Num_Var = as.factor(Num_Var)
  )

# Create a plot for each Num_Var
for (num in unique(df_summary$Num_Var)) {
  for (chr in unique(df_summary$Cat_Var)){
    
    df_subset <- df_summary %>% filter(Num_Var == num & Cat_Var == chr)
    print(df_subset)
    
    p <- ggplot(df_subset, aes(x = Category, y = Mean, fill = Year)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      labs(
        title = paste("Mean of", num, "by", chr),
        x = "Category",
        y = "Mean"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    
    # Optional: Save plot to file
    ggsave(paste0("C:/Users/Jiahui/Documents/Loop/AHA/Plot/mean_plot_", num,"versus", chr, ".png"), p, width = 8, height = 5)
  }
}

### Plot 
## Histogram 
# Reshape the data to long format
plot_hist_df <- merged_AHA %>%
  select(all_of(var_numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Create histograms using ggplot2, faceted by variable
ggplot(plot_hist_df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Numeric Variables", x = "Value", y = "Count")

## Box plot
# Loop through each continuous variable

