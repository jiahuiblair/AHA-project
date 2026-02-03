Predicting Hospital Expenditure Gaps: A Machine Learning-Based Multiple Imputation Approach
📋 Project Objective
This study addresses the growing financial instability in the U.S. healthcare system by identifying the key drivers of hospital total facility expenditures (EXPTOT). Using data from the American Hospital Association (AHA) Annual Survey, this project:
Addresses Data Quality: Evaluates and implements advanced imputation methods to handle significant reporting gaps (only 9.56% of cases were complete).
Identifies Cost Drivers: Quantifies the impact of workforce, physical infrastructure, and specialized service lines on hospital operational costs.
Visualizes Inequities: Maps geographic disparities in hospital resources to inform health system management and resource allocation.

🛠️ Data Science Workflow & Key Skills
This project demonstrates a sophisticated technical pipeline for large-scale, incomplete healthcare datasets:
Advanced Data Cleaning & Merging (R):
Integrated longitudinal survey data (n=12,359) from 2022 and 2023.
Engineered features to normalize complex survey responses into 34 standardized financial, structural, and operational variables.
Machine Learning for Data Imputation:
Framework: Leveraged the MICE (Multivariate Imputation by Chained Equations) framework.
Model Selection: Conducted five-fold cross-validation to compare 18 imputation methods, including Bayesian regression, LASSO, and Tree-based methods.
Random Forest (RF) Imputation: Selected RF as the superior method for its ability to handle complex, non-linear relationships and achieve the lowest error rates across variable types.
Statistical Modeling & Inference:
Predictive Modeling: Fitted linear regression models across five RF-imputed datasets to ensure robust parameter estimation.
Variable Importance: Identified full-time registered nurses (FTRNTF), facility square footage (GFEET), and property/equipment costs (PLNTA) as the strongest predictors (p < 0.001).
Geospatial & Visualization:
Utilized sf and tigris to perform geographic delineations using Core Based Statistical Area (CBSA) codes.
Developed Albers-projected maps to visualize national expenditure trends and resource gaps, particularly in rural trauma centers.

📊 Key Results & Impact

Superior Data Preparation: Machine learning–based imputation (RF) significantly outperformed traditional mean or simple regression methods, preserving the multivariate data structure.


Operational Insights: Found that specialized services (Oncology, Research) and accreditation (Joint Commission) correlate with significantly higher expenditures.


Structural Disparities: Rural and community trauma centers report significantly lower costs but face higher resource scarcity, highlighting geographic inequities.

💻 Tech Stack
Language: R

Machine Learning/Imputation: mice, caret, VIM, randomForest

Spatial Analysis: sf, tigris, ggmap, cowplot

Visualization: ggplot2, naniar (missingness plots), corrplot

Data Wrangling: tidyverse (dplyr, purrr, lubridate), readxl

Workflow 
1. Cleaning up the dataset (AHA_Selected final features.R)
2. Defining the complete feature and incomplete feature lists (AHA_Selected final features.R)
3. Understand the missingness in the feature (missing values analysis and plot the missing value pattern) (AHA_Selected final features.R)
4. Find correlation between different types of features (continuous and categorical) (AHA_Selected final features.R)
5. Descriptive analysis (AHA_Selected final features.R)
6. Imputed the data with MICE and RF (AHA_Selected final features.R)
7. Fit the regression model to predict the hospital facility expenditures (AHA_Selected final features.R)
8. Diagnose the regression model and model evaluation (AHA_Selected final features.R)
9. Pool the results from all imputed dataset regession model (AHA_Selected final features.R)
10. Plot the key features that can predict the hospital facility expenditure in geographic plot (LocationPlot_impute.R)

    <img width="870" height="1255" alt="image" src="https://github.com/user-attachments/assets/292d8730-f11d-47da-bafc-722388a59560" />

