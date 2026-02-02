# AHA-project
AHospital Expenditure Analysis & Missing Data Imputation (AHA Survey)
🎯 Objective 
This project addresses the operational and financial strains facing U.S. hospitals. The study aims to: Identify key predictors of total hospital facility expenditures (EXPTOT). Evaluate Multiple Imputation methods to handle incomplete data within the American Hospital Association (AHA) Annual Survey Database, ensuring robust modeling despite missing values.
🧪 Materials & Methods 
Dataset: De-identified 2022–2023 AHA Annual Survey data (n=12,359).Features: 34 financial, structural, and operational variables.Imputation Framework: Used Multivariate Imputation by Chained Equations (MICE).Algorithm Selection: Compared regression-based vs. machine learning algorithms. Random Forest (RF) was selected as the optimal imputation method via fivefold cross-validation. Modeling: Linear regression was fitted across five RF-imputed datasets to determine expenditure drivers.
📈 Key Findings 
The analysis identified several critical determinants of hospital spending (p < 0.001): Top Predictors: Full-time registered nurses (FTRNTF), facility square footage (GFEET), and property/equipment costs (PLNTA).Cost Drivers: Higher expenditures were linked to community designations, oncology/research services, and Joint Commission accreditation.Cost Reducers: Rural status and community trauma center designations were associated with lower overall costs.Geographic Insights: Visualization revealed significant disparities in resource distribution, particularly highlighting the strain on rural healthcare access.
💡Discussion & Impact 
This project demonstrates that Machine Learning-based multiple imputation significantly improves data completeness and accuracy in healthcare operations research. The findings provide a data-driven foundation for policymakers to address geographic inequities and optimize resource allocation.

Workflow (AHA_Selected final features.R)
1. Cleaning up the dataset
2. Defining the complete feature and incomplete feature lists
3. Understand the missingness in the feature (missing values analysis and plot the missing value pattern)
4. Find correlation between different types of features (continuous and categorical)
5. Descriptive analysis
6. Imputed the data with MICE and RF
7. Fit the regression model to predict the hospital facility expenditures
8. Diagnose the regression model and model evaluation
9. Pool the results from all imputed dataset regession model
