# Load necessary libraries
library(dplyr)
library(ggplot2)

# Sample dataset
insurance_data <- data.frame(
  Policy_ID = 1001:1010,
  Age = c(45, 30, 55, 42, 29, 60, 35, 50, 40, 33),
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  BMI = c(27.5, 22.1, 31.4, 25.8, 24.3, 28.9, 26.7, 29.2, 23.5, 21.8),
  Smoker = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
  Chronic_Conditions = c(2, 0, 2, 1, 1, 2, 0, 2, 1, 0),  # Count of chronic conditions
  Hospital_Visits = c(3, 1, 5, 2, 1, 6, 0, 4, 2, 1),
  Prescriptions = c(5, 1, 7, 3, 2, 8, 1, 6, 3, 1),
  Annual_Claims = c(12500, 2100, 18700, 6300, 3800, 22500, 1500, 15800, 5200, 2700),
  Risk_Score = c(0.85, 0.20, 0.92, 0.55, 0.30, 0.95, 0.15, 0.78, 0.40, 0.22)
)

# Define base premium
base_premium <- 1000

# Calculate insurance premium using risk factors
insurance_data <- insurance_data %>%
  mutate(
    Age_Factor = 1 + (Age - 30) * 0.02,  # Increase by 2% per year over 30
    BMI_Factor = 1 + (BMI - 25) * 0.03,  # Increase by 3% per BMI unit above 25
    Smoker_Factor = ifelse(Smoker, 1.5, 1),  # 50% increase for smokers
    Condition_Factor = 1 + Chronic_Conditions * 0.1,  # 10% increase per condition
    Risk_Factor = 1 + Risk_Score,  # Scaling by risk score
    Final_Premium = base_premium * Age_Factor * BMI_Factor * Smoker_Factor * Condition_Factor * Risk_Factor
  )

# Print final premium table
print(dplyr::select(insurance_data, Policy_ID, Age, BMI, Smoker, Chronic_Conditions, Risk_Score, Final_Premium))

# Visualization: Risk Score vs. Premium
ggplot(insurance_data, aes(x = Risk_Score, y = Final_Premium)) +
  geom_point(aes(color = as.factor(Smoker), size = Chronic_Conditions)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Risk Score vs. Insurance Premium",
       x = "Risk Score",
       y = "Premium ($)",
       color = "Smoker Status",
       size = "Chronic Conditions") +
  theme_minimal()
