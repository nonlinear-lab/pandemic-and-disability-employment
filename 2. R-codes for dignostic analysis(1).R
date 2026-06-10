library(car)

# Optional: Set your Working Directory (WD) where the graphs will be saved
setwd("C:/R/disability") 

panel_data <- read.csv("data(1).csv")
panel_data <- panel_data[, c("REGION", "ST", "AGEP", "SCHL", "SEX", "DIS", 
                             "ESR", "RAC1P", "PWGTP", "DEAR",
                             "DEYE", "DREM", "DPHY", "YEAR")]

# 1. Multicollinearity Tests (VIF and Tolerance)
vif_values <- vif(baseline_model)
tolerance_values <- 1 / vif_values

print("--- TABLE 5: MULTICOLLINEARITY DIAGNOSTICS ---")
print(data.frame(VIF = vif_values, Tolerance = tolerance_values))

# Filter dataset to pre-pandemic years (2017-2019) for stability checks
df_pre <- df %>% filter(Time == 0)

# 2. PTS1 Assumption: Linear Year Interaction Trend
pts1_model <- lm(EMP ~ Disability * YEAR + Gender + Race + Education + Age, data = df_pre)
pts1_results <- coeftest(pts1_model, vcov = vcovCL(pts1_model, cluster = ~ST))
print("--- PTS1 WALD TEST COEFFICIENT (Disability:YEAR) ---")
print(pts1_results["Disability:YEAR", ])

# 3. PTS2 Assumption: Categorical Year Interactions
df_pre <- df_pre %>%
  mutate(
    Year2018 = ifelse(YEAR == 2018, 1, 0),
    Year2019 = ifelse(YEAR == 2019, 1, 0)
  )
pts2_model <- lm(EMP ~ Disability * Year2018 + Disability * Year2019 + Gender + Race + Education + Age, data = df_pre)

# Joint Wald Test for both interaction coefficients equaling 0
pts2_joint_test <- waldtest(pts2_model, vcov = vcovCL(pts2_model, cluster = ~ST), terms = c("Disability:Year2018", "Disability:Year2019"))
print("--- PTS2 JOINT WALD TEST RESULTS ---")
print(pts2_joint_test)