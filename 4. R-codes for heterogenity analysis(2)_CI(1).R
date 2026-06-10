library(dplyr)
library(lmtest)
library(sandwich)

# Optional: Set your Working Directory (WD)
setwd("C:/R/disability") 

# 1. Load the Data
panel_data <- read.csv("data(1).csv")
panel_data <- panel_data[, c("REGION", "ST", "AGEP", "SCHL", "SEX", "DIS", 
                             "ESR", "RAC1P", "PWGTP", "DEAR",
                             "DEYE", "DREM", "DPHY", "YEAR")]

# 2. Prepare the dataset (Base variables + Sub-type variables)
df <- panel_data %>%
  filter(AGEP >= 16 & AGEP <= 64) %>%
  mutate(
    # Base Variables
    EMP        = ifelse(ESR %in% c(1, 2, 4, 5), 1, 0),
    Disability = ifelse(DIS == 1, 1, 0),
    Time       = ifelse(YEAR >= 2021, 1, 0),
    Gender     = ifelse(SEX == 2, 1, 0),
    Race       = ifelse(RAC1P == 1, 0, 1),
    Education  = ifelse(SCHL >= 16, 0, 1),
    Age        = ifelse(AGEP >= 16 & AGEP <= 24, 1, 0),
    
    # Specific disability indicators
    Physical_Disability     = ifelse(DPHY == 1, 1, 0),
    Intellectual_Disability = ifelse(!is.na(DREM) & DREM == 1, 1, 0)
  )

# 3. Define the Helper Function for Clustered Standard Errors and Confidence Intervals
print_with_ci <- function(model, title) {
  robust_vcov <- vcovCL(model, cluster = ~ST)
  results <- cbind(coeftest(model, vcov = robust_vcov), coefci(model, vcov = robust_vcov))
  print(title)
  print(results)
}

# ==============================================================================
# --- Replicate Tables 10 to 13: Sub-group Splits ---
# ==============================================================================

# Table 10: Gender Splits (Male vs Female)
model_male <- lm(EMP ~ Disability * Time + Race + Education + Age, data = filter(df, Gender == 0))
print_with_ci(model_male, "--- TABLE 10: MALE SUB-GROUP WITH CI ---")

model_female <- lm(EMP ~ Disability * Time + Race + Education + Age, data = filter(df, Gender == 1))
print_with_ci(model_female, "--- TABLE 10: FEMALE SUB-GROUP WITH CI ---")

# Table 11: Race Splits (White vs Non-White)
model_white <- lm(EMP ~ Disability * Time + Gender + Education + Age, data = filter(df, Race == 0))
print_with_ci(model_white, "--- TABLE 11: WHITE SUB-GROUP WITH CI ---")

model_nonwhite <- lm(EMP ~ Disability * Time + Gender + Education + Age, data = filter(df, Race == 1))
print_with_ci(model_nonwhite, "--- TABLE 11: NON-WHITE SUB-GROUP WITH CI ---")

# Table 12: Education Splits (HS Graduates vs HS Leavers)
model_grad <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, Education == 0))
print_with_ci(model_grad, "--- TABLE 12: HS GRADUATES SUB-GROUP WITH CI ---")

model_leaver <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, Education == 1))
print_with_ci(model_leaver, "--- TABLE 12: HS LEAVERS SUB-GROUP WITH CI ---")

# Table 13: Age Splits (Non-Youth vs Youth)
model_nonyouth <- lm(EMP ~ Disability * Time + Gender + Race + Education, data = filter(df, Age == 0))
print_with_ci(model_nonyouth, "--- TABLE 13: NON-YOUTH SUB-GROUP WITH CI ---")

model_youth <- lm(EMP ~ Disability * Time + Gender + Race + Education, data = filter(df, Age == 1))
print_with_ci(model_youth, "--- TABLE 13: YOUTH SUB-GROUP WITH CI ---")


# ==============================================================================
# --- NEW ANALYSIS: PHYSICAL VS. INTELLECTUAL DISABILITY HETEROGENEITY ---
# ==============================================================================

# A. Interaction term analysis focusing exclusively on Physical Disability (Ambulatory)
model_physical <- lm(EMP ~ Physical_Disability * Time + Gender + Race + Education + Age, data = df)
print_with_ci(model_physical, "--- EXTENSION: PHYSICAL DISABILITY RESULTS WITH CI ---")

# B. Interaction term analysis focusing exclusively on Intellectual Disability (Cognitive)
model_intellectual <- lm(EMP ~ Intellectual_Disability * Time + Gender + Race + Education + Age, data = df)
print_with_ci(model_intellectual, "--- EXTENSION: INTELLECTUAL DISABILITY RESULTS WITH CI ---")