library(dplyr)
library(lmtest)
library(sandwich)

# Optional: Set your Working Directory (WD)
setwd("C:/R/disability") 

panel_data <- read.csv("data(1).csv")
panel_data <- panel_data[, c("REGION", "ST", "AGEP", "SCHL", "SEX", "DIS", 
                            "ESR", "RAC1P", "PWGTP", "DEAR",
                            "DEYE", "DREM", "DPHY", "YEAR")]

df <- panel_data %>%
  filter(AGEP >= 16 & AGEP <= 64) %>%
  mutate(
    EMP = ifelse(ESR %in% c(1, 2, 4, 5), 1, 0),
    Disability = ifelse(DIS == 1, 1, 0),
    Time       = ifelse(YEAR >= 2021, 1, 0),
    Gender    = ifelse(SEX == 2, 1, 0),                       
    Race      = ifelse(RAC1P == 1, 0, 1),                     
    Education = ifelse(SCHL >= 16, 0, 1),                     
    Age       = ifelse(AGEP >= 16 & AGEP <= 24, 1, 0)         
  )

# --- Replicate Table 4: Baseline Model ---
baseline_model <- lm(EMP ~ Disability * Time + Gender + Race + Education + Age, data = df)

# Calculate robust standard errors and confidence intervals
robust_vcov <- vcovCL(baseline_model, cluster = ~ST)
baseline_coef <- coeftest(baseline_model, vcov = robust_vcov)
baseline_ci <- coefci(baseline_model, vcov = robust_vcov)

# Bind them together for a clean output table
baseline_results <- cbind(baseline_coef, baseline_ci)
print("--- TABLE 4: BASELINE REGRESSION RESULTS (CLUSTERED BY STATE WITH 95% CI) ---")
print(baseline_results)