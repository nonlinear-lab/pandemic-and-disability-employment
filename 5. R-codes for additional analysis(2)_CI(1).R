library(dplyr)
library(lmtest)
library(sandwich)

# Optional: Set your Working Directory (WD)
setwd("C:/R/disability") 

# 1. Load the Raw Microdata
panel_data <- read.csv("data(1).csv")
panel_data <- panel_data[, c("REGION", "ST", "AGEP", "SCHL", "SEX", "DIS", 
                             "ESR", "RAC1P", "PWGTP", "DEAR",
                             "DEYE", "DREM", "DPHY", "YEAR")]

# 2. Build the Core Dataset (Retaining the raw 'SCHL' variable for splitting)
df <- panel_data %>%
  filter(AGEP >= 16 & AGEP <= 64) %>%
  mutate(
    EMP        = ifelse(ESR %in% c(1, 2, 4, 5), 1, 0),
    Disability = ifelse(DIS == 1, 1, 0),
    Time       = ifelse(YEAR >= 2021, 1, 0),
    Gender     = ifelse(SEX == 2, 1, 0),
    Race       = ifelse(RAC1P == 1, 0, 1),
    Age        = ifelse(AGEP >= 16 & AGEP <= 24, 1, 0)
  )

# 3. Helper Function for Cluster-Robust SEs and 95% CIs (Clustered by US State)
print_with_ci <- function(model, title) {
  robust_vcov <- vcovCL(model, cluster = ~ST)
  results <- cbind(coeftest(model, vcov = robust_vcov), coefci(model, vcov = robust_vcov))
  print(title)
  print(results)
}

# ==============================================================================
# --- Replicate Table 17: Additional Analysis on Educational Difference ---
# ==============================================================================

# --- A. ELEMENTARY SCHOOL THRESHOLD (SCHL = 8; Grade 5) ---
# Elementary school graduates (Grade 5 or above) vs. Elementary school leavers (Grade 4 or below)
model_elem_grad   <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, SCHL >= 8))
model_elem_leaver <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, SCHL < 8))

print_with_ci(model_elem_grad, "--- TABLE 17: ELEMENTARY SCHOOL GRADUATES (SCHL >= 8) ---")
print_with_ci(model_elem_leaver, "--- TABLE 17: ELEMENTARY SCHOOL LEAVERS (SCHL < 8) ---")


# --- B. MIDDLE SCHOOL THRESHOLD (SCHL = 11; Grade 8) ---
# Middle school graduates (Grade 8 or above) vs. Middle school leavers (Grade 7 or below)
model_mid_grad   <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, SCHL >= 11))
model_mid_leaver <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, SCHL < 11))

print_with_ci(model_mid_grad, "--- TABLE 17: MIDDLE SCHOOL GRADUATES (SCHL >= 11) ---")
print_with_ci(model_mid_leaver, "--- TABLE 17: MIDDLE SCHOOL LEAVERS (SCHL < 11) ---")


# --- C. COLLEGE GRADUATE THRESHOLD (SCHL = 21; Bachelor's Degree) ---
# College graduates (Bachelor's or above) vs. Non-college graduates (Associate's or below)
model_college_grad <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, SCHL >= 21))
model_college_non  <- lm(EMP ~ Disability * Time + Gender + Race + Age, data = filter(df, SCHL < 21))

print_with_ci(model_college_grad, "--- TABLE 17: COLLEGE GRADUATES (SCHL >= 21) ---")
print_with_ci(model_college_non, "--- TABLE 17: NON-COLLEGE GRADUATES (SCHL < 21) ---")