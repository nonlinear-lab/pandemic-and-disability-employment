library(survey)
library(dplyr)
library(lmtest)
library(sandwich)

# Create df_robust
df_robust <- df %>%
  mutate(
    At_Work      = ifelse(ESR %in% c(1, 4), 1, 0),
    Unemployment = ifelse(ESR == 3, 1, 0),
    Inactivity   = ifelse(ESR == 6, 1, 0)
  )

# Create a quick helper function to print tables with CIs
print_with_ci <- function(model, title) {
  robust_vcov <- vcovCL(model, cluster = ~ST)
  results <- cbind(coeftest(model, vcov = robust_vcov), coefci(model, vcov = robust_vcov))
  print(title)
  print(results)
}

# Table 6, 7, and 8
rob_at_work <- lm(At_Work ~ Disability * Time + Gender + Race + Education + Age, data = df_robust)
print_with_ci(rob_at_work, "--- TABLE 6: AT-WORK RATE WITH CI ---")

rob_unemp <- lm(Unemployment ~ Disability * Time + Gender + Race + Education + Age, data = df_robust)
print_with_ci(rob_unemp, "--- TABLE 7: UNEMPLOYMENT RATE WITH CI ---")

rob_inactive <- lm(Inactivity ~ Disability * Time + Gender + Race + Education + Age, data = df_robust)
print_with_ci(rob_inactive, "--- TABLE 8: INACTIVITY RATE WITH CI ---")


# Table 9: Logistic Regression Format (Requires exponentiating the CI)
logistic_model <- glm(EMP ~ Disability * Time + Gender + Race + Education + Age, data = df_robust, family = binomial(link = "logit"))
log_vcov <- vcovCL(logistic_model, cluster = ~ST)

logistic_coef <- coeftest(logistic_model, vcov = log_vcov)
logistic_ci <- coefci(logistic_model, vcov = log_vcov)

print("--- TABLE 9: LOGISTIC REGRESSION RESULTS (ODDS RATIOS WITH CI) ---")
print(cbind(Odds_Ratio = exp(logistic_coef[, 1]), exp(logistic_ci)))


# --- NEW ROBUSTNESS TEST: CENSUS SURVEY WEIGHTS (PWGTP) ---
acs_design <- svydesign(ids = ~ST, weights = ~PWGTP, data = df_robust)

weighted_baseline <- svyglm(EMP ~ Disability * Time + Gender + Race + Education + Age, design = acs_design)
print("--- NEW ROBUSTNESS: SURVEY WEIGHTED MODEL RESULTS WITH CI ---")
print(summary(weighted_baseline))
print(confint(weighted_baseline)) # Automatically calculates CI based on survey design