# Load necessary packages
# install.packages("readr")      # uncomment if needed
# install.packages("dplyr")      # uncomment if needed
library(readr)
library(dplyr)
library(tibble)
# 1. Load and prepare the data
df <- read_csv("Sec4_FP_JonasP_WillB_AlexH/data_folder/student_lifestyle_dataset..csv") %>%
  mutate(
    Stress_Level = factor(Stress_Level, levels = c("Low", "Moderate", "High")),
    Grades       = as.numeric(Grades)
  )

# 2. One‐way ANOVA
anova_model <- aov(Grades ~ Stress_Level, data = df)
anova_sum   <- summary(anova_model)[[1]]
anova_f     <- anova_sum["Stress_Level", "F value"]
anova_df1   <- anova_sum["Stress_Level", "Df"]
anova_df2   <- anova_sum["Residuals",    "Df"]
anova_p     <- anova_sum["Stress_Level", "Pr(>F)"]
# 1. Tukey HSD test
# 1. Run ANOVA and TukeyHSD
anova_model <- aov(Grades ~ Stress_Level, data = df)
tukey       <- TukeyHSD(anova_model, "Stress_Level")

# 2. Extract the numeric results
tukey_df <- as.data.frame(tukey$Stress_Level)

# 3.Move the rownames into a column for clarity

tukey_df <- rownames_to_column(tukey_df, var = "Comparison")

# 4. View the table
print(tukey_df)

# 2. (Optional) Plot the confidence intervals
plot(tukey, las = 1)
# 3. Multiple Linear Regression
lm_model  <- lm(
  Grades ~ Stress_Level
  + Study_Hours_Per_Day
  + Sleep_Hours_Per_Day
  + Extracurricular_Hours_Per_Day
  + Social_Hours_Per_Day
  + Physical_Activity_Hours_Per_Day,
  data = df
)
lm_sum    <- summary(lm_model)
f_lm      <- lm_sum$fstatistic[1]
df1_lm    <- lm_sum$fstatistic[2]
df2_lm    <- lm_sum$fstatistic[3]
p_lm      <- pf(f_lm, df1_lm, df2_lm, lower.tail = FALSE)

# 4. Two‐sample t‐test (Low vs High stress)
df_sub <- df %>% filter(Stress_Level %in% c("Low", "High"))
t_test <- t.test(Grades ~ Stress_Level, data = df_sub, var.equal = TRUE)
t_stat  <- unname(t_test$statistic)
df_t    <- unname(t_test$parameter)
p_t     <- t_test$p.value

# 5. Compile results into a table
results <- tibble::tibble(
  Test       = c("ANOVA", "Linear Regression", "T-test (Low vs High)"),
  Statistic  = c(anova_f, f_lm, t_stat),
  DF1        = c(anova_df1, df1_lm, NA),
  DF2        = c(anova_df2, df2_lm, df_t),
  p_value    = c(anova_p, p_lm, p_t)
)

print(results)