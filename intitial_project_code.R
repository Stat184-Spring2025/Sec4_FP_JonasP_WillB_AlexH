

student_raw <-  read.csv(
  "https://raw.githubusercontent.com/Stat184-Spring2025/Sec4_FP_JonasP_WillB_AlexH/refs/heads/main/student_lifestyle_dataset..csv",
  sep = ","
)
student_summary <- student_raw %>%
  select(Stress_Level, Grades) %>%
  group_by(Stress_Level) %>%
  summarize(
    count = n(),
    min = min(Grades),
    Q1 = quantile(Grades, 0.25),
    median = median(Grades),
    Q1 = quantile(Grades, 0.75),
    max = max(Grades),
    mad = mad(Grades),
    mean = mean(Grades),
    count = n(), 
    min = min(Grades), 
    Q1 = quantile(Grades, 0.25), 
    median = median(Grades), 
    Q3 = quantile(Grades, 0.75), 
    max = max(Grades), 
    mad = mad(Grades), 
    mean = mean(Grades), 
    sd = sd(Grades)
  )