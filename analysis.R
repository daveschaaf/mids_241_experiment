# ==============================================================================
# DATASCI-241 Killer Sudoku Experiment - Final Analysis
# Author: Nonso Orji
# ==============================================================================

library(dplyr)
library(tidyverse)
library(sandwich)
library(lmtest)

# ---- Step 1: Load raw data ----

df <- read_csv("./data/raw/killer_sudoku_experiment_raw.csv") |>
  mutate(
    group = factor(group, levels = c("control", "video", "ai"))
  )

cat("Total participants loaded:", nrow(df), "\n")
cat("\nGroup sizes:\n")
print(table(df$group))

# ---- Step 2: Clean data ----
# Drop participants who:
#   1. Walked away mid-experiment (puzzle1 >= 7000 sec)
#   2. Didn't try AT ALL (puzzle1 < 60 sec AND no puzzle2 data)
# For remaining NA puzzle2Correct cases, treat as FALSE (did not complete)

df_clean <- df |>
  filter(puzzle1ElapsedSeconds < 7000) |>
  filter(!(puzzle1ElapsedSeconds < 60 & is.na(puzzle2Correct))) |>
  mutate(completed = ifelse(is.na(puzzle2Correct), FALSE, puzzle2Correct))

cat("Original sample:", nrow(df), "\n")
cat("Cleaned sample:", nrow(df_clean), "\n")
cat("Dropped:", nrow(df) - nrow(df_clean), "participants\n")
cat("\nCleaned group sizes:\n")
print(table(df_clean$group))
cat("\nCompletion by group (cleaned):\n")
print(table(df_clean$group, df_clean$completed))

# ---- Step 3: ITT Analysis on Completion ----

itt_completion <- lm(completed ~ group, data = df_clean)
itt_completion_robust <- coeftest(itt_completion, vcov = vcovHC(itt_completion, type = "HC1"))

cat("=== ITT: Effect on Puzzle Completion ===\n")
print(itt_completion_robust)

# ---- Step 4: ITT Analysis on Completion Time ----
# Use only participants with valid puzzle2 time data
df_time <- df_clean |>
  filter(!is.na(puzzle2ElapsedSeconds)) |>
  mutate(puzzle2ElapsedMinutes = puzzle2ElapsedSeconds / 60)

cat("Sample size for time analysis:", nrow(df_time), "\n")
cat("\nGroup sizes:\n")
print(table(df_time$group))

# Mean time by group
cat("\nMean time by group (minutes):\n")
df_time |>
  group_by(group) |>
  summarise(
    n = n(),
    mean_time = round(mean(puzzle2ElapsedMinutes), 2),
    sd_time = round(sd(puzzle2ElapsedMinutes), 2)
  ) |>
  print()

# ITT regression
itt_time <- lm(puzzle2ElapsedMinutes ~ group, data = df_time)
itt_time_robust <- coeftest(itt_time, vcov = vcovHC(itt_time, type = "HC1"))

cat("\n=== ITT: Effect on Completion Time (minutes) ===\n")
print(itt_time_robust)

# ---- Step 5: Define compliance and calculate compliance rates ----
# Same definition as Dave's summary_stats.R:
# Video: spent at least 279 seconds on puzzle 1 (length of video)
# AI: sent at least one message to the AI tutor

df_clean <- df_clean |>
  mutate(compliant = case_when(
    group == "video"   ~ puzzle1ElapsedSeconds >= 279,
    group == "ai"      ~ aiMessageCount > 0,
    group == "control" ~ NA
  ))

cat("=== Compliance by Group ===\n")
print(table(df_clean$group, df_clean$compliant, useNA = "ifany"))

# Calculate compliance rates
compliance_rate_video <- mean(df_clean$compliant[df_clean$group == "video"], na.rm = TRUE)
compliance_rate_ai <- mean(df_clean$compliant[df_clean$group == "ai"], na.rm = TRUE)

cat("\nCompliance rate (Video):", round(compliance_rate_video * 100, 1), "%\n")
cat("Compliance rate (AI):", round(compliance_rate_ai * 100, 1), "%\n")

# ---- Step 6: CACE (Complier Average Causal Effect) ----
# CACE = ITT effect / Compliance rate
# This estimates the effect on people who actually used the treatment

# ITT effects from Step 3 (completion)
itt_video_completion <- coef(itt_completion)["groupvideo"]
itt_ai_completion <- coef(itt_completion)["groupai"]

# CACE for completion
cace_video_completion <- itt_video_completion / compliance_rate_video
cace_ai_completion <- itt_ai_completion / compliance_rate_ai

cat("=== CACE: Effect on Puzzle Completion ===\n")
cat("ITT (Video):", round(itt_video_completion, 3), 
    "| Compliance:", round(compliance_rate_video, 3),
    "| CACE:", round(cace_video_completion, 3), "\n")
cat("ITT (AI):", round(itt_ai_completion, 3),
    "| Compliance:", round(compliance_rate_ai, 3),
    "| CACE:", round(cace_ai_completion, 3), "\n")

# ITT effects from Step 4 (time)
itt_video_time <- coef(itt_time)["groupvideo"]
itt_ai_time <- coef(itt_time)["groupai"]

# CACE for time
cace_video_time <- itt_video_time / compliance_rate_video
cace_ai_time <- itt_ai_time / compliance_rate_ai

cat("\n=== CACE: Effect on Completion Time (minutes) ===\n")
cat("ITT (Video):", round(itt_video_time, 3), 
    "| Compliance:", round(compliance_rate_video, 3),
    "| CACE:", round(cace_video_time, 3), "min\n")
cat("ITT (AI):", round(itt_ai_time, 3),
    "| Compliance:", round(compliance_rate_ai, 3),
    "| CACE:", round(cace_ai_time, 3), "min\n")

# ---- Step 7: Covariate adjustment ----
# Note: puzzle2GivenDigits has no variation in our sample (no participants 
# received the easier version yet), so we cannot adjust for it.
# Instead we adjust for prior experience (playedSudoku, playedKillerSudoku)

# Adjusted ITT on completion
itt_completion_adj <- lm(completed ~ group + playedSudoku + playedKillerSudoku, 
                         data = df_clean)
itt_completion_adj_robust <- coeftest(itt_completion_adj, 
                                      vcov = vcovHC(itt_completion_adj, type = "HC1"))

cat("=== ITT (Adjusted for prior experience): Effect on Completion ===\n")
print(itt_completion_adj_robust)

# Adjusted ITT on time
itt_time_adj <- lm(puzzle2ElapsedMinutes ~ group + playedSudoku + playedKillerSudoku, 
                   data = df_time)
itt_time_adj_robust <- coeftest(itt_time_adj, 
                                vcov = vcovHC(itt_time_adj, type = "HC1"))

cat("\n=== ITT (Adjusted for prior experience): Effect on Completion Time ===\n")
print(itt_time_adj_robust)

# ---- Step 8: Final results summary ----
library(ggplot2)

# ---- Summary table of all ITT and CACE results ----
results_table <- data.frame(
  Outcome = c("Completion (probability)", "Completion (probability)",
              "Time (minutes)", "Time (minutes)"),
  Treatment = c("Video", "AI Tutor", "Video", "AI Tutor"),
  ITT_Estimate = c(round(itt_video_completion, 3), round(itt_ai_completion, 3),
                   round(itt_video_time, 2), round(itt_ai_time, 2)),
  Compliance_Rate = c(round(compliance_rate_video, 3), round(compliance_rate_ai, 3),
                      round(compliance_rate_video, 3), round(compliance_rate_ai, 3)),
  CACE_Estimate = c(round(cace_video_completion, 3), round(cace_ai_completion, 3),
                    round(cace_video_time, 2), round(cace_ai_time, 2))
)

cat("=== Final Results Summary ===\n")
print(results_table)

# Save the table to output folder
write.csv(results_table, "./output/results_summary.csv", row.names = FALSE)

# ---- Visualization 1: Completion rates by group ----
completion_plot <- df_clean |>
  group_by(group) |>
  summarise(completion_rate = mean(completed)) |>
  ggplot(aes(x = group, y = completion_rate, fill = group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(completion_rate * 100, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("control" = "#084594", "video" = "#2171b5", "ai" = "#6baed6")) +
  ylim(0, 0.6) +
  labs(title = "Puzzle Completion Rate by Group",
       x = "Group", y = "Completion Rate") +
  theme_minimal() +
  theme(legend.position = "none")

print(completion_plot)
ggsave("./output/completion_rates.png", completion_plot, width = 5, height = 4, dpi = 300)

# ---- Visualization 2: Time to completion (only those with valid time data) ----
time_plot <- df_time |>
  ggplot(aes(x = group, y = puzzle2ElapsedMinutes, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = c("control" = "#084594", "video" = "#2171b5", "ai" = "#6baed6")) +
  labs(title = "Puzzle 2 Completion Time by Group",
       x = "Group", y = "Time (minutes)") +
  theme_minimal() +
  theme(legend.position = "none")

print(time_plot)
ggsave("./output/completion_times.png", time_plot, width = 5, height = 4, dpi = 300)

# ---- Save all key results to file ----
save(itt_completion, itt_completion_robust,
     itt_time, itt_time_robust,
     itt_completion_adj, itt_completion_adj_robust,
     itt_time_adj, itt_time_adj_robust,
     compliance_rate_video, compliance_rate_ai,
     cace_video_completion, cace_ai_completion,
     cace_video_time, cace_ai_time,
     results_table,
     file = "./data/processed/analysis_results.RData")

cat("\n=== Analysis complete! ===\n")
cat("Results saved to: ./data/processed/analysis_results.RData\n")
cat("Plots saved to: ./output/\n")
cat("Summary table saved to: ./output/results_summary.csv\n")