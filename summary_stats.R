library(dplyr)
library(tidyverse)
library(knitr)
library(readr)
library(ggplot2)

height = 4
width = 3.5

df <- read_csv("./data/raw/killer_sudoku_experiment_raw.csv") |>
  mutate(
    group = factor(group, levels = c("control", "video", "ai")),
    completed = puzzle2Correct == TRUE
  )
  

cov_summary <- df |>
  group_by(group) |>
  summarise(
    N = as.character(n()),
    `Age 18-24 (%)` = as.character(round(mean(ageRange == "18\u201324", na.rm=TRUE)*100, 1)),
    `Age 25-34 (%)` = as.character(round(mean(ageRange == "25\u201334", na.rm=TRUE)*100, 1)),
    `Age 35-44 (%)` = as.character(round(mean(ageRange == "35\u201344", na.rm=TRUE)*100, 1)),
    `Age 45-54 (%)` = as.character(round(mean(ageRange == "45\u201354", na.rm=TRUE)*100, 1)),
    `Age 55+ (%)` = as.character(round(mean(ageRange == "55+", na.rm=TRUE)*100, 1)),
    `Played Sudoku: Yes (%)` = as.character(round(mean(playedSudoku == "Yes", na.rm=TRUE)*100, 1)),
    `Played Killer Sudoku: Yes (%)` = as.character(round(mean(playedKillerSudoku == "Yes", na.rm=TRUE)*100, 1))
  ) |>
  pivot_longer(-group, names_to = "Covariate", values_to = "value") |>
  pivot_wider(names_from = group, values_from = value)

cov_summary

## Summary chart of assignment to treatment group
df |>
  count(group) |>
  ggplot(aes(x = group, y = n, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("control" = "#084594", "video" = "#2171b5", "ai" = "#6baed6")) +
  labs(title = "Treatment Group Assignment", x = "Group", y = "Count") +
  ylim(0, 25) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./output/group_assignment.png", width = width, height = height, dpi = 300)


## Covariate balance table
## Age distribution - faceted histogram
ggplot(df, aes(x = ageRange, fill = group)) +
  geom_bar(position = "dodge") +
  facet_wrap(~group) +
  scale_fill_manual(values = c("control" = "#084594", "video" = "#2171b5", "ai" = "#6baed6")) +
  labs(title = "Age Distribution by Treatment Group", x = "Age Range", y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    strip.text = element_text(size = 7),
    plot.title = element_text(size = 9)
  )
ggsave("./output/age_distribution.png", width = width, height = height, dpi = 300)

## Previous experience
df |>
  pivot_longer(cols = c(playedSudoku, playedKillerSudoku),
               names_to = "Variable", values_to = "Response") |>
  mutate(Response = factor(Response, levels = c("Yes", "No"))) |>
  mutate(Variable = recode(Variable,
                           "playedSudoku" = "Played Sudoku",
                           "playedKillerSudoku" = "Played Killer Sudoku")) |>
  group_by(group, Variable, Response) |>
  summarise(n = n(), .groups = "drop") |>
  complete(group, Variable, Response, fill = list(n = 0)) |>
  ggplot(aes(x = group, y = n, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable) +
  scale_fill_manual(values = c("Yes" = "#2171b5", "No" = "#6baed6")) +
  labs(title = "Prior Sudoku Experience by Treatment Group", x = "Group", y = "Count") +
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 2.5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 8),
    strip.text = element_text(size = 7),
    plot.title = element_text(size = 9),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )


## previous experience broken into smaller charts
# Smaller chart - killer sudoku
df |>
  mutate(playedKillerSudoku = factor(playedKillerSudoku, levels = c("Yes", "No"))) |>
  group_by(group, playedKillerSudoku) |>
  summarise(n = n(), .groups = "drop") |>
  complete(group, playedKillerSudoku, fill = list(n = 0)) |>
  ggplot(aes(x = group, y = n, fill = playedKillerSudoku)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = c("Yes" = "#2171b5", "No" = "#6baed6")) +
  labs(title = "Played Killer Sudoku", x = "Group", y = "Count", fill = "Response") +
  ylim(0, 23) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    plot.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7)
  )

ggsave("./output/killer_sudoku_exp.png", width = 3.5, height = 2, dpi = 300)

# Smaller chart - Played Sudoku
df |>
  mutate(playedSudoku = factor(playedSudoku, levels = c("Yes", "No"))) |>
  group_by(group, playedSudoku) |>
  summarise(n = n(), .groups = "drop") |>
  complete(group, playedSudoku, fill = list(n = 0)) |>
  ggplot(aes(x = group, y = n, fill = playedSudoku)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = c("Yes" = "#2171b5", "No" = "#6baed6")) +
  labs(title = "Played Sudoku", x = "Group", y = "Count", fill = "Response") +
  ylim(0, 23) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    plot.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7)
  )

ggsave("./output/sudoku_exp.png", width = 3.5, height = 2, dpi = 300)


## Compliance check

df <- df |>
  mutate(compliant = case_when(
    group == "video" ~ puzzle1ElapsedSeconds >= 279,
    group == "ai"    ~ aiMessageCount > 0,
    group == "control" ~ NA
  ))

df |>
  filter(group != "control") |>
  group_by(group, compliant) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(compliant = ifelse(compliant, "Compliant", "Non-Compliant")) |>
  ggplot(aes(x = group, y = n, fill = compliant)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Compliant" = "#2171b5", "Non-Compliant" = "#6baed6")) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Treatment Compliance by Group", x = "Group", y = "Count", fill = "Status") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )

ggsave("/Users/davidschaaf/MIDS/CausalInference241/mids_241_experiment/output/compliance.png", 
       width = 4.5, height = 3.5, dpi = 300)


## Summary variables for inline code

# Group ns
n_control <- nrow(df |> filter(group == "control"))
n_video   <- nrow(df |> filter(group == "video"))
n_ai      <- nrow(df |> filter(group == "ai"))
n_total   <- nrow(df)

# Age distribution by group (%)
age_pct <- df |>
  group_by(group) |>
  summarise(
    pct_18_24 = round(mean(ageRange == "18–24", na.rm=TRUE)*100, 1),
    pct_25_34 = round(mean(ageRange == "25–34", na.rm=TRUE)*100, 1),
    pct_35_44 = round(mean(ageRange == "35–44", na.rm=TRUE)*100, 1),
    pct_45_54 = round(mean(ageRange == "45–54", na.rm=TRUE)*100, 1),
    pct_55_plus = round(mean(ageRange == "55+",  na.rm=TRUE)*100, 1)
  )

age_control <- age_pct |> filter(group == "control")
age_video   <- age_pct |> filter(group == "video")
age_ai      <- age_pct |> filter(group == "ai")

# Prior experience (%)
exp_pct <- df |>
  group_by(group) |>
  summarise(
    pct_sudoku        = round(mean(playedSudoku == "Yes",       na.rm=TRUE)*100, 1),
    pct_killer_sudoku = round(mean(playedKillerSudoku == "Yes", na.rm=TRUE)*100, 1)
  )

exp_control <- exp_pct |> filter(group == "control")
exp_video   <- exp_pct |> filter(group == "video")
exp_ai      <- exp_pct |> filter(group == "ai")


# Compliance checks 
# Compliance
n_video_compliant     <- df |> filter(group == "video", compliant == TRUE)  |> nrow()
n_video_noncompliant  <- df |> filter(group == "video", compliant == FALSE) |> nrow()
n_ai_compliant        <- df |> filter(group == "ai",    compliant == TRUE)  |> nrow()
n_ai_noncompliant     <- df |> filter(group == "ai",    compliant == FALSE) |> nrow()

compliance_rate_video <- round(n_video_compliant / n_video * 100, 1)
compliance_rate_ai    <- round(n_ai_compliant    / n_ai    * 100, 1)


# Save variables for access inline
save(n_control, n_video, n_ai, n_total,
     age_control, age_video, age_ai,
     exp_control, exp_video, exp_ai,
     n_video_compliant, n_video_noncompliant, compliance_rate_video,
     n_ai_compliant, n_ai_noncompliant, compliance_rate_ai,
     file = "./data/processed/sudoku_summary.RData")