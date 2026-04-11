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


df |>
  pivot_longer(cols = c(playedSudoku, playedKillerSudoku),
               names_to = "Variable", values_to = "Response") |>
  mutate(Variable = recode(Variable,
                           "playedSudoku" = "Played Sudoku",
                           "playedKillerSudoku" = "Played Killer Sudoku")) |>
  group_by(group, Variable, Response) |>
  summarise(n = n(), .groups = "drop") |>
  ggplot(aes(x = group, y = n, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable) +
  scale_fill_manual(values = c("Yes" = "#2171b5", "No" = "#6baed6")) +
  labs(title = "Prior Sudoku Experience by Treatment Group", x = "Group", y = "Count") +
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

ggsave("./output/sudoku_experience.png", width = width, height = height, dpi = 300)

