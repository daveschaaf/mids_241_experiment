# ==============================================================================
# DATASCI-241 Killer Sudoku Experiment - Randomization Inference
# Author: David Schaaf
# ==============================================================================

library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(patchwork)

df_clean <- read_csv("./data/processed/df_clean.csv") |>
  mutate(
    group = factor(group, levels = c("control", "video", "ai"))
  )

df_time <- df_clean |>
  filter(!is.na(puzzle2ElapsedSeconds)) |>
  mutate(puzzle2ElapsedMinutes = puzzle2ElapsedSeconds / 60)

setDT(df_clean)
setDT(df_time)



## Calculate ATE For Completion Rate
completion_rates <- df_clean[ , .(
  n = .N,
  completion_rate = mean(completed)
  ), keyby = group
]

control_baseline <- completion_rates[group == "control", completion_rate]
ate_video <- completion_rates[group == "video", completion_rate] - control_baseline
ate_ai <- completion_rates[group == "ai", completion_rate] - control_baseline

print(paste0("Baseline Completion Rate for Control: ", round(control_baseline,3)))
print(paste0("ATE for Video: ", round(ate_video,3)))
print(paste0("ATE for AI: ", round(ate_ai,3)))

## Completion Rate Randomization Inference
randomizations <- 1000
ri_comp_video <- numeric(randomizations)
ri_comp_ai <- numeric(randomizations)
for (i in 1:randomizations) {
  ri_data <- copy(df_clean)
  ri_data[,ri_group := sample(group)]
  ri_completion_rates <- ri_data[ , .(
    n = .N,
    completion_rate = mean(completed)
  ), keyby = ri_group]
  
  ri_baseline <- ri_completion_rates[ri_group == "control", completion_rate]
  
  ri_ate_video <- ri_completion_rates[ri_group == "video", completion_rate] - ri_baseline
  ri_comp_video[i] <- ri_ate_video
  
  ri_ate_ai <- ri_completion_rates[ri_group == "ai", completion_rate] - ri_baseline
  ri_comp_ai[i] <- ri_ate_ai
}    
    
# Compute p-values for completion
completion_video_p <- mean(abs(ri_comp_video) >= abs(ate_video))
completion_video_p

completion_ai_p <- mean(abs(ri_comp_ai) >= abs(ate_ai))
completion_ai_p

cat("Randomization Inference p-values for completion time:")
print(paste0("p-value for Video RI = ", round(completion_video_p,3)))
print(paste0("p-value for for AI RI = ", round(completion_ai_p,3)))


## Calculate ATE For Time
completion_times <- df_time[ , .(
  n = .N,
  completion_time = mean(puzzle2ElapsedMinutes)
), keyby = group]

control_baseline_t <- completion_times[group == "control", completion_time]
ate_t_video <- completion_times[group == "video", completion_time] - control_baseline_t
ate_t_ai <- completion_times[group == "ai", completion_time] - control_baseline_t

print(paste0("Baseline Completion Time for Control: ", round(control_baseline_t,3)))
print(paste0("ATE for Video: ", round(ate_t_video,3)))
print(paste0("ATE for AI: ", round(ate_t_ai,3)))

## Completion Time Randomization Inference
ri_t_video <- numeric(randomizations)
ri_t_ai <- numeric(randomizations)
for (i in 1:randomizations) {
  ri_data <- copy(df_time)
  ri_data[,ri_group := sample(group)]
  ri_times <- ri_data[ , .(
    n = .N,
    completion_time = mean(puzzle2ElapsedMinutes)
  ), keyby = ri_group]
  
  ri_t_baseline <- ri_times[ri_group == "control", completion_time]

  ri_t_ate_video <- ri_times[ri_group == "video", completion_time] - ri_t_baseline
  ri_t_video[i] <- ri_t_ate_video
  
  ri_t_ate_ai <- ri_times[ri_group == "ai", completion_time] - ri_t_baseline
  ri_t_ai[i] <- ri_t_ate_ai
}    

# Compute p-values for time
time_video_p <- mean(abs(ri_t_comp_video) >= abs(ate_video_t))
time_video_p

time_ai_p <- mean(abs(ri_t_comp_ai) >= abs(ate_ai_t))
time_ai_p

cat("Randomization Inference p-values for completion time:")
print(paste0("p-value for Video RI = ", round(time_video_p,3)))
print(paste0("p-value for AI RI = ", round(time_ai_p,3)))

## Save data
save(ri_comp_video, ri_comp_ai,
     ri_t_video, ri_t_ai,
     completion_video_p, completion_ai_p,
     time_video_p, time_ai_p,
     ate_video, ate_ai,
     ate_t_video, ate_t_ai,
     file = "./data/processed/ri_results.RData")


## Create Plots 

# Completion - Video
comp_video_plot <- ggplot(data.frame(ri = ri_comp_video), aes(x = ri)) +
  geom_histogram(bins = 30, fill = "#2171b5", alpha = 0.7) +
  geom_vline(xintercept = ate_video, color = "red", linewidth = 1) +
  labs(title = "RI Null Distribution: Completion Rate for Video",
       x = "Simulated ATE", y = "Count",) +
  annotate("text", 
           x = ate_video, 
           y = Inf,  # top of plot
           label = paste0("ATE = ", round(ate_video, 3), "\np = ", round(completion_video_p, 3)),
           hjust = -0.1, vjust = 1.5,
           color = "red",
           size = 6) +
  theme_minimal()
comp_video_plot

# Completion - AI
comp_ai_plot <- ggplot(data.frame(ri = ri_comp_ai), aes(x = ri)) +
  geom_histogram(bins = 30, fill = "#2171b5", alpha = 0.7) +
  geom_vline(xintercept = ate_ai, color = "red", linewidth = 1) +
  labs(title = "RI Null Distribution: Completion Rate for AI",
       x = "Simulated ATE", y = "Count",) +
  annotate("text", 
           x = ate_ai, 
           y = Inf,  # top of plot
           label = paste0("ATE = ", round(ate_ai, 3), "\np = ", round(completion_ai_p, 3)),
           hjust = -0.1, vjust = 1.5,
           color = "red",
           size = 6) +
  theme_minimal()
comp_ai_plot

# Time - Video
time_video_plot <- ggplot(data.frame(ri = ri_t_comp_video), aes(x = ri)) +
  geom_histogram(bins = 30, fill = "#2171b5", alpha = 0.7) +
  geom_vline(xintercept = ate_video, color = "red", linewidth = 1) +
  labs(title = "RI Null Distribution: Completion Time for Video",
       x = "Simulated ATE", y = "Count",) +
  annotate("text", 
           x = ate_video, 
           y = Inf,  # top of plot
           label = paste0("ATE = ", round(ate_video, 3), "\np = ", round(time_video_p, 3)),
           hjust = -0.1, vjust = 1.5,
           color = "red",
           size = 6) +
  theme_minimal()
time_video_plot

# Time - AI
time_ai_plot <- ggplot(data.frame(ri = ri_t_comp_ai), aes(x = ri)) +
  geom_histogram(bins = 30, fill = "#2171b5", alpha = 0.7) +
  geom_vline(xintercept = ate_ai_t, color = "red", linewidth = 1) +
  labs(title = "RI Null Distribution: Completion Time for AI",
       x = "Simulated ATE", y = "Count",) +
  annotate("text", 
           x = ate_ai_t, 
           y = Inf,  # top of plot
           label = paste0("ATE = ", round(ate_ai_t, 3), "\np = ", round(time_ai_p, 3)),
           hjust = -0.1, vjust = 1.5,
           color = "red",
           size = 6) +
  theme_minimal()
time_ai_plot


# Compose a single chart

ri_plot <- (comp_video_plot + comp_ai_plot) / (time_video_plot + time_ai_plot) +
  plot_annotation(
    title = "Randomization Inference Null Distributions",
    caption = "Red solid line = observed ATE."
  )

ggsave("./output/ri_distributions.png", ri_plot, width = 10, height = 8, dpi = 300)