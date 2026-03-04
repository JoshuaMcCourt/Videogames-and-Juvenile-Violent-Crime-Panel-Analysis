# Load libraries
library(fixest)
library(dplyr)
library(ggplot2)
library(stargazer)  # For presentable regression tables

# Load config
source("src/config.R")

# Create results directories if they do not exist
dir.create(file.path(config$ROOT, config$RESULTS, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(config$ROOT, config$RESULTS, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(config$ROOT, config$RESULTS, "logs"), recursive = TRUE, showWarnings = FALSE)

# Load merged panel dataset
full_merge = readRDS(
  file.path(config$ROOT, config$PROCESSED, "merged_panel.rds")
)

# Ensure key variables are integers
full_merge$STATEFIP = as.integer(full_merge$STATEFIP)
full_merge$year = as.integer(full_merge$year)


# Regression Specifications

m1 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population,
            data = full_merge, cluster = ~STATEFIP)

m2 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | ori,
            data = full_merge, cluster = ~STATEFIP)

m3 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | STATEFIP,
            data = full_merge, cluster = ~STATEFIP)

m4 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | ori + date,
            data = full_merge, cluster = ~STATEFIP)

m5 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | STATEFIP + date,
            data = full_merge, cluster = ~STATEFIP)


# Save Regression Table (LaTeX)

etable(m1, m2, m3, m4, m5,
       file = file.path(config$ROOT, config$RESULTS, "tables", "regression_table.tex"))

# Save Regression Table (HTML) for easy viewing
etable(m1, m2, m3, m4, m5,
       tex = FALSE,
       file = file.path(config$ROOT, config$RESULTS, "tables", "regression_table.html"))


# Save Regression Summaries to Logs

sink(file.path(config$ROOT, config$RESULTS, "logs", "model_summaries.txt"))

cat("MODEL 1\n")
print(summary(m1))
cat("\nMODEL 2\n")
print(summary(m2))
cat("\nMODEL 3\n")
print(summary(m3))
cat("\nMODEL 4\n")
print(summary(m4))
cat("\nMODEL 5\n")
print(summary(m5))

sink()


# Create Figures

# 1. Scatter of mean_videogame vs total juvenile violent crimes
ggplot(full_merge, aes(x = mean_videogame, y = total_violent_crimes_juv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Mean Videogame Exposure vs Total Juvenile Violent Crimes",
       x = "Mean Videogame Exposure",
       y = "Total Juvenile Violent Crimes")
ggsave(file.path(config$ROOT, config$RESULTS, "figures", "scatter_mean_videogame.png"),
       width = 8, height = 6)

# 2. Boxplot of total juvenile violent crimes by postrelease period
ggplot(full_merge, aes(x = factor(postrelease), y = total_violent_crimes_juv, fill = factor(postrelease))) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("#fc9272", "#a6bddb")) +
  labs(title = "Juvenile Violent Crimes by Postrelease Period",
       x = "Postrelease (0=pre, 1=post)",
       y = "Total Juvenile Violent Crimes")
ggsave(file.path(config$ROOT, config$RESULTS, "figures", "boxplot_postrelease.png"),
       width = 8, height = 6)

# Create Aggregated Tables for Reference

# Average crimes by state
avg_state = full_merge %>%
  group_by(STATEFIP) %>%
  summarise(
    avg_violent_crimes = mean(total_violent_crimes_juv, na.rm = TRUE),
    avg_videogame = mean(mean_videogame, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(avg_state,
          file.path(config$ROOT, config$RESULTS, "tables", "avg_crimes_by_state.csv"),
          row.names = FALSE)

# Average crimes by year
avg_year = full_merge %>%
  group_by(year) %>%
  summarise(
    avg_violent_crimes = mean(total_violent_crimes_juv, na.rm = TRUE),
    avg_videogame = mean(mean_videogame, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(avg_year,
          file.path(config$ROOT, config$RESULTS, "tables", "avg_crimes_by_year.csv"),
          row.names = FALSE)


# Save Summary Statistics

summary_stats = full_merge %>%
  summarise(
    min_crimes = min(total_violent_crimes_juv, na.rm = TRUE),
    max_crimes = max(total_violent_crimes_juv, na.rm = TRUE),
    mean_crimes = mean(total_violent_crimes_juv, na.rm = TRUE),
    median_crimes = median(total_violent_crimes_juv, na.rm = TRUE),
    sd_crimes = sd(total_violent_crimes_juv, na.rm = TRUE)
  )

write.csv(summary_stats,
          file.path(config$ROOT, config$RESULTS, "tables", "summary_statistics.csv"),
          row.names = FALSE)

cat("Regression analysis, tables, and figures successfully created and saved in 'results/'\n")
