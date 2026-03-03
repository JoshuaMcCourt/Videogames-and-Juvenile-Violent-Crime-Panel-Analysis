library(dplyr)
source("src/config.R")

ipums_data = readRDS(file.path(config$ROOT, config$PROCESSED, "ipums_clean.rds"))
ucr_full = readRDS(file.path(config$ROOT, config$PROCESSED, "ucr_clean_1984_2003.rds"))

state_data = ipums_data %>%
  group_by(STATEFIP) %>%
  summarise(
    count = n(),
    mean_videogame = mean(CICMPGAM, na.rm = TRUE),
    yes_videogame = sum(CICMPGAM == 1, na.rm = TRUE),
    no_videogame = sum(CICMPGAM == 2, na.rm = TRUE)
  )

ucr_full$fips_state_code = as.integer(ucr_full$fips_state_code)
state_data$STATEFIP = as.integer(state_data$STATEFIP)

full_merge = left_join(state_data, ucr_full, by = c("STATEFIP" = "fips_state_code")) %>%
  mutate(
    total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv,
    postrelease = ifelse(year >= 1992, 1, 0)
  )

saveRDS(full_merge, file.path(config$ROOT, config$PROCESSED, "merged_panel.rds"))
