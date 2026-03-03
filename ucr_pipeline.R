library(haven)
library(dplyr)
source("src/config.R")

years = 1984:2003
ucr_list = list()

for (yr in years) {
  df = read_dta(file.path(config$ROOT, config$UCR_DIR, paste0("ucr_arrests_monthly_all_crimes_race_sex_", yr, ".dta")))
  df$date = as.Date(paste0("01/", df$month, "/", df$year), format="%d/%B/%Y")
  df_new = df[,c("ori","date","month","number_of_months_reported","population",
                 "fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
  df_new = df_new[df_new$number_of_months_reported == 12,]
  df_new$year = yr
  ucr_list[[as.character(yr)]] = df_new
}

ucr_full = bind_rows(ucr_list)
saveRDS(ucr_full, file.path(config$ROOT, config$PROCESSED, "ucr_clean_1984_2003.rds"))
