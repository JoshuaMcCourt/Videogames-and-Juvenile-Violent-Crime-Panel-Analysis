library(fixest)
source("src/config.R")

full_merge = readRDS(file.path(config$ROOT, config$PROCESSED, "merged_panel.rds"))

m1 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population, full_merge, cluster = ~STATEFIP)
m2 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | ori, full_merge, cluster = ~STATEFIP)
m3 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | STATEFIP, full_merge, cluster = ~STATEFIP)
m4 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | ori + date, full_merge, cluster = ~STATEFIP)
m5 = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population | STATEFIP + date, full_merge, cluster = ~STATEFIP)

etable(m1, m2, m3, m4, m5)
