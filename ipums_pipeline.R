install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")

library(haven)
library(dplyr)
library(ggplot2)
source("src/config.R")
source("src/utils.R")

full_ipums_data = read.csv(file.path(config$ROOT, config$IPUMS_RAW))
ipums_data0 = full_ipums_data[full_ipums_data$AGE %in% 10:17, ]
ipums_data = ipums_data0[ipums_data0$CICMPGAM %in% c(1, 2), ]
rm(ipums_data0)

write.csv(
  ipums_data,
  file.path(config$ROOT, config$PROCESSED, "CPSProcessed.csv"),
  row.names = FALSE
)

saveRDS(ipums_data, file.path(config$ROOT, config$PROCESSED, "ipums_clean.rds"))

count_numbers(ipums_data$CICMPGAM)
count_by_state(ipums_data)

ipums_data_filtered = full_ipums_data[full_ipums_data$AGE %in% 10:17 & full_ipums_data$CICMPGAM %in% 2, ]
age_counts = table(ipums_data_filtered$AGE)
print(age_counts)
