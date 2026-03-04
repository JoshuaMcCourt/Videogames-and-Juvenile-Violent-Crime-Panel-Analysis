# Full Structure

#videogame-juvenile-violence/
#├── README.md
#├── .gitignore
#│
#├── data/
#│   ├── raw/
#│   │   ├── ipums/
#│   │   │   └── CPSFull.csv
#│   │   └── ucr/
#│   │       └── ucr_arrests_monthly_all_crimes_race_sex_1974_2020_dta/
#│   └── processed/
#│       ├── CPSProcessed.csv
#│       ├── ipums_clean.rds
#│       ├── ucr_clean_1984_2003.rds
#│       └── merged_panel.rds
#│
#├── full_pipeline/
#│   └── full_pipeline.R
#│
#├── src/
#│   ├── config.R
#│   ├── utils.R
#│   ├── ipums_pipeline.R
#│   ├── ucr_pipeline.R
#│   ├── merge_panel.R
#│   └── regressions.R
#│
#├── results/
#│   ├── figures/
#│   ├── tables/
#│   └── logs/
#│
#└── run_all.R


# IPUMS Workings

install.packages("haven")
library(haven)
library(dplyr)

setwd("C:/Users/JoshuaMcCourt/Documents/Masters Work/Dissertation/Videogame Juvenile Violence/data/raw/ipums")

full_ipums_data = read.csv("C:/Users/JoshuaMcCourt/Documents/Masters Work/Dissertation/Videogame Juvenile Violence/data/raw/ipums/CPSFull.csv")
ipums_data0 = full_ipums_data[full_ipums_data$AGE %in% c(10, 11, 12, 13, 14, 15, 16, 17), ]
ipums_data = ipums_data0[ipums_data0$CICMPGAM %in% c(1, 2), ]
rm(list="ipums_data0")

count_numbers = function(data) {
  if (!is.numeric(data)) {
    stop("Need numeric input data")
  }
  counts = table(data, useNA = "always")
  return(counts)
}

count_by_state = function(data) {
  data %>%
    group_by(STATEFIP) %>%
    summarise(count = n(), count_CICMPGAM = sum(CICMPGAM %in% c(1, 2)))
}


count_numbers(ipums_data$CICMPGAM)
34946/(34946+8341) #Individuals who reported playing videogames against the ones who didn't

count_by_state(ipums_data) #Number of individuals who responded in each state

# Number of juveniles who played games from ages 10 to 17
ipums_data_filtered = full_ipums_data[full_ipums_data$AGE %in% c(10:17) & full_ipums_data$CICMPGAM %in% 2, ]
age_counts = table(ipums_data_filtered$AGE)
print(age_counts)

## Videogame exposure distribution

birth_67_70 = ipums_1984 %>%
  filter(AGE >= 14 & AGE <= 17)


birth_71_74_1 = ipums_1984 %>%
  filter(AGE >= 10 & AGE <= 13)
birth_71_74_2 = ipums_1989 %>%
  filter(AGE >= 15 & AGE <= 17)
birth_71_74 = bind_rows(
  birth_71_74_1,
  birth_71_74_2
)
rm(list="birth_71_74_1")
rm(list="birth_71_74_2")


birth_75_78_1 = ipums_1989 %>%
  filter(AGE >= 11 & AGE <= 14)
birth_75_78_2 = ipums_1993 %>%
  filter(AGE >= 15 & AGE <= 17)
birth_75_78 = bind_rows(
  birth_75_78_1,
  birth_75_78_2
)
rm(list="birth_75_78_1")
rm(list="birth_75_78_2")


birth_79_82_1 = ipums_1989 %>%
  filter(AGE >= 10)
birth_79_82_2 = ipums_1993 %>%
  filter(AGE >= 11 & AGE <= 14)
birth_79_82_3 = ipums_1997 %>%
  filter(AGE >= 15 & AGE <= 17)
birth_79_82 = bind_rows(
  birth_79_82_1,
  birth_79_82_2,
  birth_79_82_3
)
rm(list="birth_79_82_1")
rm(list="birth_79_82_2")
rm(list="birth_79_82_3")


birth_83_86_1 = ipums_1993 %>%
  filter(AGE >= 10)
birth_83_86_2 = ipums_1997 %>%
  filter(AGE >= 11 & AGE <= 14)
birth_83_86_3 = ipums_2001 %>%
  filter(AGE >= 15 & AGE <= 17)
birth_83_86 = bind_rows(
  birth_83_86_1,
  birth_83_86_2,
  birth_83_86_3
)
rm(list="birth_83_86_1")
rm(list="birth_83_86_2")
rm(list="birth_83_86_3")


birth_87_90_1 = ipums_1997 %>%
  filter(AGE >= 10)
birth_87_90_2 = ipums_2001 %>%
  filter(AGE >= 11 & AGE <= 14)
birth_87_90_3 = ipums_2003 %>%
  filter(AGE >= 13 & AGE <= 16)
birth_87_90 = bind_rows(
  birth_87_90_1,
  birth_87_90_2,
  birth_87_90_3
)
rm(list="birth_87_90_1")
rm(list="birth_87_90_2")
rm(list="birth_87_90_3")


birth_91_94_1 = ipums_2001 %>%
  filter(AGE >= 10)
birth_91_94_2 = ipums_2003 %>%
  filter(AGE >= 10 & AGE <= 12)
birth_91_94 = bind_rows(
  birth_91_94_1,
  birth_91_94_2
)
rm(list="birth_91_94_1")
rm(list="birth_91_94_2")


count_numbers(birth_67_70$CICMPGAM)
909/(909+306)
count_numbers(birth_71_74$CICMPGAM)
2049/(2049+575)
count_numbers(birth_75_78$CICMPGAM)
1870/(1870+1597)
count_numbers(birth_79_82$CICMPGAM)
6974/(6974+1852)
count_numbers(birth_83_86$CICMPGAM)
9858/(9858+3360)
count_numbers(birth_87_90$CICMPGAM)
16937/(16937+3360)
count_numbers(birth_91_94$CICMPGAM)
14891/(14891+2022)


# Birth year ranges
birth_year_ranges = c("1967-1970", "1971-1974", "1975-1978", "1979-1982", 
                       "1983-1986", "1987-1990", "1991-1994")

# Corresponding percentages
decimals = c(0.7481481, 0.7808689, 0.5393712, 0.7901654,
              0.7458012, 0.8344583, 0.880447)

# Create data frame for plotting
plot_data = data.frame(
  BirthYearRange = birth_year_ranges,
  Percentage = decimals
)

print(plot_data)

library(ggplot2)

# Bar plot of proportion of video game players by birth year range
ggplot(plot_data, aes(x = BirthYearRange, y = Percentage)) +
  geom_bar(stat = "identity", width = 0.75) +
  theme_minimal() +
  labs(
    title = "Proportion of Video Game Players by Birth Year Range",
    x = "Birth Year Range",
    y = "Proportion of Video Game Players"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, 1)

ggsave(
  file.path(config$ROOT, config$RESULTS, "figures", "bar_birthyear_videogame.png"),
  width = 8,
  height = 6
)

# UCR Workings

install.packages("haven")
library(haven)

install.packages("tidyverse")
library(tidyverse)

install.packages("data.table")
library(data.table)

install.packages("stringr")
library(stringr)

# Clear all objects from the R environment
rm(list=ls())

setwd("C:/Users/JoshuaMcCourt/Documents/Masters Work/Dissertation/Videogame Juvenile Violence/data/raw/ucr/ucr_arrests_monthly_all_crimes_race_sex_1974_2020_dta")

# Analysing data for the years 1984-2003

ucr_1984 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1984.dta")
colnames(ucr_1984)
colnames(ucr_1984)[1:100]
colnames(ucr_1984)[1000:1233]
ucr_1984$date = as.Date(paste0("01/",ucr_1984$month,"/",ucr_1984$year),format="%d/%B/%Y")
ucr_1984_new = ucr_1984[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1984_new = ucr_1984_new[ucr_1984_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1984")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1984 = names(table(ucr_1984_new$ori))

ucr_1985 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1985.dta")
colnames(ucr_1985)
colnames(ucr_1985)[1000:1233]
ucr_1985$date = as.Date(paste0("01/",ucr_1985$month,"/",ucr_1985$year),format="%d/%B/%Y")
ucr_1985_new = ucr_1985[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1985_new = ucr_1985_new[ucr_1985_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1985")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1985 = names(table(ucr_1985_new$ori))

ucr_1986 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1986.dta")
colnames(ucr_1986)
colnames(ucr_1986)[1000:1233]
ucr_1986$date = as.Date(paste0("01/",ucr_1986$month,"/",ucr_1986$year),format="%d/%B/%Y")
ucr_1986_new = ucr_1986[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1986_new = ucr_1986_new[ucr_1986_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1986")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1986 = names(table(ucr_1986_new$ori))

ucr_1987 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1987.dta")
colnames(ucr_1987)
colnames(ucr_1987)[1000:1233]
ucr_1987$date = as.Date(paste0("01/",ucr_1987$month,"/",ucr_1987$year),format="%d/%B/%Y")
ucr_1987_new = ucr_1987[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1987_new = ucr_1987_new[ucr_1987_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1987")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1987 = names(table(ucr_1987_new$ori))

ucr_1988 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1988.dta")
colnames(ucr_1988)
colnames(ucr_1988)[1000:1233]
ucr_1988$date = as.Date(paste0("01/",ucr_1988$month,"/",ucr_1988$year),format="%d/%B/%Y")
ucr_1988_new = ucr_1988[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1988_new = ucr_1988_new[ucr_1988_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1988")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1988 = names(table(ucr_1988_new$ori))

ucr_1989 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1989.dta")
colnames(ucr_1989)
colnames(ucr_1989)[1000:1233]
ucr_1989$date = as.Date(paste0("01/",ucr_1989$month,"/",ucr_1989$year),format="%d/%B/%Y")
ucr_1989_new = ucr_1989[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1989_new = ucr_1989_new[ucr_1989_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1989")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1989 = names(table(ucr_1989_new$ori))

ucr_1990 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1990.dta")
colnames(ucr_1990)
colnames(ucr_1990)[1000:1233]
ucr_1990$date = as.Date(paste0("01/",ucr_1990$month,"/",ucr_1990$year),format="%d/%B/%Y")
ucr_1990_new = ucr_1990[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1990_new = ucr_1990_new[ucr_1990_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1990")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1990 = names(table(ucr_1990_new$ori))

ucr_1991 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1991.dta")
colnames(ucr_1991)
colnames(ucr_1991)[1000:1233]
ucr_1991$date = as.Date(paste0("01/",ucr_1991$month,"/",ucr_1991$year),format="%d/%B/%Y")
ucr_1991_new = ucr_1991[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1991_new = ucr_1991_new[ucr_1991_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1991")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1991 = names(table(ucr_1991_new$ori))

ucr_1992 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1992.dta")
colnames(ucr_1992)
colnames(ucr_1992)[1000:1233]
ucr_1992$date = as.Date(paste0("01/",ucr_1992$month,"/",ucr_1992$year),format="%d/%B/%Y")
ucr_1992_new = ucr_1992[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1992_new = ucr_1992_new[ucr_1992_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1992")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1992 = names(table(ucr_1992_new$ori))

ucr_1993 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1993.dta")
colnames(ucr_1993)
colnames(ucr_1993)[1000:1233]
ucr_1993$date = as.Date(paste0("01/",ucr_1993$month,"/",ucr_1993$year),format="%d/%B/%Y")
ucr_1993_new = ucr_1993[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1993_new = ucr_1993_new[ucr_1993_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1993")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1993 = names(table(ucr_1993_new$ori))

ucr_1994 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1994.dta")
colnames(ucr_1994)
colnames(ucr_1994)[1000:1233]
ucr_1994$date = as.Date(paste0("01/",ucr_1994$month,"/",ucr_1994$year),format="%d/%B/%Y")
ucr_1994_new = ucr_1994[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1994_new = ucr_1994_new[ucr_1994_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1994")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1994 = names(table(ucr_1994_new$ori))

ucr_1995 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1995.dta")
colnames(ucr_1995)
colnames(ucr_1995)[1000:1233]
ucr_1995$date = as.Date(paste0("01/",ucr_1995$month,"/",ucr_1995$year),format="%d/%B/%Y")
ucr_1995_new = ucr_1995[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1995_new = ucr_1995_new[ucr_1995_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1995")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1995 = names(table(ucr_1995_new$ori))

ucr_1996 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1996.dta")
colnames(ucr_1996)
colnames(ucr_1996)[1000:1233]
ucr_1996$date = as.Date(paste0("01/",ucr_1996$month,"/",ucr_1996$year),format="%d/%B/%Y")
ucr_1996_new = ucr_1996[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1996_new = ucr_1996_new[ucr_1996_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1996")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1996 = names(table(ucr_1996_new$ori))

ucr_1997 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1997.dta")
colnames(ucr_1997)
colnames(ucr_1997)[1000:1233]
ucr_1997$date = as.Date(paste0("01/",ucr_1997$month,"/",ucr_1997$year),format="%d/%B/%Y")
ucr_1997_new = ucr_1997[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1997_new = ucr_1997_new[ucr_1997_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1997")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1997 = names(table(ucr_1997_new$ori))

ucr_1998 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1998.dta")
colnames(ucr_1998)
colnames(ucr_1998)[1000:1233]
ucr_1998$date = as.Date(paste0("01/",ucr_1998$month,"/",ucr_1998$year),format="%d/%B/%Y")
ucr_1998_new = ucr_1998[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1998_new = ucr_1998_new[ucr_1998_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1998")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1998 = names(table(ucr_1998_new$ori))

ucr_1999 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_1999.dta")
colnames(ucr_1999)
colnames(ucr_1999)[1000:1233]
ucr_1999$date = as.Date(paste0("01/",ucr_1999$month,"/",ucr_1999$year),format="%d/%B/%Y")
ucr_1999_new = ucr_1999[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_1999_new = ucr_1999_new[ucr_1999_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_1999")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_1999 = names(table(ucr_1999_new$ori))

ucr_2000 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_2000.dta")
colnames(ucr_2000)
colnames(ucr_2000)[1000:1233]
ucr_2000$date = as.Date(paste0("01/",ucr_2000$month,"/",ucr_2000$year),format="%d/%B/%Y")
ucr_2000_new = ucr_2000[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_2000_new = ucr_2000_new[ucr_2000_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_2000")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_2000 = names(table(ucr_2000_new$ori))

ucr_2001 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_2001.dta")
colnames(ucr_2001)
colnames(ucr_2001)[1000:1233]
ucr_2001$date = as.Date(paste0("01/",ucr_2001$month,"/",ucr_2001$year),format="%d/%B/%Y")
ucr_2001_new = ucr_2001[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_2001_new = ucr_2001_new[ucr_2001_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_2001")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_2001 = names(table(ucr_2001_new$ori))

ucr_2002 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_2002.dta")
colnames(ucr_2002)
colnames(ucr_2002)[1000:1233]
ucr_2002$date = as.Date(paste0("01/",ucr_2002$month,"/",ucr_2002$year),format="%d/%B/%Y")
ucr_2002_new = ucr_2002[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_2002_new = ucr_2002_new[ucr_2002_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_2002")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_2002 = names(table(ucr_2002_new$ori))

ucr_2003 = read_dta("ucr_arrests_monthly_all_crimes_race_sex_2003.dta")
colnames(ucr_2003)
colnames(ucr_2003)[1000:1233]
ucr_2003$date = as.Date(paste0("01/",ucr_2003$month,"/",ucr_2003$year),format="%d/%B/%Y")
ucr_2003_new = ucr_2003[,c("ori","date","month","number_of_months_reported","population","fips_state_code","murder_tot_juv","rape_tot_juv","robbery_tot_juv","agg_assault_tot_juv")]
ucr_2003_new = ucr_2003_new[ucr_2003_new$number_of_months_reported == 12,]
# Remove the original loaded dataset to free up memory
rm(list="ucr_2003")
# List of agency identifiers (ORI codes) that report for all 12 months
agencies_12_2003 = names(table(ucr_2003_new$ori))



# Merging Datasets

# 01-Alabama, 02-Alaska, 04-Arizona, 05-Arkansas, 06-California, 08-Colorado, 09-Connecticut, 10-Delaware, 11-District of Columbia, 12-Florida, 13-Georgia, 15-Hawaii, 16-Idaho, 17-Illinois, 18-Indiana, 19-Iowa, 20-Kansas, 21-Kentucky, 22-Louisiana, 23-Maine, 24-Maryland, 25-Massachusetts, 26-Michigan, 27-Minnesota, 28-Mississippi, 29-Missouri, 30-Montana, 31-Nebraska, 32-Nevada, 33-New Hampshire, 34-New Jersey, 35-New Mexico, 36-New York, 37-North Carolina, 38-North Dakota, 39-Ohio, 40-Oklahoma, 41-Oregon, 42-Pennsylvania, 44-Rhode Island, 45-South Carolina, 46-South Dakota, 47-Tennessee, 48-Texas, 49-Utah, 50-Vermont, 51-Virginia, 53-Washington, 54-West Virginia, 55-Wisconsin, 56-Wyoming

# New dataset at the STATEFIP level
library(dplyr)

# Summarize the data by STATEFIP
state_data = ipums_data %>%
  group_by(STATEFIP) %>%
  summarise(
    count = n(),
    mean_videogame = mean(CICMPGAM, na.rm = TRUE),
    yes_videogame = sum(CICMPGAM == 1, na.rm = TRUE),
    no_videogame = sum(CICMPGAM == 2, na.rm = TRUE)
  )

print(state_data)

socio_data = ipums_data %>%
  group_by(STATEFIP) %>%
  summarise(
    count = n(),
    mean_videogame = mean(CICMPGAM, na.rm = TRUE),
    yes_videogame = sum(CICMPGAM == 1, na.rm = TRUE),
    no_videogame = sum(CICMPGAM == 2, na.rm = TRUE),
    annual_mean_income = 12*mean(FAMINC, na.rm = TRUE),
    african_american = sum(RACE == 200, na.rm = TRUE)
  )

list_socio_data = socio_data %>%
  arrange(desc(mean_videogame)) %>%
  select(STATEFIP, mean_videogame, annual_mean_income, african_american)

print(list_socio_data, n=51)

#1984
# Convert fips_state_code to integer
if (!is.integer(ucr_1984_new$fips_state_code)) {
  ucr_1984_new$fips_state_code = as.integer(ucr_1984_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1984 = left_join(state_data, ucr_1984_new, by = c("STATEFIP" = "fips_state_code"))
merge_1984 = merge_1984 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1984$year = 1984
print(merge_1984)
count_numbers(merge_1984$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1984_new")

#1985
# Convert fips_state_code to integer
if (!is.integer(ucr_1985_new$fips_state_code)) {
  ucr_1985_new$fips_state_code = as.integer(ucr_1985_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1985 = left_join(state_data, ucr_1985_new, by = c("STATEFIP" = "fips_state_code"))
merge_1985 = merge_1985 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1985$year = 1985
print(merge_1985)
count_numbers(merge_1985$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1985_new")

#1986
# Convert fips_state_code to integer
if (!is.integer(ucr_1986_new$fips_state_code)) {
  ucr_1986_new$fips_state_code = as.integer(ucr_1986_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1986 = left_join(state_data, ucr_1986_new, by = c("STATEFIP" = "fips_state_code"))
merge_1986 = merge_1986 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1986$year = 1986
print(merge_1986)
count_numbers(merge_1986$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1986_new")

#1987
# Convert fips_state_code to integer
if (!is.integer(ucr_1987_new$fips_state_code)) {
  ucr_1987_new$fips_state_code = as.integer(ucr_1987_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1987 = left_join(state_data, ucr_1987_new, by = c("STATEFIP" = "fips_state_code"))
merge_1987 = merge_1987 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1987$year = 1987
print(merge_1987)
count_numbers(merge_1987$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1987_new")

#1988
# Convert fips_state_code to integer
if (!is.integer(ucr_1988_new$fips_state_code)) {
  ucr_1988_new$fips_state_code = as.integer(ucr_1988_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1988 = left_join(state_data, ucr_1988_new, by = c("STATEFIP" = "fips_state_code"))
merge_1988 = merge_1988 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1988$year = 1988
print(merge_1988)
count_numbers(merge_1988$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1988_new")

#1989
# Convert fips_state_code to integer
if (!is.integer(ucr_1989_new$fips_state_code)) {
  ucr_1989_new$fips_state_code = as.integer(ucr_1989_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1989 = left_join(state_data, ucr_1989_new, by = c("STATEFIP" = "fips_state_code"))
merge_1989 = merge_1989 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1989$year = 1989
print(merge_1989)
count_numbers(merge_1989$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1989_new")

#1990
# Convert fips_state_code to integer
if (!is.integer(ucr_1990_new$fips_state_code)) {
  ucr_1990_new$fips_state_code = as.integer(ucr_1990_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1990 = left_join(state_data, ucr_1990_new, by = c("STATEFIP" = "fips_state_code"))
merge_1990 = merge_1990 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1990$year = 1990
print(merge_1990)
count_numbers(merge_1990$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1990_new")

#1991
# Convert fips_state_code to integer
if (!is.integer(ucr_1991_new$fips_state_code)) {
  ucr_1991_new$fips_state_code = as.integer(ucr_1991_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1991 = left_join(state_data, ucr_1991_new, by = c("STATEFIP" = "fips_state_code"))
merge_1991 = merge_1991 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1991$year = 1991
print(merge_1991)
count_numbers(merge_1991$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1991_new")

#1992
# Convert fips_state_code to integer
if (!is.integer(ucr_1992_new$fips_state_code)) {
  ucr_1992_new$fips_state_code = as.integer(ucr_1992_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1992 = left_join(state_data, ucr_1992_new, by = c("STATEFIP" = "fips_state_code"))
merge_1992 = merge_1992 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1992$year = 1992
print(merge_1992)
count_numbers(merge_1992$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1992_new")

#1993
# Convert fips_state_code to integer
if (!is.integer(ucr_1993_new$fips_state_code)) {
  ucr_1993_new$fips_state_code = as.integer(ucr_1993_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1993 = left_join(state_data, ucr_1993_new, by = c("STATEFIP" = "fips_state_code"))
merge_1993 = merge_1993 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1993$year = 1993
print(merge_1993)
count_numbers(merge_1993$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1993_new")

#1994
# Convert fips_state_code to integer
if (!is.integer(ucr_1994_new$fips_state_code)) {
  ucr_1994_new$fips_state_code = as.integer(ucr_1994_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1994 = left_join(state_data, ucr_1994_new, by = c("STATEFIP" = "fips_state_code"))
merge_1994 = merge_1994 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1994$year = 1994
print(merge_1994)
count_numbers(merge_1994$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1994_new")

#1995
# Convert fips_state_code to integer
if (!is.integer(ucr_1995_new$fips_state_code)) {
  ucr_1995_new$fips_state_code = as.integer(ucr_1995_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1995 = left_join(state_data, ucr_1995_new, by = c("STATEFIP" = "fips_state_code"))
merge_1995 = merge_1995 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1995$year = 1995
print(merge_1995)
count_numbers(merge_1995$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1995_new")

#1996
# Convert fips_state_code to integer
if (!is.integer(ucr_1996_new$fips_state_code)) {
  ucr_1996_new$fips_state_code = as.integer(ucr_1996_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1996 = left_join(state_data, ucr_1996_new, by = c("STATEFIP" = "fips_state_code"))
merge_1996 = merge_1996 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1996$year = 1996
print(merge_1996)
count_numbers(merge_1996$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1996_new")

#1997
# Convert fips_state_code to integer
if (!is.integer(ucr_1997_new$fips_state_code)) {
  ucr_1997_new$fips_state_code = as.integer(ucr_1997_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1997 = left_join(state_data, ucr_1997_new, by = c("STATEFIP" = "fips_state_code"))
merge_1997 = merge_1997 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1997$year = 1997
print(merge_1997)
count_numbers(merge_1997$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1997_new")

#1998
# Convert fips_state_code to integer
if (!is.integer(ucr_1998_new$fips_state_code)) {
  ucr_1998_new$fips_state_code = as.integer(ucr_1998_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1998 = left_join(state_data, ucr_1998_new, by = c("STATEFIP" = "fips_state_code"))
merge_1998 = merge_1998 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1998$year = 1998
print(merge_1998)
count_numbers(merge_1998$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1998_new")

#1999
# Convert fips_state_code to integer
if (!is.integer(ucr_1999_new$fips_state_code)) {
  ucr_1999_new$fips_state_code = as.integer(ucr_1999_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_1999 = left_join(state_data, ucr_1999_new, by = c("STATEFIP" = "fips_state_code"))
merge_1999 = merge_1999 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_1999$year = 1999
print(merge_1999)
count_numbers(merge_1999$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_1999_new")

#2000
# Convert fips_state_code to integer
if (!is.integer(ucr_2000_new$fips_state_code)) {
  ucr_2000_new$fips_state_code = as.integer(ucr_2000_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_2000 = left_join(state_data, ucr_2000_new, by = c("STATEFIP" = "fips_state_code"))
merge_2000 = merge_2000 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_2000$year = 2000
print(merge_2000)
count_numbers(merge_2000$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_2000_new")

#2001
# Convert fips_state_code to integer
if (!is.integer(ucr_2001_new$fips_state_code)) {
  ucr_2001_new$fips_state_code = as.integer(ucr_2001_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_2001 = left_join(state_data, ucr_2001_new, by = c("STATEFIP" = "fips_state_code"))
merge_2001 = merge_2001 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_2001$year = 2001
print(merge_2001)
count_numbers(merge_2001$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_2001_new")

#2002
# Convert fips_state_code to integer
if (!is.integer(ucr_2002_new$fips_state_code)) {
  ucr_2002_new$fips_state_code = as.integer(ucr_2002_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_2002 = left_join(state_data, ucr_2002_new, by = c("STATEFIP" = "fips_state_code"))
merge_2002 = merge_2002 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_2002$year = 2002
print(merge_2002)
count_numbers(merge_2002$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_2002_new")

#2003
# Convert fips_state_code to integer
if (!is.integer(ucr_2003_new$fips_state_code)) {
  ucr_2003_new$fips_state_code = as.integer(ucr_2003_new$fips_state_code)
}

if (!is.integer(state_data$STATEFIP)) {
  state_data$STATEFIP = as.integer(state_data$STATEFIP)
}

merge_2003 = left_join(state_data, ucr_2003_new, by = c("STATEFIP" = "fips_state_code"))
merge_2003 = merge_2003 %>%
  mutate(total_violent_crimes_juv = murder_tot_juv + rape_tot_juv + robbery_tot_juv + agg_assault_tot_juv)
merge_2003$year = 2003
print(merge_2003)
count_numbers(merge_2003$STATEFIP)
# Remove the UCR dataset to free up memory
rm(list="ucr_2003_new")

# Full dataset

full_merge = rbind(merge_1984, merge_1985, merge_1986, merge_1987, merge_1988, 
                   merge_1989, merge_1990, merge_1991, merge_1992, merge_1993, 
                   merge_1994, merge_1995, merge_1996, merge_1997, merge_1998, 
                   merge_1999, merge_2000, merge_2001, merge_2002, merge_2003)
full_merge$postrelease = ifelse(full_merge$year >= 1992 & full_merge$year <= 2003, 1, 
                                 ifelse(full_merge$year >= 1984 & full_merge$year <= 1991, 0, NA))


# Regressions

install.packages("fixest")
library(fixest)

# Whole dataset

whole_base_post = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population, data = full_merge, cluster~STATEFIP)
summary(whole_base_post)
whole_base_full_ori = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population|ori, data = full_merge, cluster~STATEFIP)
summary(whole_base_full_ori)
whole_base_full_fip = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population|STATEFIP, data = full_merge, cluster~STATEFIP)
summary(whole_base_full_fip)
whole_base_full_ori_date = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population|ori+date, data = full_merge, cluster~STATEFIP)
summary(whole_base_full_ori)
whole_base_full_fip_date = feols(total_violent_crimes_juv ~ mean_videogame*postrelease + population|STATEFIP+date, data = full_merge, cluster~STATEFIP)
summary(whole_base_full_fip)

etable(whole_base_post, whole_base_full_ori, whole_base_full_fip, whole_base_full_ori_date, whole_base_full_fip_date, se = "white")

# Whole dataset with inverse hyperbolic sine transformation

inv_whole_base_post = feols(asinh(total_violent_crimes_juv) ~ mean_videogame*postrelease + population, data = full_merge, cluster~STATEFIP)
summary(inv_whole_base_post)
inv_whole_base_full_ori = feols(asinh(total_violent_crimes_juv) ~ mean_videogame*postrelease + population|ori, data = full_merge, cluster~STATEFIP)
summary(inv_whole_base_full_ori)
inv_whole_base_full_fip = feols(asinh(total_violent_crimes_juv) ~ mean_videogame*postrelease + population|STATEFIP, data = full_merge, cluster~STATEFIP)
summary(inv_whole_base_full_fip)
inv_whole_base_full_ori_date = feols(asinh(total_violent_crimes_juv) ~ mean_videogame*postrelease + population|ori+date, data = full_merge, cluster~STATEFIP)
summary(inv_whole_base_full_ori)
inv_whole_base_full_fip_date = feols(asinh(total_violent_crimes_juv) ~ mean_videogame*postrelease + population|STATEFIP+date, data = full_merge, cluster~STATEFIP)
summary(inv_whole_base_full_fip)

etable(inv_whole_base_post, inv_whole_base_full_ori, inv_whole_base_full_fip, inv_whole_base_full_ori_date, inv_whole_base_full_fip_date, se = "white")
