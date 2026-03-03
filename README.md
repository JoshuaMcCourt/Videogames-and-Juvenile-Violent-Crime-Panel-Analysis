# Videogames-and-Juvenile-Violent-Crime-Panel-Analysis
State-level panel analysis of video game exposure and juvenile violent crime using IPUMS CPS and FBI UCR data (1984–2003).

# Video Game Exposure and Juvenile Violent Crime

## Overview

This repository contains the full replication pipeline for an empirical study
examining the relationship between video game exposure and juvenile violent crime
rates in the United States.

The analysis combines:

- IPUMS CPS microdata (individual-level media exposure proxies)
- FBI Uniform Crime Reports (UCR) arrest panel data
- State-level panel construction (1984–2003)
- Fixed-effects regression framework

All data processing, merging, and estimation are reproducible through a
modular R pipeline via `run_all.R`.

## Repository Structure

videogame-juvenile-violence/
├── README.md
├── .gitignore
├── renv.lock
│
├── data/
│ ├── raw/
│ │ ├── ipums/
│ │ │ └── CPSFull.csv
│ │ └── ucr/
│ │ └── ucr_arrests_monthly_all_crimes_race_sex_1974_2020_dta/
│ └── processed/
│ ├── CPSProcessed.csv
│ ├── ipums_clean.rds
│ ├── ucr_clean_1984_2003.rds
│ └── merged_panel.rds
│
├── full_pipeline/
│ └── full_pipeline.R
│
├── src/
│ ├── config.R
│ ├── utils.R
│ ├── ipums_pipeline.R
│ ├── ucr_pipeline.R
│ ├── merge_panel.R
│ └── regressions.R
│
├── results/
│ ├── figures/
│ ├── tables/
│ └── logs/
│
└── run_all.R

## Data Sources

### IPUMS CPS
Source: IPUMS CPS extracts  
Used for demographic composition and exposure proxies.

Raw location:
data/raw/ipums/CPSFull.csv

### UCR Arrest Data
Source: FBI UCR monthly arrest data  
Filtered to agencies reporting 12 months per year.

Raw location:
data/raw/ucr/ucr_arrests_monthly_all_crimes_race_sex_1974_2020_dta/

## How to Run the Full Pipeline

From project root:
source("run_all.R")

This will:
  - Process IPUMS microdata
  - Clean UCR arrest data
  - Construct state-year panel
  - Estimate regressions
  - Output results to /results

## Reproducibility

This project uses renv for package version control.

To restore required packages:
renv::restore()

## Outputs

Processed datasets:
  - ipums_clean.rds
  - ucr_clean_1984_2003.rds
  - merged_panel.rds

Results:
Tables → results/tables/
Figures → results/figures/
Logs → results/logs/
