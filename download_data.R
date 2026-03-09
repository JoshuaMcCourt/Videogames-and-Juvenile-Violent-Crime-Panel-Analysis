# Install required packages if missing

if (!requireNamespace("utils", quietly = TRUE)) install.packages("utils")

# Define local folders

ipums_folder = "data/raw/ipums"
ucr_folder   = "data/raw/ucr"

dir.create(ipums_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(ucr_folder, recursive = TRUE, showWarnings = FALSE)

# URLs for direct download from OSF
# Make sure these are the direct download links (with /download?direct)

ipums_url = "https://osf.io/53k9r/download?direct"
ucr_url   = "https://osf.io/2cjes/download?direct"

# Destination files

ipums_dest = file.path(ipums_folder, "CPSFull.csv")
ucr_dest   = file.path(ucr_folder, "ucr_arrests_monthly_all_crimes_race_sex_1974_2020_dta.zip")

# Download IPUMS CSV

if (!file.exists(ipums_dest)) {
  cat("Downloading IPUMS CPS dataset...\n")
  download.file(url = ipums_url, destfile = ipums_dest, mode = "wb")
  cat("IPUMS download complete!\n\n")
} else {
  cat("IPUMS file already exists. Skipping download.\n\n")
}

# Download UCR ZIP

if (!file.exists(ucr_dest)) {
  cat("Downloading UCR dataset (large file, may take several minutes)...\n")
  
  # Use curl_download for robust large file download
  curl::curl_download(
    url = ucr_url,
    destfile = ucr_dest,
    mode = "wb"
  )
  
  cat("UCR download complete!\n\n")
} else {
  cat("UCR zip already exists. Skipping download.\n\n")
}

# Extract UCR ZIP

ucr_extract_folder = file.path(ucr_folder, "ucr_arrests_monthly_all_crimes_race_sex_1974_2020_dta")

if (!dir.exists(ucr_extract_folder) & file.exists(ucr_dest)) {
  cat("Extracting UCR dataset...\n")
  unzip(ucr_dest, exdir = ucr_extract_folder)
  cat("UCR extraction complete!\n\n")
} else if (dir.exists(ucr_extract_folder)) {
  cat("UCR data already extracted. Skipping unzip.\n\n")
} else {
  stop("UCR zip file not found. Check your download.")
}

cat("All raw data is ready in 'data/raw/'!\n")
