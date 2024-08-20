# Download the PISA data set from the OECD website, specifically the SAS data
# files, which can then be read in using the {haven} package in R. There are 7
# data sets of interest:
# 
# 1. School questionnaire data file
# 2. Stdent questionnaire data file
# 3. Teacher questionnaire data file
# 4. Cognitive item data file
# 5. Questionnaire timing data file
# 6. Creative Thinking data file
# 7. Financial Literacy data file

# Download codebook ------------------------------------------------------------
if (!file.exists("CY08MSP_CODEBOOK_27thJune24.xlsx")) {
  download.file(
    "https://webfs.oecd.org/pisa2022/CY08MSP_CODEBOOK_27thJune24.xlsx",
    "CY08MSP_CODEBOOK_27thJune24.xlsx"
  )
}

# Download all zip files from OECD website -------------------------------------
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/_raw")) dir.create("data/_raw")
# run this code or otherwise browse
# https://www.oecd.org/en/data/datasets/pisa-2022-database.html 
# to download one  by one
zip_files <- c(
  # Data files
  "SCH_QQQ_SAS.zip",
  "STU_QQQ_SAS.zip",
  "TCH_QQQ_SAS.zip",
  "STU_COG_SAS.zip",
  "STU_TIM_SAS.zip",
  "CRT_SAS.zip",
  "FLT_SAS.zip",
  # Compendia
  "PISA2022_FinalRelease_Compendia_18thJune24.zip",
  "PISA2022_FinalRelease_Compendia_18thJune24_cog.zip",
  "PISA2022_FinalRelease_CrT_Compendia_18thJune24.zip",
  "PISA2022_FinalRelease_FLT_Compendia_27thJune24.zip"
)
for (i in seq_along(zip_files)) {
  if (!file.exists(paste0("data/_raw/", zip_files[i]))) {
    download.file(
      paste0("https://webfs.oecd.org/pisa2022/", zip_files[i]),
      paste0("data/_raw/", zip_files[i])
    )
  }
}

# Unzip all files --------------------------------------------------------------
files_to_unzip <- list.files("data/_raw", pattern = ".zip", full.names = TRUE)
for (i in seq_along(files_to_unzip)) {
  the_file <- tools::file_path_sans_ext(basename(files_to_unzip[i]))
  if (!dir.exists(paste0("data/", the_file))) {
    unzip(
      zipfile = files_to_unzip[i], 
      exdir = paste0("data/", the_file)
    )
  }
}
