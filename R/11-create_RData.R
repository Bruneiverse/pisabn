# The data/ folder contains the data in SAS format. This will be read in as R
# data frame objects. The file names are
#
# 1. CY08MSP_CRT_COG (Creative Thinking data)
# 2. CY08MSP_FLT_COG (Financial Literacy data)
# 3. CY08MSP_FLT_QQQ (Financial Literacy questionnaire data)
# 4. CY08MSP_FLT_TIM (Financial Literacy timing data)
# 5. CY08MSP_SCH_QQQ (School questionnaire data)
# 6. CY08MSP_STU_COG (Student cognitive item data)
# 7. CY08MSP_STU_QQQ (Student questionnaire data)
# 8. CY08MSP_STU_TIM (Student questionnaire timing data)
# 9. CY08MSP_TCH_QQQ (Teacher questionnaire data)

# Read in all the data files ---------------------------------------------------
sas_files <- list.files(
  "data", 
  pattern = ".SAS7BDAT", 
  recursive = TRUE, 
  full.names = TRUE
)

for (i in seq_along(sas_files)) {
  cli::cli_alert_info(paste0("Reading ", basename(sas_files[i])))
  assign(
    tools::file_path_sans_ext(basename(sas_files[i])),
    haven::read_sas(sas_files[i])
  )
}

# Save each into its own separate RData for easy loading later
save(CY08MSP_CRT_COG, file = "data/CY08MSP_CRT_COG.RData")
save(CY08MSP_FLT_COG, file = "data/CY08MSP_FLT_COG.RData")
save(CY08MSP_FLT_QQQ, file = "data/CY08MSP_FLT_QQQ.RData")
save(CY08MSP_FLT_TIM, file = "data/CY08MSP_FLT_TIM.RData")
save(CY08MSP_SCH_QQQ, file = "data/CY08MSP_SCH_QQQ.RData")
save(CY08MSP_STU_COG, file = "data/CY08MSP_STU_COG.RData")
save(CY08MSP_STU_QQQ, file = "data/CY08MSP_STU_QQQ.RData")
save(CY08MSP_STU_TIM, file = "data/CY08MSP_STU_TIM.RData")
save(CY08MSP_TCH_QQQ, file = "data/CY08MSP_TCH_QQQ.RData")
