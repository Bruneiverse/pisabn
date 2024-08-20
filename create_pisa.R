library(tidyverse)

pisa_stu <- haven::read_sas("STU_QQQ_SAS/cy08msp_stu_qqq.sas7bdat")
pisa_sch <- haven::read_sas("SCH_QQQ_SAS/cy08msp_sch_qqq.sas7bdat")
pisa_cog <- haven::read_sas("STU_COG_SAS/CY08MSP_STU_COG.SAS7BDAT")

# Export raw data to csv for FYP students to analyse ---------------------------
write_csv(pisa_stu, file = "raw/cy08msp_stu_qqq.csv")
write_csv(pisa_sch, file = "raw/cy08msp_sch_qqq.csv")
write_csv(pisa_cog, file = "raw/CY08MSP_STU_COG.csv")

xlsx::write_xlsx()