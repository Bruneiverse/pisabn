library(tidyverse)

# Load PISA data
load("data/CY08MSP_STU_COG.RData")
load("data/CY08MSP_STU_QQQ.RData")
load("data/CY08MSP_SCH_QQQ.RData")

# What are the country codes?
unique(CY08MSP_STU_COG$CNT)

# Filter for ASEAN countries
asean_cnt <- c("BRN", "KHM", "IDN", "MYS", "PHL", "SGP", "VNM", "THA")

pisa_asean_cog <- filter(
  CY08MSP_STU_COG,
  CNT %in% asean_cnt
)   

pisa_asean_stu <- filter(
  CY08MSP_STU_QQQ, CNT %in% asean_cnt
)

pisa_asean_sch <- filter(
  CY08MSP_SCH_QQQ, 
  CNT %in% asean_cnt
)

# Sample size

nrow(pisa_asean_cog)
nrow(pisa_asean_stu)
nrow(pisa_asean_sch)

# save(pisa_asean_cog, pisa_asean_stu, pisa_asean_sch, file = "data/pisa_asean.RData")

# Computing the maths scores-------------------------------------------------------- 
labels_list <- 
  unlist(map(
    seq_len(ncol(pisa_asean_cog)), 
    \(x) attr(pisa_asean_cog[[x]], "label")
  ))

# idx <- which(grepl("\\(Scored Response\\)", labels_list))

check_SR <- grepl("\\(Scored Response\\)", labels_list)
check_CM <- grepl("CM", colnames(pisa_asean_cog))

check1 <- check_SR & check_CM 

check_PSR <- grepl("\\(Paper Scored Response\\)", labels_list) 
check_PM <- grepl("PM", colnames(pisa_asean_cog))

check2 <- check_PSR & check_PM

idx1 <- which(check1)
idx2 <- which(check2)

# Combine the indices and subset based on them
combined_idx <- unique(c(idx1, idx2))  # Use unique to avoid duplicates
cog_data <- pisa_asean_cog[, combined_idx]


math_score <-
  cog_data |>
  mutate(across(everything(), \(x) as.numeric(x > 0))) |>
  mutate(score = round(100 * rowMeans(across(everything()), na.rm = TRUE), 0)) |>
  pull(score)

# Independent variables based of student questionnaire----------------------------------------------------------------------------------

pisa_asean_math <-  
  pisa_asean_stu |>
  select(
    country = CNT, 
    gender = ST004D01T,
    mat_deg = ST006Q03JA, 
    pat_deg = ST008Q03JA,
    stu_help = ST270Q02JA,
    stu_safe = ST265Q03JA,
    stu_eff = ST307Q02JA,
    math_ext = ST276Q01JA,
    math_itp = ST276Q02JA,
    fam_eng = ST300Q08JA
  ) |>   
  mutate(score = math_score) |>
  # ADD FACTORS
  mutate(
    gender = factor(gender, labels = c("Female", "Male")),
    mat_deg = factor(mat_deg, labels = c("Yes","No")),
    pat_deg = factor(pat_deg, labels = c("Yes","No")),
    stu_help = factor(stu_help, labels = c( "Every lesson","Most lessons", "Some lessons","Never or almost never" )),
    stu_safe = factor (stu_safe, labels = c("Strongly agree","Agree","Disagree","Strongly Disagree")),
    stu_eff = factor (stu_eff, labels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")),
    math_ext = factor (math_ext, labels = c("Frequently", "Sometimes", "Rarely", "Never")),
    math_itp = factor(math_itp, labels = c("Frequently", "Sometimes", "Rarely", "Never")),
    fam_eng = factor(fam_eng, labels = c("Never or almost never", "About once or twice a year", "About once or twice a month", "About once or twice a week", "Every day or almost every day"))
    )

write_csv(pisa_asean_math, "data/pisa_asean_math.csv", na = "")

# Summary statistics -----------------------------------------------------------
pisa_asean_math |>
  group_by(country) |>
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(mean))

#check why KHM and VNM give NaN 
#find way how to view khm and vnm column in cog file

# Filter for KHM and VNM
 filter_khm_vnm <- pisa_asean_cog |> 
   filter (CNT %in% c ("KHM","VNM")) |>
   select (CNT, contains ("cm"))
 #Data to calculate score is not available for KHM and VNM
 

# Model ------------------------------------------------------------------------
mod <- lm(
  formula = 
    score ~ gender + country + mat_deg + 
    pat_deg + stu_help + stu_safe +
    stu_eff + math_ext + math_itp + fam_eng,
  data = drop_na(pisa_asean_math)
)
summary(mod)
#observations: only country, stu_safe, stu_eff and math_ext seems to significantly impact scores.

mod_step <- step(mod)
summary(mod_step)







# interaction effect
mod <- lm(
  formula = score ~ country * (fam_eng),
  data = pisa_asean_math
)
summary(mod)
