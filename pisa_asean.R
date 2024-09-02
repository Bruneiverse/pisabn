library(tidyverse)
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

# save(pisa_asean_cog, pisa_asean_stu, pisa_asean_sch, file = "data/pisa_asean.RData")

# My way of computing the maths scores -----------------------------------------
labels_list <- 
  unlist(map(
    seq_len(ncol(pisa_asean_cog)), 
    \(x) attr(pisa_asean_cog[[x]], "label")
  ))
# idx <- which(grepl("\\(Scored Response\\)", labels_list))

check_SR <- grepl("\\(Scored Response\\)", labels_list)
check_CM <- grepl("CM", colnames(pisa_asean_cog))
check <- check_SR & check_CM
idx <- which(check)

cog_data <- pisa_asean_cog[, idx]

math_score <-
  cog_data |>
  mutate(across(everything(), \(x) as.numeric(x > 0))) |>
  mutate(score = round(100 * rowMeans(across(everything()), na.rm = TRUE), 0)) |>
  pull(score)
#pisa_asean_cog <- pisa_asean_cog[, c(1:(min(idx) - 1), idx)]

names(pisa_asean_stu) #Just to check the questionnaire label
names(pisa_asean_sch)

# Add student and school variables ---------------------------------------------
pisa_asean_math <-  
  pisa_asean_stu |>
  # left_join(pisa_asean_sch) |>
  select(
    country = CNT, 
    gender = ST004D01T,
    number_of_smartphones = ST254Q06JA,
    mother_qualification = ST006Q03JA,
    father_qualification = ST008Q03JA,
    student_well_being = ST267Q07JA,
    student_safety = ST265Q03JA,
    student_effort = ST307Q02JA,
    student_passion = ST301Q05JA,
    assigned_everyday_maths_problems = ST283Q06JA,
    real_life_maths_task = ST276Q02JA,
    family_interest_in_school_problem = ST300Q05JA,
    family_interest_in_school_learning = ST300Q08JA
  ) |>   
  mutate(score = math_score) |>
  # ADD FACTORS
  mutate(
    gender = factor(gender, labels = c("Female", "Male")),
    # FIXME: Add the rest here
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
  formula = score ~ gender + country,
  data = pisa_asean_math
)
