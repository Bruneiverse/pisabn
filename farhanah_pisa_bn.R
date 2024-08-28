library(tidyverse)
load("data/pisa_bn.RData")  # load pisa bn data

# My way of computing the maths scores -----------------------------------------
labels_list <- 
  unlist(map(
    seq_len(ncol(pisa_bn_cog)), 
    \(x) attr(pisa_bn_cog[[x]], "label")
  ))
# idx <- which(grepl("\\(Scored Response\\)", labels_list))

check_SR <- grepl("\\(Scored Response\\)", labels_list)
check_CM <- grepl("CM", colnames(pisa_bn_cog))
check <- check_SR & check_CM
idx <- which(check)

cog_data <- pisa_bn_cog[, idx]

math_score <-
  cog_data |>
  mutate(across(everything(), \(x) as.numeric(x > 0))) |>
  mutate(score = round(100 * rowMeans(across(everything()), na.rm = TRUE), 0)) |>
  pull(score)

# Add student and school variables ---------------------------------------------
pisa_bn_math <-  
  pisa_bn_stu |>
  select(
    school = CNTSCHID, 
    gender = ST004D01T, 
    age = AGE,
    bullied = BULLIED,
    escs = ESCS,
    skip_sch = SKIPPING,
    birth_year = ST003D03T,
    digital_devices = ST253Q01JA,
    books = ST255Q01JA,
    mother = ST005Q01JA,
    father = ST007Q01JA,
    language = ST022Q01TA,
  ) |>   
  mutate(score = math_score)

# What are the names of the variables from the pisa_bn_sch data set
pisa_bn_sch2 <-
  pisa_bn_sch |>
  select(
    school = CNTSCHID,
    location = SC001Q01TA,
    school_type = SC013Q01TA,
    boys = SC002Q01TA,
    girls = SC002Q02TA,
    # Student SES background percentage
    heritage_language = SC211Q01JA,
    special_learning = SC211Q02JA,
    underprivileged_household = SC211Q03JA,
    immigrant_students = SC211Q04JA,
    immigrant_parents = SC211Q05JA,
    refugee_students = SC211Q06JA,
    )

# Merge the two data sets
pisa_bn_math <- 
  left_join(pisa_bn_math, pisa_bn_sch2, by = "school") |>
  drop_na()

# Need to convert to factors
pisa_bn_math <-
  pisa_bn_math |>
  mutate(
    gender = factor(gender, labels = c("Female", "Male")),
    # FIXME: ADD THE REST HERE
  )

# Write the data set to a csv file
write_csv(pisa_bn_math, "data/pisa_bn_math.csv", na = "")

# Multilevel model -------------------------------------------------------------
library(lme4)
library(lmerTest)

# A simple linear model
mod0 <- lm(
  formula = score ~ escs + gender + skip_sch,
  data = pisa_bn_math
)

# Multilevel model
mod <- lmer(
  formula = score ~ escs + gender + skip_sch + (1 | school),
  data = pisa_bn_math
)
