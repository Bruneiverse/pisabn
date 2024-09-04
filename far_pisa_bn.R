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
    bullied = BULLIED,
    escs = ESCS,
    skip_sch = SKIPPING,
    devices = ST253Q01JA,
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
    sch_type = SC013Q01TA,
    boys = SC002Q01TA,
    girls = SC002Q02TA,
    # Student SES background percentage
    her_lang = SC211Q01JA,
    spe_learn = SC211Q02JA,
    unpri_house = SC211Q03JA,
    img_stu = SC211Q04JA,
    img_prnt = SC211Q05JA,
    ref_stu = SC211Q06JA,
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
    devices = factor(devices, labels = c("None", "One", "Two", "Three", "Four", "Five", "6:10", "Greater than 10")),
    mother = factor(mother, labels = c("ISCED level 3.4", "ISCED level 3.3", "ISCED level 2", "ISCED level 1", "She did not complete ISCED level 1")),
    father = factor(father, labels = c("ISCED level 3.4", "ISCED level 3.3", "ISCED level 2", "ISCED level 1", "She did not complete ISCED level 1")),
    language = factor(language, labels = c("Language of the test", "Other languages")),
    sch_type = factor(sch_type, labels = c("Public", "Private"))
  )

# Write the data set to a csv file
write_csv(pisa_bn_math, "data/pisa_bn_math.csv", na = "")

# Multilevel model -------------------------------------------------------------
library(lme4)
library(lmerTest)

# A simple linear model
mod0 <- lm(
  formula = score ~ gender + skip_sch + devices + mother + father + language + sch_type + unpri_house,
  data = pisa_bn_math
)
summary(mod0)

# Multilevel model
mod <- lmer(
  formula = score ~ escs + gender + skip_sch + devices + books +
    mother + father + language + sch_type + her_lang + spe_learn +
    unpri_house + img_stu + img_prnt + ref_stu + (1 | school),
  data = pisa_bn_math
)

step(mod)

mod <- lmer(
  formula = score ~ gender + skip_sch + devices + mother + father + language + sch_type + unpri_house + (1 | school),
  data = pisa_bn_math
)
summary(mod)


anova(mod, mod0)
# What to write
# 
# Significance of random intercept model
# 1. Significance of the variance of the school level effects. u ~ N(0, sigma_u^2) -- THERE DO EXIST variation in school wrt scores.
# 2. Write about the "range" of possible max/min scores due to school level effects.
# 3. Find some tutorials online that make use of lme4 plots.
# 
# Interpreting the fixed effects
# 1. Look at regression table and make story
# 





