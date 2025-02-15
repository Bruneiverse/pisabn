library(tidyverse)
LETTERS676 <- c(sapply(LETTERS, function(x) paste0(x, LETTERS)))
load("data/CY08MSP_STU_COG.RData")
load("data/CY08MSP_STU_QQQ.RData")
load("data/CY08MSP_SCH_QQQ.RData")
load("data/pisa_bn.RData")  # load pisa bn data

# What are the country codes?
unique(CY08MSP_STU_COG$CNT)

# Filter for Brunei
# pisa_bn_cog <- filter(CY08MSP_STU_COG, CNT == "BRN")
# pisa_bn_stu <- filter(CY08MSP_STU_QQQ, CNT == "BRN")
# pisa_bn_sch <- filter(CY08MSP_SCH_QQQ, CNT == "BRN")
# save(pisa_bn_cog, pisa_bn_stu, pisa_bn_sch, file = "data/pisa_bn.RData")

# Filter for ASEAN countries
pisa_asean_cog <- filter(
  CY08MSP_STU_COG,
  CNT %in% c("BRN", "MYS", "PHL")  # insert more countries as needed
)
pisa_asean_stu <- ...
pisa_asean_sch <- ...

# etc. for the stu and sch data

# cog_data <-
  select(
    pisa_bn_cog,
    CM033Q01S,
    
  )
  
  select( 
    pisa_bn_cog)

# see below
stu_data <-
  select(
    pisa_bn_stu,
    ST004D01T, 
    ST254Q06JA,
    ...
  )
















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

# pisa_bn_cog <- pisa_bn_cog[, c(1:(min(idx) - 1), idx)]

# Add student and school variables ---------------------------------------------
pisa_bn_math <-  # Haziqah rename this to pisa_asean_math
  pisa_bn_stu |>
  left_join(pisa_bn_sch) |>
  select(
    school = CNTSCHID, 
    gender = ST004D01T, 
    age = AGE,
    bullied = BULLIED,
    escs = ESCS,
    skip_sch = SKIPPING,
    # continue here
  ) |>   
  mutate(score = math_score)
  
write_csv(pisa_bn_math, "data/pisa_bn_math.csv", na = "")



# Farhanah 
# What are the names of the variables from the pisa_bn_sch data set


# example model
mod <- lm(score ~ escs, data = pisa_bn_math)  
ggplot(pisa_bn_math, aes(escs, score, col = school)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  scale_colour_viridis_c()




  
  

