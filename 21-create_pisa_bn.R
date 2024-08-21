library(tidyverse)
LETTERS676 <- c(sapply(LETTERS, function(x) paste0(x, LETTERS)))
load("data/CY08MSP_STU_COG.RData")
load("data/CY08MSP_STU_QQQ.RData")
load("data/CY08MSP_SCH_QQQ.RData")

# What are the country codes?
unique(CY08MSP_STU_COG$CNT)

# Filter for Brunei
pisa_bn_cog <- filter(CY08MSP_STU_COG, CNT == "BRN")
pisa_bn_stu <- filter(CY08MSP_STU_QQQ, CNT == "BRN")
pisa_bn_sch <- filter(CY08MSP_SCH_QQQ, CNT == "BRN")

pisa_asean_cog <- filter(
  CY08MSP_STU_COG,
  CNT %in% c("BRN", "MYS", "PHL")
)
# etc. for the stu and sch data











# My way of computing the cognitive scores -------------------------------------
labels_list <- 
  map(
    seq_len(ncol(pisa_bn_cog)), 
    \(x) attr(pisa_bn_cog[[x]], "label")
  )
idx <- which(grepl("\\(Scored Response\\)", labels_list))

math_score <-
  pisa_bn_cog[, idx] |>
  mutate(across(everything(), \(x) as.numeric(x > 0))) |>
  mutate(score = round(100 * rowMeans(across(everything()), na.rm = TRUE), 0)) |>
  pull(score)

pisa_bn_cog <- pisa_bn_cog[, c(1:(min(idx) - 1), idx)]

# Add student and school variables ---------------------------------------------
pisa_bn_math <-
  pisa_bn_stu |>
  mutate(score = math_score) |>
  left_join(pisa_bn_sch) |>
  select(
    score = score,
    school = CNTSCHID, 
    gender = ST004D01T, 
    age = AGE,
    bullied = BULLIED,
    escs = ESCS,
    skip_sch = SKIPPING
  ) |> 
  mutate(
    gender = case_when(
      gender == 1 ~ "F",
      gender == 2 ~ "M",
      TRUE ~ NA
    ) |> factor(),
    school = LETTERS676[as.numeric(factor(school))]
  ) |>
  drop_na() |>
  arrange(school)
  
write_csv(pisa_bn_math, file = "data/pisa_bn_math.csv")

# ggplot(pisa_bn_math, aes(escs, score, col = school)) +
#   geom_point(size = 2, alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_colour_viridis_d() +
#   theme_bw() +
#   theme(legend.position = "none")
