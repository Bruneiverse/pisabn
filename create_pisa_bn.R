library(tidyverse)
LETTERS676 <- c(sapply(LETTERS, function(x) paste0(x, LETTERS)))

pisa_bn_stu <- haven::read_sas("STU_QQQ_SAS/cy08msp_stu_qqq.sas7bdat") |>
  filter(CNT == "BRN")
pisa_bn_sch <- haven::read_sas("SCH_QQQ_SAS/cy08msp_sch_qqq.sas7bdat") |>
  filter(CNT == "BRN")
pisa_bn_cog <- haven::read_sas("STU_COG_SAS/CY08MSP_STU_COG.SAS7BDAT") |>
  filter(CNT == "BRN")

# My way of computing the math scores ------------------------------------------
labels_list <- map(seq_len(ncol(pisa_bn_cog)), 
                   \(x) attr(pisa_bn_cog[[x]], "label"))
idx <- which(grepl("\\(Scored Response\\)", labels_list))


pisa_bn_cog <- pisa_bn_cog[, c(1:(min(idx) - 1), idx)]

math_score <-
  pisa_bn_cog[, -(1:33)] |>
  mutate(across(everything(), \(x) as.numeric(x > 0))) |>
  mutate(score = round(100 * rowMeans(across(everything()), na.rm = TRUE), 0)) |>
  pull(score)

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
  
write_csv(pisa_bn_math, file = "pisa_bn_math.csv")


# ggplot(pisa_bn_math, aes(escs, score, col = school)) +
#   geom_point(size = 2, alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_colour_viridis_d() +
#   theme_bw() +
#   theme(legend.position = "none")
