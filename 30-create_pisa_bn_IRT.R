library(tidyverse)
LETTERS676 <- c(sapply(LETTERS, function(x) paste0(x, LETTERS)))
load("data/CY08MSP_STU_COG.RData")
load("data/CY08MSP_STU_QQQ.RData")
load("data/CY08MSP_SCH_QQQ.RData")

# Filter for Brunei
pisa_bn_cog <- filter(CY08MSP_STU_COG, CNT == "BRN")
pisa_bn_stu <- filter(CY08MSP_STU_QQQ, CNT == "BRN")
pisa_bn_sch <- filter(CY08MSP_SCH_QQQ, CNT == "BRN")

# Collect all scored responses -------------------------------------------------
labels_list <- 
  unlist(map(
    seq_len(ncol(pisa_bn_cog)), 
    \(x) attr(pisa_bn_cog[[x]], "label")
  ))
idx <- which(grepl("\\(Scored Response\\)", labels_list))
pisa_bn_cog <- select(
  pisa_bn_cog, 
  CNTSTUID,
  CNTSCHID,
  all_of(idx),
)

# pisa_bn_cog |>
#   pivot_longer(cols = -c(CNTSTUID, CNTSCHID), names_to = "item") |>
#   group_by(item, value) |>
#   summarise(y = n(), .groups = "drop") |>
#   ggplot(aes(item, y, fill = factor(value))) +
#   geom_bar(stat = "identity")
#   

# Add student and school variables ---------------------------------------------
pisa_bn_irt <-  
  pisa_bn_stu |>  # start with students data
  left_join(pisa_bn_sch) |>  # combine school data
  select(
    id = CNTSTUID,
    school = CNTSCHID, 
    stratum = STRATUM,
    gender = ST004D01T, 
    age = AGE,
    bullied = BULLIED,
    escs = ESCS,
    skip_sch = SKIPPING
    # continue here for other data
  ) |>
  left_join(pisa_bn_cog, by = join_by(id == CNTSTUID), keep = FALSE) |> # combine cognitive data
  arrange(school, id, stratum)
  
write_csv(pisa_bn_irt, "data/pisa_bn_irt.csv", na = "")
