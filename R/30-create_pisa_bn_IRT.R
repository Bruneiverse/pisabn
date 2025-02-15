library(tidyverse)
here::i_am("R/30-create_pisa_bn_IRT.R")
load(here::here("data/CY08MSP_STU_COG.RData"))
load(here::here("data/CY08MSP_STU_QQQ.RData"))
load(here::here("data/CY08MSP_SCH_QQQ.RData"))

# Filter for Brunei
pisa_bn_cog <- filter(CY08MSP_STU_COG, CNT == "BRN")
pisa_bn_stu <- filter(CY08MSP_STU_QQQ, CNT == "BRN")
pisa_bn_sch <- filter(CY08MSP_SCH_QQQ, CNT == "BRN")

# Collect all scored responses 
labels_list <- 
  unlist(map(
    seq_len(ncol(CY08MSP_STU_COG)), 
    \(x) attr(CY08MSP_STU_COG[[x]], "label")
  ))
idx <- which(grepl("\\(Scored Response\\)", labels_list))
pisa_bn_cog <- select(
  pisa_bn_cog, 
  CNTSTUID,
  CNTSCHID,
  all_of(idx),
)

# Remove columns that are all NAs
pisa_bn_cog <- pisa_bn_cog[, colSums(is.na(pisa_bn_cog)) < nrow(pisa_bn_cog)]

# Add student and school variables 
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
    # continue here for other data...
  ) |>
  # combine cognitive data
  left_join(pisa_bn_cog, 
            by = join_by(id == CNTSTUID, school == CNTSCHID), keep = FALSE) |>
  arrange(school, id, stratum)
  
write_csv(pisa_bn_irt, "pisa_bn_irt.csv", na = "")

## ----- Mathematics data set --------------------------------------------------
pisa_bn_maths <- 
  pisa_bn_irt |>
  select(id:skip_sch, starts_with("CM")) |>  # CM are the MCQ type items
  select(-starts_with("CMA"))                # CMA are alternative formats

write_csv(pisa_bn_maths, "pisa_bn_maths.csv", na = "")

# Note that there's one item which is a partial credit question
which(sapply(pisa_bn_maths, max, na.rm = TRUE) > 1)[-(1:7)]

pisa_bn_maths |>
  pivot_longer(cols = -(id:skip_sch), names_to = "item") |>
  group_by(item, value) |>
  summarise(y = n(), .groups = "drop") |>
  ggplot(aes(item, y, fill = factor(value))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(x = NULL, y = "Frequency", fill = "Response")
