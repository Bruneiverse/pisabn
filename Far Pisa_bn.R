# Farhanah


# cog_data <-
select(
  pisa_bn_cog,
  CM033Q01S,
  
)



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
    birth_year = ST003D03T,
    digital_devices = ST253Q01JA,
    books = ST255Q01JA,
    mother = ST005Q01JA,
    father = ST007Q01JA,
    language = ST022Q01TA,
    
    
  ) |>   
  mutate(score = math_score)

write_csv(pisa_bn_math, "data/pisa_bn_math.csv", na = "")



# Farhanah 
# What are the names of the variables from the pisa_bn_sch data set

pisa_bn_math <-
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
    
    

# example model
mod <- lm(score ~ escs, data = pisa_bn_math)  
ggplot(pisa_bn_math, aes(escs, score, col = school)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  scale_colour_viridis_c()








