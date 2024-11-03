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

summary(mod)
step(mod)

anova(mod, mod0)

# Load necessary libraries
library(lme4)
library(tibble)

# Fit the multilevel model
model2 <- lmer(score ~ escs + skip_sch + gender + (1 | school), data = pisa_bn_math)

# Extract fixed effects
fixed_effects <- summary(model2)$coefficients

# Create tibble for fixed effects
fixed_effects_data <- tibble(
  Effect = rownames(fixed_effects),                   # Effect names
  Estimate = fixed_effects[, "Estimate"],             # Coefficient estimates
  `Std. Error` = fixed_effects[, "Std. Error"],       # Standard errors
  df = fixed_effects[, "df"],                         # Degrees of freedom
  `t value` = fixed_effects[, "t value"],             # t-values
  `Pr(>|t|)` = format.pval(fixed_effects[, "Pr(>|t|)"], digits = 3)  # p-values (formatted)
)

# View the result
fixed_effects_data


# Load the necessary package if not already loaded
library(dplyr)
# Calculate means
means <- pisa_bn_math %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    mean_escs = mean(escs, na.rm = TRUE),
    mean_genderMale = mean(gender == "Male", na.rm = TRUE),  # Assuming 'gender' is a character variable
    mean_skip_sch = mean(skip_sch, na.rm = TRUE)
  )
print(means)

# Load necessary libraries
library(tibble)
library(gtsummary)
# Create the data frame with the fixed effects data
fixed_effects_data <- tribble(
  ~Effect,       ~Estimate, ~`Std. Error`, ~df, ~`t value`, ~`Pr(>|t|)`,
  "(Intercept)",  47.2883,   1.0841,       52.7313, 43.619, "< 2e-16",
  "escs",         3.2080,    0.2582,       4852.5239, 12.425, "< 2e-16",
  "genderMale",   0.5538,    0.4881,       4858.7923, 1.135, "0.257",
  "skip_sch",    -4.2226,    0.5942,       4821.0841, -7.107, "1.36e-12"
)


install.packages("arm")
library(arm)

# Load necessary libraries
library(lme4)
library(ggplot2)
library(arm)

# Fit the model (math_score as outcome, escs, gender, skip_sch as fixed effects)
model <- lmer(score ~ escs + gender + skip_sch + (1 | school), data = pisa_bn_math)
pisa_bn_math |>
  select( gender, age, bullied, escs, books) |>
  tbl_summary(
  )

pisa_bn_math |>
  # mutate(school = forcats::fct_reorder(as.character(school), score)) |>
  ggplot(aes(school, score, group = school)) +
  geom_boxplot()
# Create a summary table using gtsummary
table_summary <- data_dictionary %>%
  tbl_summary(
    by = Level, # Split by "Level" (Student-Level, School-Level)
    label = list(
      `Variable Name` ~ "Variable Name",
      Description ~ "Description",
      `Original Code` ~ "Original Code"
    ),
    missing = "no" # Remove missing data footnote
  ) %>%
  bold_labels()
table_summary

library(dplyr)
library(gtsummary)

pisa_bn_math %>%
  select(gender, age, bullied, escs, books, skip_sch, digital_devices, mother, father, language, school_type, location,
         boys, girls, heritage_language, special_learning, underprivileged_household, immigrant_students, immigrant_parents, refugee_students) %>%
  tbl_summary()



# Load necessary libraries
library(lme4)
library(ggplot2)

# Fit the multilevel model (with random intercepts for schools)
model2 <- lmer(score ~ escs + skip_sch + gender + (1 | school), data = pisa_bn_math)

# Extract random effects (random intercepts for schools)
random_intercepts <- ranef(model2)$school

# Convert random intercepts to a data frame for plotting
random_intercepts_df <- data.frame(school = rownames(random_intercepts), 
                                   random_intercept = random_intercepts[,1])

# Calculate standard errors for the random effects
random_intercepts_se <- attr(ranef(model2)$school, "postVar")[1, 1, ]
random_intercepts_df$se <- sqrt(random_intercepts_se)

# Calculate confidence intervals (95%)
random_intercepts_df$lower <- random_intercepts_df$random_intercept - 1.96 * random_intercepts_df$se
random_intercepts_df$upper <- random_intercepts_df$random_intercept + 1.96 * random_intercepts_df$se

# Create the caterpillar plot using ggplot2
ggplot(random_intercepts_df, aes(x = reorder(school, random_intercept), y = random_intercept)) +
  geom_point() +  # Plot the random intercepts
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Add confidence intervals
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Clean and minimal theme
  labs(title = "Caterpillar Plot of Random Intercepts by School",
       x = "School",
       y = "Random Intercept (Deviation from Overall Mean)") +
  theme(axis.text.y = element_text(size = 6))  # Adjust text size for school labels


