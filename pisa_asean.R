library(tidyverse)
library(gtsummary)
library(kableExtra)
library(ggplot2)
library(gt)


# Load PISA data----
load("data/CY08MSP_STU_COG.RData")
load("data/CY08MSP_STU_QQQ.RData")
load("data/CY08MSP_SCH_QQQ.RData")

# Filter for ASEAN countries----
asean_cnt <- c("BRN", "KHM", "IDN", "MYS", "PHL", "SGP", "VNM", "THA")

pisa_asean_cog <- filter(
  CY08MSP_STU_COG,
  CNT %in% asean_cnt)   

pisa_asean_stu <- filter(
  CY08MSP_STU_QQQ, CNT %in% asean_cnt)

pisa_asean_sch <- filter(
  CY08MSP_SCH_QQQ, 
  CNT %in% asean_cnt)


# Sample size----

nrow(pisa_asean_cog)
nrow(pisa_asean_stu)
nrow(pisa_asean_sch)


# Computing the maths scores----
labels_list <- 
  unlist(map(
    seq_len(ncol(pisa_asean_cog)), 
    \(x) attr(pisa_asean_cog[[x]], "label")
  ))

check_SR <- grepl("\\(Scored Response\\)", labels_list)
check_CM <- grepl("CM", colnames(pisa_asean_cog))

check1 <- check_SR & check_CM 

check_PSR <- grepl("\\(Paper Scored Response\\)", labels_list) 
check_PM <- grepl("PM", colnames(pisa_asean_cog))

check2 <- check_PSR & check_PM

idx1 <- which(check1)
idx2 <- which(check2)

combined_idx <- unique(c(idx1, idx2))  
cog_data <- pisa_asean_cog[, combined_idx]

math_score <-
  cog_data |>
  mutate(across(everything(), \(x) as.numeric(x > 0))) |>
  mutate(score = round(100 * rowMeans(across(everything()), na.rm = TRUE), 0)) |>
  pull(score)


# Independent variables based of student questionnaire----

pisa_asean_math <-  
  pisa_asean_stu |>
  select(
    country = CNT, 
    gender = ST004D01T,
    mat_deg = ST006Q03JA, 
    pat_deg = ST008Q03JA,
    stu_help = ST270Q02JA,
    stu_safe = ST265Q03JA,
    stu_eff = ST307Q02JA,
    math_ext = ST276Q01JA,
    math_itp = ST276Q02JA,
    fam_eng = ST300Q08JA
  ) |>   
  mutate(score = math_score) |>
  mutate(
    gender = factor(gender, labels = c("Female", "Male")),
    mat_deg = factor(mat_deg, labels = c("Yes","No")),
    pat_deg = factor(pat_deg, labels = c("Yes","No")),
    stu_help = factor(stu_help, labels = c( "Every lesson","Most lessons", "Some lessons","Never or almost never" )),
    stu_safe = factor (stu_safe, labels = c("Strongly agree","Agree","Disagree","Strongly Disagree")),
    stu_eff = factor (stu_eff, labels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")),
    math_ext = factor (math_ext, labels = c("Frequently", "Sometimes", "Rarely", "Never")),
    math_itp = factor(math_itp, labels = c("Frequently", "Sometimes", "Rarely", "Never")),
    fam_eng = factor(fam_eng, labels = c("Never or almost never", "About once or twice a year", "About once or twice a month", "About once or twice a week", "Every day or almost every day"))
  )

write.csv(pisa_asean_math, file = "pisa_asean_math.csv")


# Variables table----

total_obs <- nrow(pisa_asean_math)

# Function to generate a summary table with percentages and a label
generate_summary_table <- function(data, group_var, label, total) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(`Number of students` = n()) %>%
    mutate(Percentage = round(100 * `Number of students` / total, 2)) %>%
    mutate(Category = label) %>%
    rename("Group" = {{ group_var }}) %>%
    select(Category, everything())}

# Function to style tables
style_table <- function(table) {
  table %>%
    kable("html") %>%
    kable_styling(full_width = FALSE, position = "center", html_font = "Arial") %>%
    row_spec(0, bold = TRUE) %>%  
    row_spec(which(table$Category != ""), extra_css = "border-bottom: 1px solid black;") %>% 
    row_spec(1:nrow(table), background = "transparent") %>% 
    collapse_rows(columns = 1, valign = "top") %>%
    row_spec(1:nrow(table), extra_css = "border-bottom: none;") %>% 
    column_spec(1, width_min = "200px", extra_css = "padding-left: 10px;") %>% 
    column_spec(2, width_min = "150px", extra_css = "padding-right: 10px;") %>% 
    column_spec(3, width_min = "150px", extra_css = "padding-right: 10px;")}

# Country & Gender Table----

table_country_gender <- bind_rows(
  generate_summary_table(pisa_asean_math, country, "Country", total_obs),
  generate_summary_table(pisa_asean_math, gender, "Gender", total_obs))
styled_table_country_gender <- style_table(table_country_gender)
styled_table_country_gender

# Recreate table:

data1 <- data.frame(
  Category = c("Country", rep("", 7), "Gender", ""),
  Group = c("Brunei (BRN)", "Indonesia (IDN)", "Cambodia (KHM)", "Malaysia (MYS)", "Philippines (PHL)", "Singapore (SGP)", "Thailand (THA)", "Vietnam (VNM)", "Female", "Male"),
  `Number of Students` = c(
    "5576 (9.34%)", "13439 (22.50%)", "5279 (8.84%)", "7069 (11.84%)",
    "7193 (12.04%)", "6606 (11.06%)", "8495 (14.22%)", "6068 (10.16%)",
    "30540 (51.13%)", "29185 (48.87%)"))

kable(data1, align = c("l", "l", "l"), col.names = c("Category", "Group", "Number of Students")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "25%") %>%   
  column_spec(2, width = "30%") %>%  
  column_spec(3, width = "30%") 


# Degree Qualifications Table ----

table_mother_father_deg <- bind_rows(
  generate_summary_table(pisa_asean_math, mat_deg, "Mother's Degree Qualification", total_obs),
  generate_summary_table(pisa_asean_math, pat_deg, "Father's Degree Qualification", total_obs))

styled_table_mother_father_deg <- style_table(table_mother_father_deg)
styled_table_mother_father_deg

# Recreate table:

data2 <- tibble(
  Category = c("Mother's Degree Qualification", "Father's Degree Qualification"),
  Yes_Number = c(10923, 10964),
  Yes_Percentage = c("18.29%", "18.36%"),
  No_Number = c(41838, 40712),
  No_Percentage = c("70.05%", "68.17%"),
  NA_Number = c(6964, 8049),
  NA_Percentage = c("11.66%", "13.48%"))

data2 %>%
  mutate(
    Yes = paste(Yes_Number, "<br>", paste0("(", Yes_Percentage, ")")),
    No = paste(No_Number, "<br>", paste0("(", No_Percentage, ")")),
    NA_ = paste(NA_Number, "<br>", paste0("(", NA_Percentage, ")"))) %>%
  select(Category, Yes, No, NA_) %>%
  kable("html", col.names = c("Category", "Yes", "No", "NA"), escape = FALSE) %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Number of Students" = 3))


# Teacher Assistance Table ----

table_teacher <- bind_rows(
  generate_summary_table(pisa_asean_math, stu_help, "Teacher Assistance", total_obs))
styled_table_teacher <- style_table(table_teacher)
styled_table_teacher

# Recreate table:

data3 <- tibble(
  Category = c("Teacher Assistance"),
  Never_or_Almost_Never_Number = c(1857),
  Never_or_Almost_Never_Percentage = c("3.11%"),
  Some_Lessons_Number = c(10996),
  Some_Lessons_Percentage = c("18.41%"),
  Most_Lessons_Number = c(16598),
  Most_Lessons_Percentage = c("27.79%"),
  Every_Lesson_Number = c(27237),
  Every_Lesson_Percentage = c("45.60%"),
  NA_Number = c(3037),
  NA_Percentage = c("5.08%"))

data3 %>%
  mutate(
    Never_or_Almost_Never = paste(Never_or_Almost_Never_Number, "<br>", paste0("(", Never_or_Almost_Never_Percentage, ")")),
    Some_Lessons = paste(Some_Lessons_Number, "<br>", paste0("(", Some_Lessons_Percentage, ")")),
    Most_Lessons = paste(Most_Lessons_Number, "<br>", paste0("(", Most_Lessons_Percentage, ")")),
    Every_Lesson = paste(Every_Lesson_Number, "<br>", paste0("(", Every_Lesson_Percentage, ")")),
    NA_ = paste(NA_Number, "<br>", paste0("(", NA_Percentage, ")"))
  ) %>%
  select(Category, Every_Lesson, Most_Lessons, Some_Lessons, Never_or_Almost_Never, NA_) %>%
  kable("html", col.names = c("Category", "Every Lesson", "Most Lessons", "Some Lessons", "Never or Almost Never", "NA"), escape = FALSE) %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Number of Students" = 5))


# Student- Safety & Effort ----

table_safety_effort <- bind_rows(
  generate_summary_table(pisa_asean_math, stu_safe, "Student's Sense of Safety in the Classroom", total_obs),
  generate_summary_table(pisa_asean_math, stu_eff, "Student's Extra Effort in a Challenging Task", total_obs))
styled_table_safety_effort <- style_table(table_safety_effort)
styled_table_safety_effort

# Recreate table

data4 <- tibble(
  Category = c("Student's Sense of Safety in the Classroom", "Student's Extra Effort in a Challenging Task"),
  Strongly_Agree_Number = c(17022, 4597),
  Strongly_Agree_Percentage = c("28.50%", "7.70%"),
  Agree_Number = c(31242, 11781),
  Agree_Percentage = c("52.31%", "19.73%"),
  Neither_Agree_Nor_Disagree_Number = c(0, 4565),  
  Neither_Agree_Nor_Disagree_Percentage = c("0.00%", "7.64%"),
  Disagree_Number = c(2371, 1690),
  Disagree_Percentage = c("3.97%", "2.83%"),
  Strongly_Disagree_Number = c(900, 1048),
  Strongly_Disagree_Percentage = c("1.51%", "1.75%"),
  NA_Number = c(8190, 36044),
  NA_Percentage = c("13.71%", "60.35%"))

data4 %>%
  mutate(
    Strongly_Agree = paste(Strongly_Agree_Number, "<br>", paste0("(", Strongly_Agree_Percentage, ")")),
    Agree = paste(Agree_Number, "<br>", paste0("(", Agree_Percentage, ")")),
    Neither_Agree_Nor_Disagree = paste(Neither_Agree_Nor_Disagree_Number, "<br>", paste0("(", Neither_Agree_Nor_Disagree_Percentage, ")")),
    Disagree = paste(Disagree_Number, "<br>", paste0("(", Disagree_Percentage, ")")),
    Strongly_Disagree = paste(Strongly_Disagree_Number, "<br>", paste0("(", Strongly_Disagree_Percentage, ")")),
    NA_ = paste(NA_Number, "<br>", paste0("(", NA_Percentage, ")"))) %>%
  select(Category, Strongly_Agree, Agree, Neither_Agree_Nor_Disagree, Disagree, Strongly_Disagree, NA_) %>%
  kable("html", col.names = c("Category", "Strongly Agree", "Agree", "Neither Agree nor Disagree", "Disagree", "Strongly Disagree", "NA"), escape = FALSE) %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Number of Students" = 6))


# Academic tasks- Extraction & Interpretation ----

table_extraction_interpretation <- bind_rows(
  generate_summary_table(pisa_asean_math, math_ext, "Extraction of Maths Information from Visual Representations", total_obs),
  generate_summary_table(pisa_asean_math, math_itp, "Interpretation of Maths Solutions Based on Real-Life Situations", total_obs))
styled_table_extraction_interpretation <- style_table(table_extraction_interpretation)
styled_table_extraction_interpretation

# Recreate Table:

data5 <- tibble(
  Category = c("Extraction of Maths Information from Visual Representations", 
               "Interpretation of Maths Solutions Based on Real-Life Situations"),
  Frequently_Number = c(10892, 7756),
  Frequently_Percentage = c("18.24%", "12.99%"),
  Sometimes_Number = c(14736, 14947),
  Sometimes_Percentage = c("24.67%", "25.03%"),
  Rarely_Number = c(4906, 6764),
  Rarely_Percentage = c("8.21%", "11.33%"),
  Never_Number = c(2782, 3278),
  Never_Percentage = c("4.66%", "5.49%"),
  NA_Number = c(26409, 26980),
  NA_Percentage = c("44.22%", "45.17%"))

data5 %>%
  mutate(
    Frequently = paste(Frequently_Number, "<br>", paste0("(", Frequently_Percentage, ")")),
    Sometimes = paste(Sometimes_Number, "<br>", paste0("(", Sometimes_Percentage, ")")),
    Rarely = paste(Rarely_Number, "<br>", paste0("(", Rarely_Percentage, ")")),
    Never = paste(Never_Number, "<br>", paste0("(", Never_Percentage, ")")),
    NA_ = paste(NA_Number, "<br>", paste0("(", NA_Percentage, ")"))) %>%
  select(Category, Frequently, Sometimes, Rarely, Never, NA_) %>%
  kable("html", col.names = c("Category", "Frequently", "Sometimes", "Rarely", "Never", "NA"), escape = FALSE) %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Number of Students" = 5))


# Family Engagement Table ----

table_family <- bind_rows(
  generate_summary_table(pisa_asean_math, fam_eng, "
Parent or Family Engagement in the Student's School Life
", total_obs))
styled_table_family <- style_table(table_family)
styled_table_family

# Recreate table:

data6 <- tibble(
  Category = c("
Parent or Family Engagement in the Student's School Life"),
  Never_or_Almost_Never_Number = c(3857),
  Never_or_Almost_Never_Percentage = c("6.46%"),
  About_Once_or_Twice_a_Year_Number = c(4239),
  About_Once_or_Twice_a_Year_Percentage = c("7.10%"),
  About_Once_or_Twice_a_Month_Number = c(5894),
  About_Once_or_Twice_a_Month_Percentage = c("9.87%"),
  About_Once_or_Twice_a_Week_Number = c(7957),
  About_Once_or_Twice_a_Week_Percentage = c("13.32%"),
  Every_Day_or_Almost_Every_Day_Number = c(9804),
  Every_Day_or_Almost_Every_Day_Percentage = c("16.42%"),
  NA_Number = c(27974),
  NA_Percentage = c("46.84%"))

data6 %>%
  mutate(
    Never_or_Almost_Never = paste(Never_or_Almost_Never_Number, "<br>", paste0("(", Never_or_Almost_Never_Percentage, ")")),
    About_Once_or_Twice_a_Year = paste(About_Once_or_Twice_a_Year_Number, "<br>", paste0("(", About_Once_or_Twice_a_Year_Percentage, ")")),
    About_Once_or_Twice_a_Month = paste(About_Once_or_Twice_a_Month_Number, "<br>", paste0("(", About_Once_or_Twice_a_Month_Percentage, ")")),
    About_Once_or_Twice_a_Week = paste(About_Once_or_Twice_a_Week_Number, "<br>", paste0("(", About_Once_or_Twice_a_Week_Percentage, ")")),
    Every_Day_or_Almost_Every_Day = paste(Every_Day_or_Almost_Every_Day_Number, "<br>", paste0("(", Every_Day_or_Almost_Every_Day_Percentage, ")")),
    NA_ = paste(NA_Number, "<br>", paste0("(", NA_Percentage, ")"))) %>%
  select(Category, Never_or_Almost_Never, About_Once_or_Twice_a_Year, About_Once_or_Twice_a_Month, About_Once_or_Twice_a_Week, Every_Day_or_Almost_Every_Day, NA_) %>%
  kable("html", col.names = c("Category", "Never or Almost Never", "About Once or Twice a Year", "About Once or Twice a Month", "About Once or Twice a Week", "Every Day or Almost Every Day", "NA"), escape = FALSE) %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Number of Students" = 6))

# Model----

model <- lm(
  formula = 
    score ~ gender + country + mat_deg + 
    pat_deg + stu_help + stu_safe +
    stu_eff + math_ext + math_itp + fam_eng,
  data = drop_na(pisa_asean_math))

mod_step <- step(model)

summary(mod_step)

# regression table ----

#full table:

regression_table <- tbl_regression(
  mod_step,
  intercept = TRUE,                      
  estimate_fun = ~ style_sigfig(.x, digits = 5), 
  conf.int = TRUE,                        
  pvalue_fun = ~ style_pvalue(.x, digits = 3),
  label = list(gender = "Gender",
               country = "Country",
               mat_deg = "Mother's Degree Qualification",
               pat_deg = "Father's Degree Qualification",
               stu_help = "Teacher Assistance in the Classroom",
               stu_safe = "Student's Sense of Safety in the Classroom",
               stu_eff = "Student's Extra Effort in a Challenging Task",
               math_ext = "Extraction of Math Information from Visual Representations",
               math_itp = "Interpretation of Math Solutions Based on Real-Life Situations",
               fam_eng = "Parent or Family Engagement in the Student's School Life"))

regression_table

# separate this table into 2 part:

# part 1
reg_table_1 <- tbl_regression(
  mod_step,
  intercept = TRUE,                      
  estimate_fun = ~ style_sigfig(.x, digits = 5), 
  conf.int = TRUE,                        
  pvalue_fun = ~ style_pvalue(.x, digits = 3),
  label = list(
    gender = "Gender",
    country = "Country",
    mat_deg = "Mother's Degree Qualification",
    pat_deg = "Father's Degree Qualification",
    stu_help = "Teacher Assistance in the Classroom",
    stu_safe = "Student's Sense of Safety in the Classroom",
    stu_eff = "Student's Extra Effort in a Challenging Task",
    math_ext = "Extraction of Maths Information from Visual Representations",
    math_itp = "Interpretation of Maths Solutions Based on Real-Life Situations",
    fam_eng = "Parent or Family Engagement in the Student's School Life"),
  include = c("country", "mat_deg", "pat_deg","stu_safe"))

reg_table_1

regression_table_1 <- reg_table_1 %>% as_gt()

gtsave(
  regression_table_1,
  filename = "regression_table_1.png",
  vwidth = 1200,  
  vheight = 800,  
  expand = 0)

#part 2
reg_table_2 <- tbl_regression(
  mod_step,
  intercept = FALSE,                      
  estimate_fun = ~ style_sigfig(.x, digits = 5), 
  conf.int = TRUE,                        
  pvalue_fun = ~ style_pvalue(.x, digits = 3),
  label = list(
    gender = "Gender",
    country = "Country",
    mat_deg = "Mother's Degree Qualification",
    pat_deg = "Father's Degree Qualification",
    stu_help = "Teacher Assistance in the Classroom",
    stu_safe = "Student's Sense of Safety in the Classroom",
    stu_eff = "Student's Extra Effort in a Challenging Task",
    math_ext = "Extraction of Maths Information from Visual Representations",
    math_itp = "Interpretation of Maths Solutions Based on Real-Life Situations",
    fam_eng = "Parent or Family Engagement in the Student's School Life"),
  include = c("stu_eff", "math_ext", "math_itp","fam_eng"))

reg_table_2

regression_table_2 <- reg_table_2 %>% as_gt()
gtsave(
  regression_table_2,
  filename = "regression_table_2.png",
  vwidth = 2000,  
  vheight = 1000,  
  expand = 0)   


# model fit summary ----

fit_statistics <- tibble(
  Statistic = c("Residual Standard Error", "Multiple R-squared", "Adjusted R-squared", "F-statistic"),
  Value = c(15.96, 0.414, 0.4059, 51.38))

fit_statistics %>%
  gt() %>%
  cols_label(
    Statistic = "Statistic",
    Value = "Value") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())) %>%
  cols_width(
    Statistic ~ px(200),  
    Value ~ px(150))


# Degree graph ----

# Mother

summary_data_mat <- pisa_asean_math %>%
  filter(!is.na(mat_deg)) %>%
  group_by(country, mat_deg) %>%
  summarize(mean_score = mean(score, na.rm = TRUE))

# Plot interaction effect with geom_line
mother <- ggplot(summary_data_mat, aes(x = country, y = mean_score, color = mat_deg, group = mat_deg)) +
  geom_line(size = 1) +
  labs(
    x = "Country",
    y = "Mean Maths Score",
    color = "Mother's Degree Qualification"
  ) +
  theme_minimal()

mother

# Father

summary_data_pat <- pisa_asean_math %>%
  filter(!is.na(pat_deg)) %>%
  group_by(country, pat_deg) %>%
  summarize(mean_score = mean(score, na.rm = TRUE))

father <- ggplot(summary_data_pat, aes(x = country, y = mean_score, color = pat_deg, group = pat_deg)) +
  geom_line(size = 1) +
  labs(
    x = "Country",
    y = "Mean Maths Score",
    color = "Father's Degree Qualification"
  ) +
  theme_minimal()

father


# Student safety graph ----

mean_scores <- pisa_asean_math %>%
  filter(!is.na(stu_safe), !is.na(score)) %>%  
  group_by(country, stu_safe) %>%  
  summarize(mean_score = mean(score, na.rm = TRUE))  

safety <- ggplot(mean_scores, aes(x = country, y = mean_score, color = as.factor(stu_safe), group = as.factor(stu_safe))) +
  geom_line(size = 1) +  
  labs(
    x = "Country",
    y = "Mean Maths Scores",
    color = "Student's Sense of Safety in the Classroom"
  ) +
  theme_minimal()

safety


# Extraction graph ----

summary_data_math_ext <- pisa_asean_math %>%
  filter(!is.na(math_ext)) %>%
  group_by(country, math_ext) %>%
  summarize(mean_score = mean(score, na.rm = TRUE))

ext <- ggplot(summary_data_math_ext, aes(x = country, y = mean_score, color = math_ext, group = math_ext)) +
  geom_line(size = 1) +
  labs(
    x = "Country",
    y = "Mean Maths Score",
    color = "Extraction of Maths Information from Visual Representations"
  ) +
  theme_minimal()

ext

# Summary statistics ----
pisa_asean_math |>
  group_by(country) |>
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(mean))


# Rank ----

rank <- data.frame(
  Country = c("Singapore", "Vietnam", "Brunei", "Thailand", "Cambodia", 
              "Malaysia", "Indonesia", "Philippines"),
  Research_Result = c(1, 2, 3, 4, 5, 6, 7, 8),
  Official_PISA_Result = c(1, 2, 3, 5, 8, 4, 6, 7),
  Change_in_rank = c("No Change", "No Change", "No Change", "Down by 1", "Down by 3", "Up by 2", "Up by 1", "Up by 1")
)

rank %>%
  select(Country, Research_Result, Official_PISA_Result, Change_in_rank) %>%
  kable("html", col.names = c("Country", "Research Result", "Official PISA Result", "Change in Ranking"), escape = FALSE, align = 'c') %>%
  kable_styling("striped", full_width = F)


# 95% Confidence Interval ----

data <- data.frame(
  country = c("SGP", "VNM", "BRN", "THA", "KHM", "MYS", "IDN", "PHL"),
  mean = c(68.9, 47.6, 45.5, 40.8, 40.6, 40.0, 34.1, 30.1),
  sd = c(19.3, 21.2, 17.5, 19.2, 29.2, 15.5, 14.4, 12.9),
  n = c(6606, 6068, 5576, 8495, 5279, 7069, 13439, 7193))

data$ci_lower <- data_1$mean - 1.96 * (data$sd)
data$ci_upper <- data_1$mean + 1.96 * (data$sd)

data

ggplot(data, aes(x = mean, y = reorder(country, mean))) +
  geom_point(size = 3, color = "skyblue") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "gray") +
  labs(x = "Mean Maths Score", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(mean, 1)), vjust = 1.5, color = "black", size = 3) +
  geom_text(aes(x = ci_lower, label = round(ci_lower, 1)), vjust = 1.5, color = "black", size = 3) +
  geom_text(aes(x = ci_upper, label = round(ci_upper, 1)), vjust = 1.5, color = "black", size = 3)


