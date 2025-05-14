# EPDS (Edinburgh Postnatal Depression Scale) Comprehensive Analysis
# This script performs a complete analysis of EPDS survey data including:
# 1. Data preparation and scoring
# 2. Descriptive statistics
# 3. Reliability analysis
# 4. Risk stratification
# 5. Clinical recommendations

# Load required packages --------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data manipulation and visualization
  psych,        # Psychometric analysis
  gtsummary,    # Publication-ready tables
  factoextra,   # Factor analysis visualization
  knitr,        # RMarkdown integration
  here          # File path management
)

# Data Preparation -------------------------------------------------------------

# Convert variables to lowercase in the original data
df <- df %>% rename_with(tolower)

# Create properly scored variables (0-3 scale typical for EPDS, adjusting for your Y/N format)
# Note: Items 1 & 2 are positive and should be reverse scored
df <- df %>%
  mutate(
    # Score each item (1 = symptom present, 0 = absent, reverse for positive items)
    epds0101_score = ifelse(epds0101_y == 1, 0, ifelse(epds0101_n == 1, 1, NA)),
    epds0102_score = ifelse(epds0102_y == 1, 0, ifelse(epds0102_n == 1, 1, NA)),
    epds0103_score = ifelse(epds0103_y == 1, 1, ifelse(epds0103_n == 1, 0, NA)),
    epds0104_score = ifelse(epds0104_y == 1, 1, ifelse(epds0104_n == 1, 0, NA)),
    epds0105_score = ifelse(epds0105_y == 1, 1, ifelse(epds0105_n == 1, 0, NA)),
    epds0106_score = ifelse(epds0106_y == 1, 1, ifelse(epds0106_n == 1, 0, NA)),
    epds0107_score = ifelse(epds0107_y == 1, 1, ifelse(epds0107_n == 1, 0, NA)),
    epds0108_score = ifelse(epds0108_y == 1, 1, ifelse(epds0108_n == 1, 0, NA)),
    epds0109_score = ifelse(epds0109_y == 1, 1, ifelse(epds0109_n == 1, 0, NA)),
    epds0110_score = ifelse(epds0110_y == 1, 1, ifelse(epds0110_n == 1, 0, NA)),
    
    # Calculate total score (range 0-30)
    epds_total = rowSums(across(ends_with("_score")), na.rm = FALSE),
    
    # Categorize based on standard clinical cutoffs
    epds_category = case_when(
      epds_total >= 13 ~ "high_risk",
      epds_total >= 10 ~ "possible_depression",
      epds_total < 10 ~ "low_risk",
      is.na(epds_total) ~ "missing"
    ) %>% factor(levels = c("low_risk", "possible_depression", "high_risk", "missing")),
    
    # Flag for critical item (suicidal ideation)
    suicidal_risk = ifelse(epds0110_y == 1, "yes", "no") %>% factor(levels = c("no", "yes"))
  )

# Data Quality Check -----------------------------------------------------------
cat("\n=== Missing Data Analysis ===\n")
print(sapply(select(df, starts_with("epds")), function(x) sum(is.na(x))))

# Descriptive Statistics --------------------------------------------------------
cat("\n=== EPDS Score Distribution ===\n")
summary(df$epds_total)

# Score distribution visualization
ggplot(df, aes(x = epds_total)) +
  geom_histogram(binwidth = 1, fill = "#3498db", color = "white") +
  geom_vline(xintercept = c(10, 13), linetype = "dashed", color = "#e74c3c") +
  labs(title = "Distribution of EPDS Total Scores",
       subtitle = "Red dashed lines show clinical cutoffs (10 = possible, 13 = probable depression)",
       x = "Total EPDS Score (0-30)",
       y = "Number of Participants") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "epds_score_distribution.png"), width = 8, height = 6, dpi = 300)

# Item Analysis ----------------------------------------------------------------
cat("\n=== Item-Level Statistics ===\n")
item_stats <- df %>%
  select(ends_with("_score")) %>%
  psych::describe() %>%
  as_tibble(rownames = "item") %>%
  select(item, n, mean, sd, min, max)

print(kable(item_stats, digits = 2, caption = "EPDS Item-Level Statistics"))

# Reliability Analysis ---------------------------------------------------------
cat("\n=== Scale Reliability ===\n")
alpha_results <- df %>%
  select(ends_with("_score")) %>%
  psych::alpha(check.keys = TRUE)

print(alpha_results)

# Risk Stratification ----------------------------------------------------------
cat("\n=== Depression Risk Categories ===\n")
risk_table <- df %>%
  count(epds_category) %>%
  mutate(percent = round(n/sum(n)*100, 1))

print(kable(risk_table, caption = "Participants by Depression Risk Category"))

# Suicidal Ideation Analysis
cat("\n=== Suicidal Ideation (Item 10) by Risk Category ===\n")
suicide_risk_table <- df %>%
  group_by(epds_category) %>%
  count(suicidal_risk) %>%
  mutate(percent = round(n/sum(n)*100, 1))

print(kable(suicide_risk_table, caption = "Suicidal Ideation Prevalence"))

# Clinical Recommendations -----------------------------------------------------
df <- df %>%
  mutate(
    clinical_action = case_when(
      epds_category == "high_risk" ~ "Urgent clinical assessment",
      epds_category == "possible_depression" & suicidal_risk == "yes" ~ "Immediate assessment with suicide evaluation",
      epds_category == "possible_depression" ~ "Follow-up assessment within 1 week",
      epds_category == "low_risk" & suicidal_risk == "yes" ~ "Suicide risk evaluation",
      epds_category == "low_risk" ~ "Routine follow-up",
      TRUE ~ "Missing data - reassessment needed"
    )
  )

# Export High Risk Cases -------------------------------------------------------
high_risk_cases <- df %>%
  filter(epds_category %in% c("high_risk", "possible_depression") | suicidal_risk == "yes")

write_csv(high_risk_cases, here("output", "high_risk_cases.csv"))

# Generate Report --------------------------------------------------------------
cat("\n=== Analysis Complete ===\n")
cat(paste0(
  "\nKey Findings:\n",
  "1. ", sum(df$epds_category == "high_risk", na.rm = TRUE), " participants at high risk\n",
  "2. ", sum(df$suicidal_risk == "yes", na.rm = TRUE), " participants reported suicidal thoughts\n",
  "3. Scale reliability (Cronbach's α) = ", round(alpha_results$total$raw_alpha, 2), "\n",
  "\nOutput files saved to: ", here("output")
))

# Save Workspace ---------------------------------------------------------------
save.image(here("output", "epds_analysis.RData"))