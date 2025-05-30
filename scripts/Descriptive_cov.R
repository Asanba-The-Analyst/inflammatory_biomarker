data <- long_data


data <- data %>%
  mutate(Age_group = factor(case_when(
    momage < 20 ~ "15–19",
    momage >= 20 & momage <= 29 ~ "20–29",
    momage >= 30 & momage <= 39 ~ "30–39",
    momage >= 40 ~ "40+"
  ),levels = c("15–19", "20–29", "30–39", "40+")))


data <- data %>%
  mutate(Employment = factor(case_when(
    job_scorres  %in% c(1, 2, 3, 4, 5, 6, 7, 8,88) ~ "employed",
    job_scorres == 77 ~ "unemployed",
    TRUE ~ NA_character_
  ), levels = c("employed", "unemployed")))

# Convert medlev variable into a factor
data <- data %>%
  mutate(Education = factor(case_when(
    school_scorres == 1 ~ "ATTENDED SCHOOL",
    school_scorres == 0 ~ "No",
    TRUE ~ NA_character_
  ), levels = c("ATTENDED SCHOOL", "No")))


# Convert ethnic variable into factor
data <- data %>%
  mutate(ethnic = factor(case_when(
    cethnic == 1 ~ "Akan/Bono", 
    cethnic == 2 ~ "Mo",
    cethnic %in% c(3, 4) ~ "Ewe/Ga-Adangme", 
    cethnic %in% c(13, 88) ~ "Northern Tribe",
    TRUE ~ NA_character_
  ), levels = c("Akan/Bono", "Mo", "Ewe/Ga-Adangme", "Northern Tribe")))

# Convert marital status variable into factor
data <- data %>%
  mutate(Marita_status = factor(case_when(
    marital_scorres == 1 ~ "Married",
    marital_scorres %in% c(2, 3, 4, 5) ~ "Single",
    TRUE ~ as.character(marital_scorres)
  ), levels = c("Married", "Single")))


## DEMOGRAPHICS ANALYSIS


# Function to summarize categorical variables
summarize_categorical <- function(data, var, var_label) {
  data %>%
    count(!!sym(var)) %>%
    mutate(
      percent = round(100 * n / sum(n), 1),
      Indicator = var_label,
      Category = !!sym(var),
      Measure = paste0(n, " (", percent, "%)")
    ) %>%
    select(Indicator, Category, Measure)
}

# Apply to each categorical variable
summary_list <- list(
  summarize_categorical(data, "Age_group", "Age"),
  summarize_categorical(data, "Education", "Education"),
  summarize_categorical(data, "Marita_status", "Marital Status"),
  summarize_categorical(data, "Employment", "Employment Status")
  #summarize_categorical(df, "Residency", "Location/Residency")
)

# Combine all summaries
cat_summary <- bind_rows(summary_list)

# Summarize numeric variables (e.g., parity, gravidity)
numeric_summary <-data %>%
  summarise(
    `Parity (mean ± SD)` = paste0(round(mean(Parity, na.rm = TRUE), 1), " ± ", round(sd(Parity, na.rm = TRUE), 1))
  ) %>%
  pivot_longer(everything(), names_to = "Indicator", values_to = "Measure") %>%
  mutate(Category = NA) %>%
  select(Indicator, Category, Measure)

# Combine categorical and numeric summaries
final_summary <- bind_rows(cat_summary, numeric_summary)

# View the result
print(final_summary)

# Optionally save to CSV
# write.csv(final_summary, "summary_indicators.csv", row.names = FALSE)
