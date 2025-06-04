# library(dplyr)
# library(lubridate)

library(gtsummary)
library(summarytools)
# Disable footnotes (e.g., on statistics and missing data)
#st_options(footnote = FALSE)




###############################################################


# Function to standardize special values across all variables
clean_special_values <- function(x) {
  x <- case_when(
    x %in% c(-7, 77) ~ NA_real_,      # Not applicable
    x %in% c(-5, 55) ~ NA_real_,      # Missing
    x %in% c(-6, 66) ~ NA_real_,      # Refused
    x %in% c(-9, 99) ~ NA_real_,      # Don't know
    TRUE ~ as.numeric(x)
  )
  return(x)
}


clean_dates <- function(date_col) {
  # Ensure input is character
  date_col <- as.character(date_col)
  
  # Replace special string values with NA
  date_col <- case_when(
    date_col %in% c("07-07-1907", "05-05-1905", "06-06-1906", "09-09-1909") ~ NA_character_,
    TRUE ~ date_col
  )
  
  # Convert to Date format
  date_col <- dmy(date_col)
  
  return(date_col)
}



clean_yes_no <- function(x) {
  x <- case_when(
    x == 1 ~ "Yes",
    x == 0 ~ "No",
    TRUE ~ NA_character_
  )
  return(factor(x, levels = c("No", "Yes")))
}


clean_pos_neg <- function(x) {
  x <- case_when(
    x == 1 ~ "Positive",
    x == 0 ~ "Negative",
    TRUE ~ NA_character_
  )
  return(factor(x, levels = c("Negative", "Positive")))
}

clean_data <- long_data %>%
  mutate(across(where(is.numeric), clean_special_values)) %>%
  mutate(across(ends_with("_lbtstdat") | ends_with("lbstdat"), clean_dates)) %>%
  mutate(across(starts_with("cbc_lbperf") | starts_with("lft_lbperf") |
                  starts_with("renal_lbperf") | starts_with("mn_lbperf") |
                  starts_with("thyroid_lbperf") | starts_with("bgluc_lbperf") |
                  starts_with("hba1c_test_yn") | starts_with("rbc_lbperf") |
                  starts_with("ua_lbperf") | starts_with("tb_lbperf") |
                  starts_with("blead_lbperf") | starts_with("malbl_lbperf") |
                  starts_with("placmal_lbperf") | starts_with("hb_poc_lbperf") |
                  starts_with("malaria_poc_lbperf") | starts_with("hiv_poc_lbperf") |
                  starts_with("syph_poc_lbperf") | starts_with("hbv_poc_lbperf") |
                  starts_with("hcv_poc_lbperf") | starts_with("bgluc_poc_lbperf") |
                  starts_with("covid_poc_lbperf"), clean_yes_no))



# Handle ordinal urine analysis results
clean_data <- clean_data %>%
  mutate(
    ua_prot_lborres = factor(ua_prot_lborres,
                             levels = 0:5,
                             labels = c("None (negative)", "Trace", "1+ (0.3 g/L)",
                                        "2+ (1 g/L)", "3+ (3 g/L)", "4+ (>10 g/L)"),
                             ordered = TRUE),
    ua_leuk_lborres = factor(ua_leuk_lborres,
                             levels = 0:4,
                             labels = c("None (negative)", "Trace", "1+", "2+", "3+"),
                             ordered = TRUE),
    ua_nitrite_lborres = factor(ua_nitrite_lborres,
                                levels = 0:1,
                                labels = c("None (negative)", "Present (positive)"))
  )

# Handle RBC disorder results
clean_data <- clean_data %>%
  mutate(
    rbc_sickle_lborres = factor(rbc_sickle_lborres,
                                levels = 0:1,
                                labels = c("Normal, disease absent", "Sickle cell disease present")),
    rbc_thala_lborres = factor(rbc_thala_lborres,
                               levels = 0:1,
                               labels = c("Normal, disease absent", "Hemoglobinopathy or Thalassemias present"))
  )


# Apply Positive/Negative cleaner to result variables
clean_data <- clean_data %>%mutate(across(c(malaria_poc_lborres, malbl_lborres,hiv_poc_lborres, syph_poc_lborres,
                hbv_poc_lborres, hcv_poc_lborres, covid_poc_lborres), clean_pos_neg))



clean_data <- clean_data %>%
  mutate(placmal_lborres = factor(
    case_when(
      placmal_lborres == 0 ~ "Not Infected",
      placmal_lborres == 1 ~ "Acute infection",
      placmal_lborres == 2 ~ "Chronic infection",
      placmal_lborres == 3 ~ "Past infection",
      # placmal_lborres == 77 ~ "Not applicable",
      # placmal_lborres == 55 ~ "Missing",
      # placmal_lborres == 66 ~ "Refused to answer",
      # placmal_lborres == 99 ~ "Don't know",
      TRUE ~ "Unknown"
    ),
    levels = c("Not Infected", "Acute infection", "Chronic infection", "Past infection",
               "Not applicable", "Missing", "Refused to answer", "Don't know", "Unknown"),
    ordered = TRUE
  ))





# Apply encoding to all test performance variables
perf_vars <- c("zcd_lbperf_1", "zcd_lbperf_2",
               "zcd_lbperf_3", "zcd_lbperf_4",
               "zcd_lbperf_5", "zcd_lbperf_6")

clean_data <- clean_data %>%
  mutate(across(all_of(perf_vars), clean_yes_no))


encode_result <- function(x) {
  case_when(
    x == 0 ~ "Negative",
    x == 1 ~ "Positive",
    x == 2 ~ "Inconclusive",
    TRUE ~ as.character(x)
  )
  return(factor(x, levels = c("Negative", "Positive","Inconclusive")))
}


# Apply encoding to all test result variables
result_vars <- c("zcd_zikigm_lborres",
                 "zcd_zikigg_lborres",
                 "zcd_denigm_lborres",
                 "zcd_denigg_lborres",
                 "zcd_chkigm_lborres",
                 "zcd_chkigg_lborres")

clean_data <- clean_data %>%
  mutate(across(all_of(result_vars), encode_result))







# Check performance variables
clean_data %>% 
  select(all_of(perf_vars)) %>% 
  sapply(table, useNA = "always")

# Check result variables
clean_data %>% 
  select(all_of(result_vars)) %>% 
  sapply(table, useNA = "always")

#clean_data %>% select(-ends_with("_lbtstdat"), -ends_with("lbstdat"), -pregid, -momid) %>% dfSummary() %>% stview()

PB <- clean_data %>% 
    select(
      # Exclude all date variables including numbered ones
      -matches("_lbtstdat?[0-9]*$"),  # Catches LBTSTDAT, LBTSTDAT_1, etc.
      -matches("lbstdat.?[0-9]*$"),   # Catches LBSTDAT, LBSTDAT_1, etc.
      # Exclude other known date patterns if they exist
      -matches("date.?[0-9]*$"),
      -matches("dat.?[0-9]*$"),
      # Exclude specific ID variables
      -pregid, 
      -momid
    )
PB %>%dfSummary() %>% stview()
#Create a comprehensive summary
# dfSummary(clean_data,
#           plain.ascii  = FALSE,
#           style        = "grid",
#           graph.magnif = 0.8,
#           valid.col    = FALSE,
#           tmp.img.dir  = "/tmp",
#           na.col = TRUE) %>%stview()# Show NA counts

# dfSummary(clean_data %>% select(-ends_with("_lbtstdat"), -ends_with("lbstdat"), -pregid, -momid),
#           plain.ascii  = FALSE,
#           style        = "grid",
#           graph.magnif = 0.8,
#           valid.col    = FALSE,
#           tmp.img.dir  = "/tmp",
#           na.col = TRUE) %>% 
#   stview()





# 1. Create performance summary
performance_report <- clean_data %>%
  summarise(across(c(zcd_lbperf_1, zcd_lbperf_2,
                     zcd_lbperf_3, zcd_lbperf_4,
                     zcd_lbperf_5, zcd_lbperf_6),
                   list(
                     tested = ~sum(. == "yes", na.rm = TRUE),
                     not_tested = ~sum(. == "no", na.rm = TRUE),
                     pct_tested = ~mean(. == "yes", na.rm = TRUE) * 100
                   ))) %>%
  pivot_longer(everything(),
               names_to = c("test_num", ".value"),
               names_pattern = "zcd_lbperf_(\\d+)_(tested|not_tested|pct_tested)") %>%
  mutate(
    test_num = as.numeric(test_num),
    test_type = case_when(
      test_num == 1 ~ "zikigm",
      test_num == 2 ~ "zikigg", 
      test_num == 3 ~ "denigm",
      test_num == 4 ~ "denigg",
      test_num == 5 ~ "chkigm",
      test_num == 6 ~ "chkigg"
    )
  )

# 2. Create results summary (only for tests that were performed)
results_report <- clean_data %>%
  select(zcd_zikigm_lborres, zcd_zikigg_lborres,
         zcd_denigm_lborres, zcd_denigg_lborres,
         zcd_chkigm_lborres, zcd_chkigg_lborres) %>%
  map_df(~{
    tibble(
      negative = sum(. == "negative", na.rm = TRUE),
      positive = sum(. == "positive", na.rm = TRUE),
      inconclusive = sum(. == "inconclusive", na.rm = TRUE),
      total_results = negative + positive + inconclusive
    )
  }, .id = "test") %>%
  mutate(test_type = gsub("zcd_|_lborres", "", test))

# 3. Merge the reports
full_report <- performance_report %>%
  left_join(results_report, by = "test_type") %>%
  mutate(
    virus = case_when(
      grepl("zik", test_type) ~ "Zika",
      grepl("den", test_type) ~ "Dengue",
      grepl("chk", test_type) ~ "Chikungunya",
      TRUE ~ "Unknown"
    ),
    antibody = ifelse(grepl("igg", test_type), "IgG", "IgM"),
    .before = 1
  ) %>%
  select(-test_num, -test, -test_type) %>%
  arrange(virus, antibody)

# 4. Create formatted GT table
gt_report <- full_report %>%
  gt(rowname_col = "virus", groupname_col = "antibody") %>%
  tab_header(
    title = "Zika-Dengue-Chikungunya Test Performance and Results",
    subtitle = "Summary of testing outcomes (using zcd_lbperf_* variables)"
  ) %>%
  fmt_number(
    columns = c(tested, not_tested, negative, positive, inconclusive, total_results),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = pct_tested,
    decimals = 1
  ) %>%
  cols_label(
    tested = "Tested (n)",
    not_tested = "Not Tested (n)",
    pct_tested = "% Tested",
    negative = "Negative",
    positive = "Positive",
    inconclusive = "Inconclusive",
    total_results = "Total Results"
  ) %>%
  tab_spanner(
    label = "Test Performance",
    columns = c(tested, not_tested, pct_tested)
  ) %>%
  tab_spanner(
    label = "Test Results",
    columns = c(negative, positive, inconclusive, total_results)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(),
      cells_row_groups(),
      cells_column_spanners()
    )
  ) %>%
  tab_options(
    table.width = "100%",
    table.font.size = 12
  ) %>%
  tab_footnote(
    footnote = "Results only include tests that were actually performed",
    locations = cells_column_spanners(spanners = "Test Results")
  )

# Display the report
gt_report
