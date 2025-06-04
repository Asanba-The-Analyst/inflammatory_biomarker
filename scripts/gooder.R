

source("scripts/selectorbest.R")
library(gt)
library(summarytools)
library(gtsummary)

# Visits 1 to 5 = Enrollment ANC to ANC-36
trimester_visits <- c(1, 2, 3, 4, 5)

# Filter and merge datasets with type_visit 1-5
datasets_with_tv <- list(mnh01, mnh04, mnh06, mnh08, mnh25) %>%
  map(~ filter(.x, type_visit %in% trimester_visits))

# Merge all visit-based datasets
trimester_long <- reduce(datasets_with_tv, full_join, by = c("pregid", "momid", "type_visit"))

# Add static baseline info (mnh00, mnh02, mnh03)
baseline <- reduce(list(mnh00, mnh02, mnh03), full_join, by = c("pregid", "momid"))

# Combine final longitudinal dataset
long_data <- left_join(trimester_long, baseline, by = c("pregid", "momid"))


## STEP : Create Binary Infection Status Variables

long_data <- long_data %>%
  mutate(
    NLR = cbc_neu_fcc_lborres / cbc_lymph_fcc_lborres,
    PLR = cbc_plate_lborres / cbc_lymph_fcc_lborres,
    LMR = cbc_lymph_fcc_lborres / cbc_mono_fcc_lborres,
    SII = (cbc_plate_lborres * cbc_neu_fcc_lborres) / cbc_lymph_fcc_lborres,
    SIRI = (cbc_mono_fcc_lborres * cbc_neu_fcc_lborres) / cbc_lymph_fcc_lborres
  )

## STEP : Add Outcome Variables (From mnh09, mnh11, mnh25)

# After long_data creation, join outcome datasets by pregid
long_data <- long_data %>%
  left_join(mnh09, by = c("pregid", "momid")) %>%
  left_join(mnh11, by = c("pregid", "momid"))


## STEP : Create Binary Infection Status Variables


long_data <- long_data %>%
  mutate(
    malaria_positive = ifelse(malbl_lborres == "Positive", 1, 0),
    placental_malaria = ifelse(!is.na(placmal_lborres), 1, 0),
    uti_positive = ifelse(ua_prot_lborres == "Positive" | ua_leuk_lborres == "Positive" | ua_nitrite_lborres == "Positive", 1, 0),
    tb_positive = ifelse(tb_cnfrm_lborres == "Positive", 1, 0),
    crp_high = ifelse(crp_lborres > quantile(crp_lborres, 0.75, na.rm = TRUE), 1, 0),
    agp_high = ifelse(agp_lborres > quantile(agp_lborres, 0.75, na.rm = TRUE), 1, 0)
  )



# Calculate GA in total days
long_data <- long_data %>%
  mutate(gest_age_days = us_ga_wks_age_fts1 * 7 + us_ga_days_age_fts1)




# === STEP : Automated Trimester Label Assignment Based on Any Test Date ===
calculate_trimester <- function(test_date, us_ohostdat, gest_age_days) {
  ga_days <- gest_age_days + as.numeric(as.Date(test_date) - as.Date(us_ohostdat))
  ga_weeks <- ga_days / 7
  case_when(
    is.na(ga_days) ~ NA_character_,
    ga_weeks < 14 ~ "Trimester 1",
    ga_weeks >= 14 & ga_weeks < 28 ~ "Trimester 2",
    ga_weeks >= 28 ~ "Trimester 3",
    TRUE ~ NA_character_
  )
}

# Identify all *_lbtstdat variables (test dates)
date_vars <- names(long_data)[grepl("_lbtstdat$", names(long_data))]

# Standardize all test date variables to Date class
long_data <- long_data %>% mutate(across(all_of(date_vars), ~ suppressWarnings(as.Date(.))))

# Apply trimester assignment dynamically for each test date
long_data <- long_data %>% mutate(trimester = NA_character_)

for (date_var in date_vars) {
  long_data <- long_data %>%
    mutate(trimester_tmp = calculate_trimester(.data[[date_var]], us_ohostdat, gest_age_days)) %>%
    mutate(trimester = coalesce(trimester, trimester_tmp)) %>%
    select(-trimester_tmp)
}

# Add type_visit-based trimester label (static)
long_data <- long_data %>%
  mutate(
    trimester_label = case_when(
      type_visit == 1 ~ "Enrollment",
      type_visit == 2 ~ "ANC-20",
      type_visit == 3 ~ "ANC-28",
      type_visit == 4 ~ "ANC-32",
      type_visit == 5 ~ "ANC-36",
      TRUE ~ NA_character_
    )
  )



write.csv("data/long_data.csv")


# Define invalid/missing values
invalid <- c(-7, -6, -5, -9)

# Thick film parasitemia (WBC-based)
long_data$falciparum_thick_parasitemia_1 <- ifelse(
  long_data$malbl_stge_1 == 1 &
    !(long_data$malbl_tk_ct_1 %in% invalid) &
    !(long_data$malbl_tk_wbc_1 %in% invalid) &
    long_data$malbl_tk_wbc_1 != 0,
  (long_data$malbl_tk_ct_1 * 8000) / long_data$malbl_tk_wbc_1,
  NA
)

# Thin film parasitemia (RBC-based)
long_data$falciparum_thin_parasitemia_1 <- ifelse(
  long_data$malbl_stge_1 == 1 &
    !(long_data$malbl_tn_ct_1 %in% invalid) &
    !(long_data$malbl_tn_rbc_1 %in% invalid) &
    long_data$malbl_tn_rbc_1 != 0,
  (long_data$malbl_tn_ct_1 * 5000000) / long_data$malbl_tn_rbc_1,
  NA
)




long_data$malbl_lbperf_1

table(long_data$malbl_stge_1)

demo <- long_data %>%distinct(momid,.keep_all = TRUE) 

#write.csv(long_data,"data/long_data.csv")
# 
# fifi <- long_data %>% filter(type_visit==1)
# nrow(fifi %>% filter(us_ga_wks_age_fts1>=28))
# sam<-long_data %>% select(momid,type_visit,trimester_label,trimester)
# table(sam$trimester)
# nrow(sam %>% filter())
dist <- mnh08 %>% distinct(pregid,.keep_all = TRUE)

dist2 <- dist %>% filter(malbl_lbperf_1==1)
dist3 <- long_data %>% filter(malbl_lbperf_1==1)
table(mnh08$type_visit,mnh08$malbl_lbperf_1)




##how to tabulate for any infection in the longformat

## 1. filter the long data to only those that were performed like the below for malaria


dist3 <- long_data %>% filter(malbl_lbperf_1==1)
nrow(dist3 %>% distinct(pregid,.keep_all = TRUE))
## 2. table each by the 
table(dist3$trimester_label,dist3$malbl_lborres)




cds<- long_data %>% filter(zcd_lbperf_1==1)
table(zcd$zcd_zikigm_lborres)






library(dplyr)
library(tidyr)
library(gt)



# 1. Create performance summary
performance_report <- mnh08 %>%
  summarise(across(c(zcd_lbperf_1, zcd_lbperf_2,
                     zcd_lbperf_3, zcd_lbperf_4,
                     zcd_lbperf_5, zcd_lbperf_6),
                   list(
                     tested = ~sum(. == 1, na.rm = TRUE),
                     not_tested = ~sum(. == 0, na.rm = TRUE),
                     pct_tested = ~sum(. == 1, na.rm = TRUE) / 
                       sum(. %in% c(1, 2), na.rm = TRUE) 
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
    ))
  

# 2. Create results summary (only for tests that were performed)
results_report <- mnh08 %>%
  select(zcd_zikigm_lborres, zcd_zikigg_lborres,
         zcd_denigm_lborres, zcd_denigg_lborres,
         zcd_chkigm_lborres, zcd_chkigg_lborres) %>%
  map_df(~{
    tibble(
      negative = sum(. == 0, na.rm = TRUE),
      positive = sum(. == 1, na.rm = TRUE),
      inconclusive = sum(. == 2, na.rm = TRUE),
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

























# Convert test performance variables to factors
perf_vars <- paste0("zcd_lbperf_", 1:6)
df <- long_data %>%
  mutate(across(all_of(perf_vars), 
                ~ factor(case_when(
                  . == 1 ~ "Performed",
                  . == 0 ~ "Not performed",
                  TRUE ~ "Unknown"
                ),
                levels = c("Performed", "Not performed"))
  ))
# Convert test result variables to factors
result_vars <- c("zcd_zikigm_lborres", "zcd_zikigg_lborres",
                   "zcd_denigm_lborres", "zcd_denigg_lborres",
                   "zcd_chkigm_lborres", "zcd_chkigg_lborres")
  
df <- df %>%
    mutate(across(all_of(result_vars), 
                  ~ factor(case_when(
                    . == 0 ~ "Negative",
                    . == 1 ~ "Positive",
                    . == 2 ~ "Inconclusive",
                    TRUE ~ "Unknown"
                  ),
                  levels = c("Negative", "Positive", "Inconclusive"))
    ))


# Create a list to store individual tables
test_tables <- list()

# Define test mappings
test_mapping <- list(
  "1" = list(name = "Zika IgM", perf_var = "zcd_lbperf_1", res_var = "zcd_zikigm_lborres"),
  "2" = list(name = "Zika IgG", perf_var = "zcd_lbperf_2", res_var = "zcd_zikigg_lborres"),
  "3" = list(name = "Dengue IgM", perf_var = "zcd_lbperf_3", res_var = "zcd_denigm_lborres"),
  "4" = list(name = "Dengue IgG", perf_var = "zcd_lbperf_4", res_var = "zcd_denigg_lborres"),
  "5" = list(name = "Chikungunya IgM", perf_var = "zcd_lbperf_5", res_var = "zcd_chkigm_lborres"),
  "6" = list(name = "Chikungunya IgG", perf_var = "zcd_lbperf_6", res_var = "zcd_chkigg_lborres")
)

# Generate tables for each test
for (test in names(test_mapping)) {
  test_info <- test_mapping[[test]]
  
  test_data <- df %>%
    filter(.data[[test_info$perf_var]] == "Performed") %>%  # Only performed tests
    count(.data[[test_info$res_var]]) %>%             # Count results
    mutate(percent = n/sum(n)) %>%                # Calculate percentages
    rename(Result = 1, Count = n, Percent = percent)
  
  test_tables[[test_info$name]] <- test_data %>%
    gt() %>%
    tab_header(
      title = test_info$name,
      subtitle = paste("Total performed:", sum(test_data$Count))
    ) %>%
    fmt_number(columns = c(Count), decimals = 0) %>%
    fmt_percent(columns = c(Percent), decimals = 1) %>%
    tab_options(table.width = "400px")
}

# Display all tables
test_tables




#Those with NA in chikungunya igg

da <- mnh08 %>%filter(zcd_lbperf_6==1)
mis<-da %>% filter(zcd_chkigg_lborres==5)

perf <- c("malaria_poc_lbperf", "placmal_lbperf_1","malbl_lbperf_1",
          "hiv_poc_lbperf", "syph_poc_lbperf",
"hbv_poc_lbperf", "hcv_poc_lbperf", "covid_poc_lbperf",
paste0("ua_lbperf_", 1:3),paste0("rbc_lbperf_", 1:2))



clean_data <- long_data %>%
  mutate(across(all_of(perf), 
                ~ factor(case_when(
                  . == 1 ~ "Performed",
                  . == 0 ~ "Not performed",
                  TRUE ~ "Unknown"
                ),
                levels = c("Performed", "Not performed"))
  ))


result <-  c("malbl_lborres", "placmal_lborres",
             "hb_poc_lborres", "malaria_poc_lborres", "hiv_poc_lborres",
             "syph_poc_lborres", "hbv_poc_lborres", "hcv_poc_lborres",
             "covid_poc_lborres")

encode_result <- function(x) {
  factor(case_when(
    x == 0 ~ "negative",
    x == 1 ~ "positive",
    TRUE ~ "unknown"
  ), levels = c("negative", "positive"))}

clean_data <- clean_data %>%
  mutate(across(any_of(result), encode_result))

# 5. Handle special cases with custom encoding
clean_data <- clean_data %>%
  mutate(
    # Urinalysis (ordinal)
    ua_prot_lborres = factor(ua_prot_lborres,
                             levels = 0:5,
                             labels = c("none", "trace", "1+", "2+", "3+", "4+"),
                             ordered = TRUE),
    
    ua_leuk_lborres = factor(ua_leuk_lborres,
                             levels = 0:4,
                             labels = c("none", "trace", "1+", "2+", "3+"),
                             ordered = TRUE),
    
    ua_nitrite_lborres = factor(ua_nitrite_lborres,
                                levels = 0:1,
                                labels = c("negative", "positive")),
    
    # RBC disorders
    rbc_sickle_lborres = factor(rbc_sickle_lborres,
                                levels = 0:1,
                                labels = c("normal", "sickle cell present")),
    
    rbc_thala_lborres = factor(rbc_thala_lborres,
                               levels = 0:1,
                               labels = c("normal", "hemoglobinopathy present")),
    
    # Placental malaria
    placmal_lborres = factor(case_when(
      placmal_lborres == 0 ~ "not infected",
      placmal_lborres == 1 ~ "acute infection",
      placmal_lborres == 2 ~ "chronic infection",
      placmal_lborres == 3 ~ "past infection",
      TRUE ~ "unknown"
    ), levels = c("not infected", "acute infection", "chronic infection", 
                  "past infection", "unknown"),
    ordered = TRUE)
  )

# Create a list to store individual tables
test_tables <- list()

# Define test mappings for all categorical tests
test_mapping <- list(
  # Malaria POC
  "malaria_poc" = list(name = "Malaria POC", perf_var = "malaria_poc_lbperf", res_var = "malaria_poc_lborres"),
  
  # HIV POC
  "hiv_poc" = list(name = "HIV POC", perf_var = "hiv_poc_lbperf", res_var = "hiv_poc_lborres"),
  
  # Syphilis POC
  "syph_poc" = list(name = "Syphilis POC", perf_var = "syph_poc_lbperf", res_var = "syph_poc_lborres"),
  
  # HBV POC
  "hbv_poc" = list(name = "Hepatitis B POC", perf_var = "hbv_poc_lbperf", res_var = "hbv_poc_lborres"),
  
  # HCV POC
  "hcv_poc" = list(name = "Hepatitis C POC", perf_var = "hcv_poc_lbperf", res_var = "hcv_poc_lborres"),
  
  # COVID POC
  "covid_poc" = list(name = "COVID POC", perf_var = "covid_poc_lbperf", res_var = "covid_poc_lborres"),
  
  # Malaria Blood Slide
  "malbl" = list(name = "Malaria Blood Slide", perf_var = "malbl_lbperf_1", res_var = "malbl_lborres"),
  
  # Placental Malaria
  "placmal" = list(name = "Placental Malaria", perf_var = "placmal_lbperf_1", res_var = "placmal_lborres"),
  
  # RBC Disorders
  "sickle" = list(name = "Sickle Cell", perf_var = "rbc_lbperf_1", res_var = "rbc_sickle_lborres"),
  "thalassemia" = list(name = "Thalassemia", perf_var = "rbc_lbperf_2", res_var = "rbc_thala_lborres"),
  
  # Urinalysis
  "ua_prot" = list(name = "Urine Protein", perf_var = "ua_lbperf_1", res_var = "ua_prot_lborres"),
  "ua_leuk" = list(name = "Urine Leukocytes", perf_var = "ua_lbperf_2", res_var = "ua_leuk_lborres"),
  "ua_nit" = list(name = "Urine Nitrites", perf_var = "ua_lbperf_3", res_var = "ua_nitrite_lborres")
)

# Generate tables for each test
for (test in names(test_mapping)) {
  test_info <- test_mapping[[test]]
  
  # Skip if variables don't exist in data
  if(!all(c(test_info$perf_var, test_info$res_var) %in% names(clean_data))) next
  
  test_data <- clean_data %>%
    filter(.data[[test_info$perf_var]] == "Performed") %>% # Only performed tests
    count(.data[[test_info$res_var]]) %>%                # Count results
    mutate(percent = n/sum(n)) %>%                   # Calculate percentages
    rename(Result = 1, Count = n, Percent = percent)
  
  test_tables[[test_info$name]] <- test_data %>%
    gt() %>%
    tab_header(
      title = test_info$name,
      subtitle = paste("Total performed:", sum(test_data$Count))
    ) %>%
    fmt_number(columns = c(Count), decimals = 0) %>%
    fmt_percent(columns = c(Percent), decimals = 1) %>%
    tab_options(table.width = "400px")
}

# Display all tables
test_table







