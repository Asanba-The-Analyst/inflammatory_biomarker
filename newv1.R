# library(dplyr)
# library(tidyr)

library(lubridate)
library(tidyverse)



mnh00_unid <- mnh00 %>%
  distinct(pregid, .keep_all = TRUE)


mnh01_unid <- mnh01 %>%
  distinct(pregid, .keep_all = TRUE)

mnh08_unid <- mnh08 %>%
  distinct(pregid, type_visit,  .keep_all = TRUE)



dat <- master %>%left_join(mnh00_unid,by=c("momid","pregid"))

mnh0_1 <- dat %>%left_join(mnh01_unid,by=c("momid","pregid"))

data<- mnh0_1  %>%left_join(mnh08,by=c("momid","pregid","type_visit"))


df <- data

df <- df %>%
  mutate(
    us_ohostdat = as.Date(us_ohostdat),
    across(ends_with("_lbtstdat"), ~ as.Date(.x), .names = "{col}")  # Keeps original names
  )

# === Step 1: Calculate baseline GA in days ===
df <- df %>%
  mutate(
    # Safe conversion to numeric for GA weeks and days (in case of factors or characters)
    us_ga_wks_age_fts1 = as.numeric(us_ga_wks_age_fts1),
    us_ga_days_age_fts1 = as.numeric(us_ga_days_age_fts1),
    ga_days_base = case_when(
      !is.na(us_ga_wks_age_fts1) & !is.na(us_ga_days_age_fts1) ~ (us_ga_wks_age_fts1 * 7) + us_ga_days_age_fts1,
      !is.na(us_ga_wks_age_fts1) & is.na(us_ga_days_age_fts1) ~ us_ga_wks_age_fts1 * 7,
      TRUE ~ NA_real_
    )
  )

# Check that baseline ultrasound date (us_ohostdat) and lab test dates are proper Date objects
df <- df %>%
  mutate(
    us_ohostdat = as.Date(us_ohostdat),
    # Add date conversions for lab panels below if they exist in df
    across(
      ends_with("_lbtstdat"),
      ~ as.Date(.x),
      .names = "date_{col}"
    )
  )

# Helper function to process each lab panel
process_lab_panel <- function(data, date_var, result_vars, panel_name) {
  # Check if date_var exists and is date type
  if (!date_var %in% colnames(data)) {
    message(glue::glue("Date variable '{date_var}' not found for panel '{panel_name}'. Skipping."))
    return(tibble())
  }
  if (all(is.na(data[[date_var]]))) {
    message(glue::glue("All dates missing for '{date_var}' in panel '{panel_name}'. Skipping."))
    return(tibble())
  }
  # Filter visits 1 to 5 and non-missing test dates
  df_panel <- data %>%
    filter(type_visit %in% 1:6) %>%
    filter(!is.na(.data[[date_var]])) %>%
    mutate(
      # Calculate GA in days at test date (only if ga_days_base and us_ohostdat exist)
      ga_days_at_test = if_else(
        !is.na(ga_days_base) & !is.na(us_ohostdat),
        ga_days_base + as.numeric(.data[[date_var]] - us_ohostdat),
        NA_real_
      ),
      ga_weeks_at_test = ga_days_at_test / 7,
      trimester = case_when(
        !is.na(ga_weeks_at_test) & ga_weeks_at_test <= 13 ~ "1st Trimester",
        !is.na(ga_weeks_at_test) & ga_weeks_at_test <= 27 ~ "2nd Trimester",
        !is.na(ga_weeks_at_test) & ga_weeks_at_test > 27 ~ "3rd Trimester",
        TRUE ~ NA_character_
      )
    )
  
  # Keep only existing result_vars columns (avoid errors)
  valid_result_vars <- result_vars[result_vars %in% colnames(df_panel)]
  if(length(valid_result_vars) == 0) {
    message(glue::glue("No valid result variables found for panel '{panel_name}'. Skipping."))
    return(tibble())
  }
  
  # Select relevant columns
  select_vars <- c("momid", "type_visit", "trimester", date_var, valid_result_vars)
  df_panel <- df_panel %>% select(any_of(select_vars))
  
  # Pivot longer to one row per test
  df_long <- df_panel %>%
    pivot_longer(
      cols = all_of(valid_result_vars),
      names_to = "test_name",
      values_to = "test_result"
    ) %>%
    mutate(panel = panel_name)
  
  return(df_long)
}

# Define lab panels (only keep vars that exist in df to avoid errors)
safe_vars <- function(vars) vars[vars %in% colnames(df)]

cbc_result_vars <- safe_vars(c(
  "cbc_lbperf_1", "cbc_hb_lborres", "cbc_lbperf_2", "cbc_hct_lborres", "cbc_lbperf_3", "cbc_wbc_lborres",
  "cbc_lbperf_4", "cbc_neu_pct_lborres", "cbc_lbperf_5", "cbc_neu_fcc_lborres", "cbc_lbperf_6", "cbc_lymph_pct_lborres",
  "cbc_lbperf_7", "cbc_lymph_fcc_lborres", "cbc_lbperf_8", "cbc_eryth_mm_lborres", "cbc_lbperf_9", "cbc_mcv_lborres",
  "cbc_lbperf_10", "cbc_mch_lborres", "cbc_lbperf_11", "cbc_mchc_gdl_lborres", "cbc_lbperf_12", "cbc_plate_lborres",
  "cbc_lbperf_13", "cbc_mono_pct_lborres", "cbc_lbperf_14", "cbc_mono_fcc_lborres", "cbc_lbperf_15", "cbc_eos_pct_lborres",
  "cbc_lbperf_16", "cbc_eos_fcc_lborres", "cbc_lbperf_17", "cbc_rdw_pct_lborres", "cbc_lbperf_18", "cbc_pdw_ct_lborres",
  "cbc_lbperf_19", "cbc_pct_pct_lborres"
))

lft_result_vars <- safe_vars(c(
  "lft_lbperf_4", "tbilirubin_lborres", "lft_lbperf_5", "dbilirubin_lborres", "lft_lbperf_6", "tprotein_lborres",
  "lft_lbperf_7", "albumin_lborres", "lft_lbperf_8", "gammagt_lborres", "lft_lbperf_9", "ibilirubin_lborres"
))

bgluc_result_vars <- safe_vars(c(
  "bgluc_lbperf_1", "bgluc_pretest_mmoll_lborres", "bgluc_lbperf_2", "bgluc_oral_1hr_mmoll_lborres", "bgluc_lbperf_3",
  "bgluc_oral_2hr_mmoll_lborres"
))

hba1c_result_vars <- safe_vars(c("hba1c_test_yn", "hba1c_lborres", "hba1c_prcnt"))

tb_result_vars <- safe_vars(c("tb_lbperf_1", "tb_cnfrm_lborres", "tb_lbperf_2", "tb_backup_lborres"))

hev_result_vars <- safe_vars(c("hev_lbperf_1", "hev_igm_lborres", "hev_lbperf_2", "hev_igg_lborres"))

rbc_result_vars <- safe_vars(c("rbc_lbperf_1", "rbc_sickle_lborres", "rbc_lbperf_2", "rbc_thala_lborres", "rbc_lbperf_3"))

ua_result_vars <- safe_vars(c("ua_dipstick", "ua_prot_lborres",  "ua_leuk_lborres",  "ua_nitrite_lborres"))

malaria_result_vars <- safe_vars(c("malbl_lbperf_1", "malbl_lborres", "malbl_tk_ct_1", "malbl_tn_ct_1", "placmal_lbperf_1", "placmal_lborres"))

inflammatory_vars <- safe_vars(c("crp_lborres", "agp_lborres"))

# Process panels safely (only if date variables exist)
cbc_long <- process_lab_panel(df, "cbc_lbtstdat", cbc_result_vars, "CBC")
lft_long <- process_lab_panel(df, "lft_lbtstdat", lft_result_vars, "LFT")
bgluc_long <- process_lab_panel(df, "bgluc_lbtstdat", bgluc_result_vars, "Blood Glucose")
tb_long <- process_lab_panel(df, "tb_lbtstdat", tb_result_vars, "TB")
hev_long <- process_lab_panel(df, "hev_lbtstdat", hev_result_vars, "HEV")
rbc_long <- process_lab_panel(df, "rbc_sickle_lbtstdat", rbc_result_vars, "RBC")


wider <- rbc_long %>% pivot_wider(names_from = "trimester",values_from = .)
ua_long <- process_lab_panel(df, "ua_lbtstdat", ua_result_vars, "Urinalysis")

# HbA1c panel - check if date variable exists, else skip
if("hba1c_lbtstdat" %in% colnames(df)) {
  hba1c_long <- process_lab_panel(df, "hba1c_lbtstdat", hba1c_result_vars, "HbA1c")
} else {
  hba1c_long <- tibble()
}

# Malaria panel (no date) - just filter visits and reshape
malaria_long <- df %>%
  filter(type_visit %in% 1:5) %>%
  select(momid, type_visit, any_of(malaria_result_vars)) %>%
  mutate(panel = "Malaria") %>%
  pivot_longer(cols = all_of(malaria_result_vars), names_to = "test_name", values_to = "test_result")

# Inflammatory markers (no dates)
inflammatory_long <- df %>%
  filter(type_visit %in% 1:5) %>%
  select(momid, type_visit, any_of(inflammatory_vars)) %>%
  mutate(panel = "Inflammatory") %>%
  pivot_longer(cols = all_of(inflammatory_vars), names_to = "test_name", values_to = "test_result")

# Combine all long dataframes safely
all_labs_long <- bind_rows(
  cbc_long, lft_long, bgluc_long, tb_long, hev_long, rbc_long,
  hba1c_long, malaria_long, inflammatory_long
) %>%
  mutate(trimester = if_else(is.na(trimester), NA_character_, trimester))

# Optional: Example plot for WBC by trimester
if ("cbc_wbc_lborres" %in% all_labs_long$test_name) {
  all_labs_long %>%
    filter(test_name == "cbc_wbc_lborres" & !is.na(test_result)) %>%
    mutate(test_result = as.numeric(test_result)) %>%
    filter(!is.na(test_result)) %>%
    ggplot(aes(x = trimester, y = test_result)) +
    geom_boxplot() +
    labs(
      title = "White Blood Cell Count by Trimester",
      x = "Trimester",
      y = "WBC Count"
    )
}


go <- rbc_long %>% select(momid,trimester,test_name,test_result)
go_w <- go %>% pivot_wider(id_cols = momid,names_from = trimester,values_from = test_result)


