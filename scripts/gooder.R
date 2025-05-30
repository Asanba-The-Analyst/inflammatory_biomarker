

source("scripts/selectorbest.R")

# Visits 1 to 5 = Enrollment ANC to ANC-36
trimester_visits <- c(1, 2, 3, 4, 5)

# mnh01 <- mnh01 
# mnh04 <- mnh04 
# mnh06 <- mnh06
# mnh08 <- mnh08
# mnh25 <- mnh25
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



# Define invalid/missing values
invalid <- c(-7, -6, -5, -9, 55, 66, 77, 99, NA)

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





#write.csv(long_data,"data/long_data.csv")
# 
# fifi <- long_data %>% filter(type_visit==1)
# nrow(fifi %>% filter(us_ga_wks_age_fts1>=28))
# sam<-long_data %>% select(momid,type_visit,trimester_label,trimester)
# table(sam$trimester)
# nrow(sam %>% filter())

