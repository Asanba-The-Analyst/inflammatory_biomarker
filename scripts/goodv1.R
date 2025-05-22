#install.packages(c("tidyverse","readr","ggplot2"))
library(tidyverse)
library(readr)
library(ggplot2)
library(haven)
library(DataExplorer)
library(purrr)
library(skimr)

#loading the datasets

mnh00 <- read.csv("data/mnh00.csv",check.names = FALSE)
mnh01 <- read.csv("data/mnh01.csv",check.names = FALSE)
mnh02 <- read.csv("data/mnh02.csv",check.names = FALSE)
mnh03 <- read.csv("data/mnh03.csv",check.names = FALSE)
mnh04 <- read.csv("data/mnh04.csv",check.names = FALSE)
mnh06 <- read.csv("data/mnh06.csv",check.names = FALSE)
mnh08 <- read.csv("data/mnh08.csv",check.names = FALSE)
mnh09 <- read.csv("data/mnh09.csv",check.names = FALSE)
mnh11<- read.csv("data/mnh11.csv",check.names = FALSE)
mnh25 <- read.csv("data/mnh25.csv",check.names = FALSE)

master <- read.csv("data/eligible_IDs.csv")


# checking for datasets with visit types
check_variable_existence <- function(datasets, variable) {
  for (dataset_name in names(datasets)) {
    if (variable %in% names(datasets[[dataset_name]])) {
      cat(dataset_name, "YES\n")
    } else {
      cat(dataset_name, "NO\n")
    }
  }
}


datasets <- list(mnh00 = mnh00,mnh01 = mnh01,mnh02 = mnh02,mnh03 = mnh03,mnh04 = mnh04,mnh06 = mnh06,mnh08 = mnh08,mnh09 = mnh09, mnh11 = mnh11,  mnh25 = mnh25)

check_variable_existence(datasets,"TYPE_VISIT")# 


convert_names_to_lower <- function(dataset) {
  names(dataset) <- tolower(names(dataset))
  return(dataset)
}

# usage
mnh00 <- convert_names_to_lower(mnh00)
mnh01 <- convert_names_to_lower(mnh01)
mnh02 <- convert_names_to_lower(mnh02)
mnh03 <- convert_names_to_lower(mnh03)
mnh04 <- convert_names_to_lower(mnh04)
mnh06 <- convert_names_to_lower(mnh06)
mnh08 <- convert_names_to_lower(mnh08)
mnh09 <- convert_names_to_lower(mnh09)
mnh11 <- convert_names_to_lower(mnh11)
mnh25 <- convert_names_to_lower(mnh25)


#Keeping only eligible participants from mnh00-03
eli_ids <- master$pregid

mnh00 <- mnh00 %>% filter(pregid %in%eli_ids)
mnh01 <- mnh01 %>% filter(pregid %in%eli_ids)

mnh02 <- mnh02 %>% filter(pregid %in%eli_ids)

mnh08 <- mnh08 %>% filter(pregid %in%eli_ids)

mnh00_var <- c("momid","pregid","estimated_age", "brthdat","school_scorres")


mnh00 <- mnh00[,mnh00_var]
mnh00 <- mnh00 %>% rename(momage=estimated_age)

mnh01_var <- c("momid","pregid","type_visit","us_ohostdat","estimated_edd_scdat","us_ga_days_age_fts1","us_ga_wks_age_fts1")
mnh01 <- mnh01[,mnh01_var]


mnh02 <-mnh02[,c("momid","pregid","scrn_obsstdat")] 


mnh03_variables <- c(
  "momid", "pregid", "mat_vital_mnh03", 
  "marital_scorres", "house_occ_tot_fcorres", "ext_wall_fcorres", 
  "floor_fcorres", "roof_fcorres", "electricity_fcorres", 
  "solar_fcorres", "internet_fcorres", "landline_fcorres", 
  "mobile_fcorres", "mobile_access_fcorres", "radio_fcorres", 
  "tv_fcorres", "fridge_fcorres", "computer_fcorres", 
  "watch_fcorres", "bike_fcorres", "motorcycle_fcorres", 
  "car_fcorres", "boat_fcorres", "cart_fcorres", 
  "plough_fcorres", "foam_matt_fcorres", "straw_matt_fcorres", 
  "spring_matt_fcorres", "sofa_fcorres", "lantern_fcorres", 
  "sew_fcorres", "wash_fcorres", "blender_fcorres", 
  "mosquito_net_fcorres", "mosquito_net_num_fcorres", "tricycles_fcorres", 
  "tricycles_num_fcorres", "tables_fcorres", "tables_num_fcorres", 
  "cabinets_fcorres", "cabinets_num_fcorres", "sat_dish_fcorres", 
  "sat_dish_num_fcorres", "dvd_cd_fcorres", "dvd_cd_num_fcorres", 
  "aircon_fcorres", "aircon_num_fcorres", "tractor_fcorres", 
  "tractor_num_fcorres", "own_rent_scorres", "own_rent_spfy_scorres", 
  "house_rooms_fcorres", "house_room_child_fcorres", "land_fcorres", 
  "livestock_fcorres", "cattle_fcorres", "goat_fcorres", 
  "sheep_fcorres", "poultry_fcorres", "pig_fcorres", 
  "donkey_fcorres", "horse_fcorres", "animal_othr_fcorres", 
  "job_scorres", "stove_fcorres"
)


mnh03 <- mnh03[,mnh03_variables]

source("scripts/ses.R")
mnh03 <-final_df

mnh04_variables <- c(
  "pregid", "momid","type_visit",
  "prg_dsdecod", "fetal_loss_dsstdat", "fetal_loss_dsdecod", 
  "ph_prev_rporres", "ph_prevn_rporres", "ph_live_rporres",  "preterm_rporres", 
  "dthdat", "prg_dth_dsdecod")

mnh04 <- mnh04[,mnh04_variables]



mnh06_var <- c("momid","pregid","type_visit",
               "fetus_ct_peres","malaria_poc_lborres","malaria_poc_lbperf",
               "malaria_poc_lborres",
               "hiv_poc_lbperf",
               "hiv_poc_lborres",
               "syph_poc_lbperf",
               "syph_poc_lborres",
               "hbv_poc_lbperf",
               "hbv_poc_lborres",
               "hcv_poc_lbperf",
               "hcv_poc_lborres",
               "bgluc_poc_lbperf",
               "bgluc_poc_mmoll_lborres",
               "covid_poc_lbperf",
               "covid_poc_lborres")

mnh06 <- mnh06[,mnh06_var]


# Vector of variables with their associated date variables
lab_vars_with_dates <- c("momid", "pregid",
                         "type_visit",
                         # CBC Panel
                         "cbc_lbtstdat", "cbc_lbperf_1", "cbc_hb_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_2", "cbc_hct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_3", "cbc_wbc_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_4", "cbc_neu_pct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_5", "cbc_neu_fcc_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_6", "cbc_lymph_pct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_7", "cbc_lymph_fcc_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_8", "cbc_eryth_mm_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_9", "cbc_mcv_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_10", "cbc_mch_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_11", "cbc_mchc_gdl_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_12", "cbc_plate_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_13", "cbc_mono_pct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_14", "cbc_mono_fcc_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_15", "cbc_eos_pct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_16", "cbc_eos_fcc_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_17", "cbc_rdw_pct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_18", "cbc_pdw_ct_lborres",
                         "cbc_lbtstdat", "cbc_lbperf_19", "cbc_pct_pct_lborres",
                         
                         # LFT Panel
                         "lft_lbtstdat", "lft_lbperf_4", "tbilirubin_lborres",
                         "lft_lbtstdat", "lft_lbperf_5", "dbilirubin_lborres",
                         "lft_lbtstdat", "lft_lbperf_6", "tprotein_lborres",
                         "lft_lbtstdat", "lft_lbperf_7", "albumin_lborres",
                         "lft_lbtstdat", "lft_lbperf_8", "gammagt_lborres",
                         "lft_lbtstdat", "lft_lbperf_9", "ibilirubin_lborres",
                         
                         # Blood Glucose
                         "bgluc_lbtstdat", "bgluc_lbperf_1", "bgluc_pretest_mmoll_lborres",
                         "bgluc_lbtstdat", "bgluc_lbperf_2", "bgluc_oral_1hr_mmoll_lborres",
                         "bgluc_lbtstdat", "bgluc_lbperf_3", "bgluc_oral_2hr_mmoll_lborres",
                         
                         # HbA1c
                         "hba1c_lbtstdat", "hba1c_test_yn", "hba1c_lborres", "hba1c_prcnt",
                         
                         # TB
                         "tb_lbtstdat", "tb_lbperf_1", "tb_cnfrm_lborres",
                         "tb_lbtstdat", "tb_lbperf_2", "tb_backup_lborres",
                         
                         # HEV
                         "hev_lbtstdat", "hev_lbperf_1", "hev_igm_lborres",
                         "hev_lbtstdat", "hev_lbperf_2", "hev_igg_lborres",
                         
                         # RBC
                         "rbc_sickle_lbtstdat", "rbc_lbperf_1", "rbc_sickle_lborres",
                         "rbc_thala_lbtstdat", "rbc_lbperf_2", "rbc_thala_lborres",
                         "rbc_g6pd_lbtstdat", "rbc_lbperf_3",
                         
                         # Urinalysis
                         "ua_lbtstdat", "ua_dipstick",
                         "ua_lbtstdat", "ua_lbperf_1", "ua_prot_lborres",
                         "ua_lbtstdat", "ua_lbperf_2", "ua_leuk_lborres",
                         "ua_lbtstdat", "ua_lbperf_3", "ua_nitrite_lborres",
                         
                         # Malaria (assuming implicit date variable for consistency)
                         "malbl_lbperf_1", "malbl_lborres", "malbl_tk_ct_1", "malbl_tn_ct_1",
                         "placmal_lbperf_1", "placmal_lborres",
                         
                         # Inflammatory Markers (no dates assigned)
                         "crp_lborres",
                         "agp_lborres"
)


all_variables <- c("momid", "pregid",
                   "type_visit",
                   "cbc_lbtstdat",
                   "cbc_lbperf_1",
                   "cbc_hb_lborres",
                   "cbc_lbperf_2",
                   "cbc_hct_lborres",
                   "cbc_lbperf_3",
                   "cbc_wbc_lborres",
                   "cbc_lbperf_4",
                   "cbc_neu_pct_lborres",
                   "cbc_lbperf_5",
                   "cbc_neu_fcc_lborres",
                   "cbc_lbperf_6",
                   "cbc_lymph_pct_lborres",
                   "cbc_lbperf_7",
                   "cbc_lymph_fcc_lborres",
                   "cbc_lbperf_8",
                   "cbc_eryth_mm_lborres",
                   "cbc_lbperf_9",
                   "cbc_mcv_lborres",
                   "cbc_lbperf_10",
                   "cbc_mch_lborres",
                   "cbc_lbperf_11",
                   "cbc_mchc_gdl_lborres",
                   "cbc_lbperf_12",
                   "cbc_plate_lborres",
                   "cbc_lbperf_13",
                   "cbc_mono_pct_lborres",
                   "cbc_lbperf_14",
                   "cbc_mono_fcc_lborres",
                   "cbc_lbperf_15",
                   "cbc_eos_pct_lborres",
                   "cbc_lbperf_16",
                   "cbc_eos_fcc_lborres",
                   "cbc_lbperf_17",
                   "cbc_rdw_pct_lborres",
                   "cbc_lbperf_18",
                   "cbc_pdw_ct_lborres",
                   "cbc_lbperf_19",
                   "cbc_pct_pct_lborres",
                   "lft_lbperf_4",
                   "tbilirubin_lborres",
                   "lft_lbperf_5",
                   "dbilirubin_lborres",
                   "lft_lbperf_6",
                   "tprotein_lborres",
                   "lft_lbperf_7",
                   "albumin_lborres",
                   "lft_lbperf_8",
                   "gammagt_lborres",
                   "lft_lbperf_9",
                   "ibilirubin_lborres",
                   "bgluc_lbtstdat",
                   "bgluc_lbperf_1",
                   "bgluc_pretest_mmoll_lborres",
                   "bgluc_lbperf_2",
                   "bgluc_oral_1hr_mmoll_lborres",
                   "bgluc_lbperf_3",
                   "bgluc_oral_2hr_mmoll_lborres",
                   "hba1c_lbtstdat",
                   "hba1c_test_yn",
                   "hba1c_lborres",
                   "hba1c_prcnt",
                   "tb_backup_lborres",
                   "rbc_lbperf_3",
                   "rbc_g6pd_lbtstdat",
                   "ua_lbtstdat",
                   "ua_dipstick",
                   "ua_lbperf_1",
                   "ua_prot_lborres",
                   "ua_lbperf_2",
                   "ua_leuk_lborres",
                   "ua_lbperf_3",
                   "ua_nitrite_lborres",
                   "tb_lbtstdat",
                   "tb_lbperf_1",
                   "tb_cnfrm_lborres",
                   "tb_lbperf_2",
                   "hev_lbtstdat",
                   "hev_lbperf_1",
                   "hev_igm_lborres",
                   "hev_lbperf_2",
                   "hev_igg_lborres",
                   "crp_lborres",
                   "agp_lborres",
                   "malbl_lbperf_1",
                   "malbl_lborres",
                   "malbl_tk_ct_1",
                   "malbl_tn_ct_1", 
                   "placmal_lbperf_1",
                   "placmal_lborres",
                   "rbc_lbperf_1",
                   "rbc_sickle_lborres",
                   "rbc_sickle_lbtstdat",
                   "rbc_lbperf_2",
                   "rbc_thala_lborres",
                   "rbc_thala_lbtstdat",
                   "type_visit",
                   "lb_remapp3_tri"
                   
)


mnh08 <- mnh08[,lab_vars_with_dates]
#mnh08 <- mnh08[,raw]

mnh09_var <- c("momid" ,"pregid","mat_ld_oholoc","anc_tot_vists",
               "mat_vital_mnh09","malaria_mhoccur","infants_faorres",
               "infantid_inf1","deliv_dsstdat_inf1","deliv_prroute_inf1",
               "birth_dsterm_inf1","sex_inf1","mat_death_dthdat","gest_diab_mhoccur",
               "gest_diab_srce_1",
               "gest_diab_srce_2",
               "gest_diab_srce_3",
               "gest_diab_proccur_1",
               "gest_diab_proccur_2",
               "gest_diab_proccur_3",
               "gest_diab_proccur_4",
               "gest_diab_proccur_88",
               "gest_diab_proccur_77",
               "gest_diab_proccur_99")
mnh09 <- mnh09[,mnh09_var]

mnh11_variables <- c(
  "momid", "pregid", "infantid", 
  "visit_obsstdat", "inf_vital_mnh11", "inf_visit_72hr_mnh11",
  "inf_dsterm", "inf_oholoc", "sex_inf", 
  "bw_faorres_report", "bw_faorres", "bw_est_faorres", 
  "length_faorres_1", "length_faorres_2", "length_faorres_3", 
  "hc_faorres_1", "hc_faorres_2", "hc_faorres_3", 
  "dthdat"
)

mnh11 <- mnh11[,mnh11_variables]

epds_variables <- c(
  "momid", "pregid",
  "type_visit",
  # Core question variables
  "epds0101",  # Able to laugh/see funny side
  "epds0102",  # Looked forward with enjoyment
  "epds0103",  # Blamed self unnecessarily
  "epds0104",  # Anxious/worried no good reason
  "epds0105",  # Felt scared/panicky
  "epds0106",  # Things getting on top of me
  "epds0107",  # Difficulty sleeping due to unhappiness
  "epds0108",  # Felt sad/miserable
  "epds0109",  # Unhappy to the point of crying
  "epds0110",  # Thoughts of self-harm
  
  # Yes/No response variables for each item
  "epds0101_y", "epds0101_n",
  "epds0102_y", "epds0102_n",
  "epds0103_y", "epds0103_n",
  "epds0104_y", "epds0104_n",
  "epds0105_y", "epds0105_n",
  "epds0106_y", "epds0106_n",
  "epds0107_y", "epds0107_n",
  "epds0108_y", "epds0108_n",
  "epds0109_y", "epds0109_n",
  "epds0110_y", "epds0110_n",
  
  # Scoring variables
  "epds01_scorres",       # Cumulative score
  "epds01_cat_scorres",   # Depression likelihood category
  "epds0110_scorres"     # Suicidal risk score
)

mnh25<- mnh25[,epds_variables]




# Filter mnh01 (ultrasound data)
mnh01 <- mnh01%>%
  filter(type_visit %in% 1:5)


# Select datasets with type_visit field



# Assuming all mnh* datasets are already loaded individually (e.g., mnh04, mnh06, etc.)
datasets_with_type_visit <- list(
  mnh04 = mnh04,
  mnh06 = mnh06,
  mnh08 = mnh08,
  mnh25 = mnh25
)

# Filter each to only include visits 1–5
datasets_with_type_visit_filtered <- map(
  datasets_with_type_visit,
  ~ .x %>% filter(type_visit %in% 1:5)
)

# Also include mnh01 GA anchor
#datasets_with_type_visit_filtered$mnh01 <- mnh01 %>% filter(type_visit %in% 1:5)


# Filter type_visit 1 to 5 in all
datasets_with_type_visit_filtered <- map(datasets_with_type_visit, ~ .x %>%
                                           filter(type_visit %in% 1:5))

# Include mnh01 in this filtered list
datasets_with_type_visit_filtered[["mnh01"]] <- mnh01


# Merge filtered datasets
trimester_data <- reduce(datasets_with_type_visit_filtered, full_join, by = c("pregid", "momid", "type_visit"))


# Merge screening data (applies to all visits of the same pregnancy)
trimester_data <- left_join(trimester_data, mnh02, by = c("pregid", "momid"))


# Calculate GA in total days
trimester_data <- trimester_data %>%
  mutate(gest_age_days = us_ga_wks_age_fts1 * 7 + us_ga_days_age_fts1)


trimester_data <- trimester_data %>%
  mutate(visit_label = case_when(
    type_visit == 1 ~ "Enrollment ANC",
    type_visit == 2 ~ "ANC-20",
    type_visit == 3 ~ "ANC-28",
    type_visit == 4 ~ "ANC-32",
    type_visit == 5 ~ "ANC-36"
  ))

