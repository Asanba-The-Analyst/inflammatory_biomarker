
#install.packages(c("tidyverse","readr","ggplot2"))
library(tidyverse)
library(readr)
library(ggplot2)
library(haven)
library(DataExplorer)
library(skimr)

#loading the datasets

mnh00 <- read.csv("data/mnh00.csv",check.names = FALSE)

mnh01 <- read.csv("data/mnh01.csv",check.names = FALSE)
mnh02 <- read.csv("data/mnh02.csv",check.names = FALSE)
mnh03 <- read.csv("data/mnh03.csv",check.names = FALSE)
mnh06 <- read.csv("data/mnh06.csv",check.names = FALSE)
mnh08 <- read.csv("data/mnh08.csv",check.names = FALSE)
mnh09 <- read.csv("data/mnh09.csv",check.names = FALSE)
mnh11<- read.csv("data/mnh11.csv",check.names = FALSE)
# mnh09 <- read.csv("data/mnh09.csv",check.names = FALSE)
# mnh11 <- read.csv("data/mnh11.csv",check.names = FALSE)
# mnh12 <- read.csv("data/mnh12.csv",check.names = FALSE)
# mnh13 <- read.csv("data/mnh13.csv",check.names = FALSE)
# mnh14 <- read.csv("data/mnh14.csv",check.names = FALSE)
# mnh15 <- read.csv("data/mnh15.csv",check.names = FALSE)
# mnh19 <- read.csv("data/mnh19.csv",check.names = FALSE)
# mnh20 <- read.csv("data/mnh20.csv",check.names = FALSE)
# mnh23 <- read.csv("data/mnh23.csv",check.names = FALSE)
# mnh24 <- read.csv("data/mnh24.csv",check.names = FALSE)
mnh25 <- read.csv("data/mnh25.csv",check.names = FALSE)
#master <- read_dta("mnh00_02_ids.dta")

data=list(mnh01, mnh02,mnh03,mnh06,mnh08,mnh09,mnh11,mnh25)


check_variable_existence <- function(datasets, variable) {
  for (dataset_name in names(datasets)) {
    if (variable %in% names(datasets[[dataset_name]])) {
      cat(dataset_name, "YES\n")
    } else {
      cat(dataset_name, "NO\n")
    }
  }
}


datasets <- list(mnh03 = mnh03,mnh06 = mnh06,mnh08 = mnh08,mnh09 = mnh09, mnh11 = mnh11,  mnh25 = mnh25)

check_variable_existence(datasets,"TYPE_VISIT")# 


check_duplicate <- function(data){
  for (i in 1:length(data)){
    duplicate <- names(data[[i]])[duplicated(names(data[[i]]))]
    print(duplicate)
    
  }
}






#checking for duplicates in the columns
check_duplicate(data)


convert_names_to_lower <- function(dataset) {
  names(dataset) <- tolower(names(dataset))
  return(dataset)
}

# Example usage
mnh03 <- convert_names_to_lower(mnh03)
mnh04 <- convert_names_to_lower(mnh04)
mnh05 <- convert_names_to_lower(mnh05)
mnh06 <- convert_names_to_lower(mnh06)
mnh08 <- convert_names_to_lower(mnh08)
mnh09 <- convert_names_to_lower(mnh09)

mnh11 <- convert_names_to_lower(mnh11)
mnh12 <- convert_names_to_lower(mnh12)
mnh13 <- convert_names_to_lower(mnh13)
mnh14 <- convert_names_to_lower(mnh14)

mnh15 <- convert_names_to_lower(mnh15)
mnh19 <- convert_names_to_lower(mnh19)
mnh20 <- convert_names_to_lower(mnh20)
mnh23 <- convert_names_to_lower(mnh23)
mnh24 <- convert_names_to_lower(mnh24)


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

mnh04_variables <- c(
  "pregid", "momid","type_visit", "mat_vital_mnh04",
  "prg_dsdecod", "fetal_loss_dsstdat", "fetal_loss_dsdecod", 
  "ph_prev_rporres", "ph_prevn_rporres", "ph_live_rporres", 
  "ph_oth_rporres", "diabetes_ever_mhoccur", "preterm_rporres", 
  "malaria_ever_mhoccur", "dthdat", "prg_dth_dsdecod")

mnh04 <- mnh04[,mnh04_variables]


mnh05_var <- c("momid","pregid","type_visit","height_peres","weight_peres","muac_peres" )

mnh05 <- mnh05[,mnh05_var]

mnh06_var <- c("momid","pregid","type_visit","mat_vital_mnh06","singleton_peres",
               "fetus_ct_peres","malaria_poc_lborres")

mnh06 <- mnh06[,mnh06_var]

mnh08_var <- c("momid","pregid","type_visit","mat_vital_mnh08","malbl_lborres" ,
               "placmal_lbtstdat", "placmal_lbperf_1",
               "placmal_lborres" )
mnh08 <- mnh08[,mnh08_var]

mnh09_var <- c("momid" ,"pregid","mat_ld_oholoc","anc_tot_vists",
               "mat_vital_mnh09","malaria_mhoccur","infants_faorres",
               "infantid_inf1","deliv_dsstdat_inf1","deliv_prroute_inf1",
               "birth_dsterm_inf1","sex_inf1","mat_death_dthdat")
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

mnh13_variables <- c(
  "momid", "pregid","type_visit", "inf_vital_mnh13", "weight_peres", 
  "length_peres_1", "length_peres_2", "length_peres_3", 
  "hc_peres_1", "hc_peres_2", "hc_peres_3", "muac_peres_1", 
  "muac_peres_2", "muac_peres_3", "infant_dthdat"
)
mnh13 <- mnh13[,mnh13_variables]

mnh14_variables <- c(
  "momid", "pregid","type_visit", "infantid", "inf_vital_mnh14", 
  "malaria_lborres"
)
mnh14 <- mnh14[,mnh14_variables]

mnh15_variables <- c(
  "momid", "pregid","type_visit", "infantid", "obsstdat", 
  "inf_vital_mnh15"
)

mnh15 <- mnh15[,mnh15_variables]

mnh19_variables <- c(
  "momid", "pregid", "hdp_htn_mhoccur_1", 
  "hdp_htn_mhoccur_2", "hdp_htn_mhoccur_3", "malaria_cmoccur", 
  "ultrasound_proccur", "ultrasound_prstdat", "ultrasound_faorres", 
  "malaria_lborres", "early_loss_mhterm", "early_loss_spfy_mhterm", 
  "infection_mhterm_15", "preg_dsterm", "preg_faorres", 
  "visit_faorres"
)



mnh19 <- mnh19[,mnh19_variables]


#Data merging
wider_mnh04 <- mnh04 %>% 
  pivot_wider(names_from = "type_visit",
              values_from = c("mat_vital_mnh04","prg_dsdecod", 
                              "fetal_loss_dsstdat", "fetal_loss_dsdecod",
                              "ph_prev_rporres", "ph_prevn_rporres", 
                              "ph_live_rporres", "ph_oth_rporres",
                              "diabetes_ever_mhoccur", "preterm_rporres",
                              "malaria_ever_mhoccur", "dthdat", "prg_dth_dsdecod"))


wider_mnh044<- mnh04 %>% 
  pivot_wider(names_from = "type_visit",
              values_from = c("mat_vital_mnh04","prg_dsdecod", 
                              "fetal_loss_dsdecod",
                              "preterm_rporres",
                              "malaria_ever_mhoccur"))


fetal_los <- c("fetal_loss_dsdecod_2","fetal_loss_dsdecod_3",
               "fetal_loss_dsdecod_4","fetal_loss_dsdecod_5",
               "fetal_loss_dsdecod_13" )

momvital <- c( "mat_vital_mnh04_1","mat_vital_mnh04_2","mat_vital_mnh04_3",
               "mat_vital_mnh04_4","mat_vital_mnh04_5","mat_vital_mnh04_13",
               "mat_vital_mnh04_77"  )

malr <- c("malaria_ever_mhoccur_1","malaria_ever_mhoccur_2",
          "malaria_ever_mhoccur_3","malaria_ever_mhoccur_4",
          "malaria_ever_mhoccur_5",
          "malaria_ever_mhoccur_13","malaria_ever_mhoccur_77")

preterm <- c("preterm_rporres_1","preterm_rporres_2","preterm_rporres_3",
             "preterm_rporres_4","preterm_rporres_5","preterm_rporres_13",
             "preterm_rporres_77")




# # Function to check if any of the specified columns contain a 1
# check_n_generat<- function(data,columns) {
#   data %>%
#     mutate(malaria = if_else(rowSums(select(., all_of(columns)) == 1, na.rm = TRUE) > 0, 1, 0))
# }




# Function to check if any of the specified columns contain a 1 and assign to a custom variable name
check_n_generat <- function(data, columns, new_var_name) {
  data %>%
    mutate(!!sym(new_var_name) := if_else(rowSums(select(., all_of(columns)) == 1, na.rm = TRUE) > 0, 1, 0))
}

# Example usage with a custom variable name
wider_mnh04 <- check_n_generat(wider_mnh04,malr,"malaria")

wider_mnh04 <- check_n_generat(wider_mnh04,momvital,"momstatus")

wider_mnh04 <- check_n_generat(wider_mnh04,preterm,"preterm")
wider_mnh04 <- check_n_generat(wider_mnh04,fetal_los,"fetal_los")


wider_mnh04 <- wider_mnh04[,c("momid","pregid","ph_prevn_rporres","ph_live_rporres","preg_status", "malaria","momstatus","preterm","fetal_los" )]

# widing the dataset from mnh04-mnh09

#merging  from mnh03 to mnh09 for the mother data

#merging that data together with the master dataset to drop off all ineligible 




