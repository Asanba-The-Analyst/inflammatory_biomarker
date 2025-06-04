
#install.packages("summarytools")
library(gtsummary)
library(summarytools)
library(tidyverse)



# trial %>% select(trt,age,grade,response) %>% 
#   tbl_summary(by=trt) %>% add_p() %>% 
#   add_overall() %>% 
#   add_n() %>% 
#   bold_labels()


data <- long_data


data <- data %>%
  mutate(Age_group = factor(case_when(
    momage <  20 ~ "15–19",
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
    Marita_status == 1 ~ "Married",
    Marita_status %in% c(2, 3, 4, 5) ~ "Single",
    TRUE ~ as.character(Marita_status)
  ), levels = c("Married", "Single")))


demo-var <- c(Marita_status,ethnic,Education,Employment,momage)

## DEMOGRAPHICS ANALYSIS
demo <- data %>% distinct(pregid,.keep_all = TRUE)

table(demo$Age_group)

# Function to summarize categorical variables
summarize_categorical <- function(demo, var, var_label) {
  demo %>%
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
  summarize_categorical(demo, "Age_group", "Age"),
  summarize_categorical(demo, "Education", "Education"),
  summarize_categorical(demo, "Marita_status", "Marital Status"),
  summarize_categorical(demo, "Employment", "Employment Status")
  #summarize_categorical(df, "Residency", "Location/Residency")
)

# Combine all summaries
cat_summary <- bind_rows(summary_list)

# Summarize numeric variables (e.g., parity, gravidity)
numeric_summary <-demo %>%
  summarise(
    `Parity (mean ± SD)` = paste0(round(mean(Parity, na.rm = TRUE), 1), " ± ", round(sd(Parity, na.rm = TRUE), 1))
  ) %>%
  pivot_longer(everything(), names_to = "Indicator", values_to = "Measure") %>%
  mutate(Category = NA) %>%
  select(Indicator, Category, Measure)

# Combine categorical and numeric summaries
final_summary <- bind_rows(cat_summary, numeric_summary)

# View the result

final_summary %>% stview()



# trial %>% select(trt,age,grade,response) %>% 
  demo %>% select() %>% tbl_summary() %>% add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()
  
  
  # based on thin film (RBC)
  #Parasites/μL = (No. of parasitized red cells counted x 5 000 000)/No. of white cells counted
  
  #Parasitemia based on thick film (WBC)
  #Parasites/μL blood = (Number of parasites counted x 8000 white cells)/No. of white cells counted
# Optionally save to CSV
# write.csv(final_summary, "summary_indicators.csv", row.names = FALSE)
  
  
  
  source("scripts/general_encoder.R")
  
  
  labs <- c(# 2. Acute-Phase Proteins
  "crp_lborres", "agp_lborres",
  
  # 3. Common Infections
  "malbl_lbperf_1", "malbl_lborres", "malbl_tk_ct_1", "malbl_tn_ct_1",
  "placmal_lbperf_1", "placmal_lborres",
  "ua_prot_lborres", "ua_leuk_lborres", "ua_nitrite_lborres",
  "tb_cnfrm_lborres",
  
  # 4. Hemoglobinopathies and G6PD
  "rbc_lbperf_1", "rbc_sickle_lborres",
  "rbc_lbperf_2", "rbc_thala_lborres",
  #paste0("rbc_thala_", 1:15),
  "rbc_lbperf_3", "rbc_g6pd_lborres",
  
  # 5. Pregnancy Outcomes
  "bgluc_pretest_mmoll_lborres", "bgluc_oral_1hr_mmoll_lborres", "bgluc_oral_2hr_mmoll_lborres",
  "hba1c_prcnt",
  "tbilirubin_lborres",
  
  # 6. Covariates
  #"type_visit", "lb_remapp3_tri",
  "trimester_label",
  
  # Objective 1 CBC
  "cbc_neu_fcc_lborres", "cbc_lymph_fcc_lborres")

#demo %>% select(Age_group,Employment,Education,Marita_status,ethnic,labs) %>% dfSummary( na.rm = FALSE,graph.col = TRUE,varnumbers = TRUE, graph.magnif = 0.75) %>% stview()  # Include missing value info

clean_data %>% select(labs) %>% dfSummary( na.rm = FALSE,graph.col = TRUE,varnumbers = TRUE, graph.magnif = 0.75) %>% stview()  # Include missing value info

# clon <- long_data %>% filter(malbl_lbperf_1==1)
# 
# ctable(long_data$trimester_label,long_data$malbl_lborres, useNA = "always") %>% stview()














# 
# library(dplyr)
# 
# your_data <- your_data %>%
#   mutate(
#     # Malaria & Parasitemia
#     malaria_status = factor(malaria_status, levels = c("Negative", "Positive")),
#     placental_malaria = factor(placental_malaria, levels = c("Negative", "Positive")),
#     parasite_species = factor(parasite_species, levels = c("P.f", "Pm", "P.o")),
#     parasitemia_density = case_when(
#       parasitemia_density <= 1000 ~ "Low",
#       parasitemia_density > 1000 & parasitemia_density <= 10000 ~ "Medium",
#       parasitemia_density > 10000 ~ "High",
#       TRUE ~ NA_character_
#     ),
#     parasitemia_density = factor(parasitemia_density, levels = c("Low", "Medium", "High")),
#     
#     # STIs
#     hiv_status = factor(hiv_status, levels = c("Negative", "Positive")),
#     hep_b_status = factor(hep_b_status, levels = c("Negative", "Positive")),
#     gonorrhea_status = factor(gonorrhea_status, levels = c("Negative", "Positive")),
#     syphilis_status = factor(syphilis_status, levels = c("Negative", "Positive")),
#     chlamydia_status = factor(chlamydia_status, levels = c("Negative", "Positive")),
#     
#     # UTI and Viral Infections
#     uti_status = factor(uti_status, levels = c("Negative", "Positive")),
#     viral_infections = factor(viral_infections, levels = c("Negative", "Positive")),
#     
#     # Pregnancy complications
#     preeclampsia = factor(preeclampsia, levels = c("No", "Yes")),
#     gestational_diabetes = factor(gestational_diabetes, levels = c("No", "Yes")),
#     perinatal_depression = factor(perinatal_depression, levels = c("No", "Yes")),
#     
#     # Birth outcomes
#     birth_asphyxia = factor(birth_asphyxia, levels = c("No", "Yes")),
#     neonatal_mortality = factor(neonatal_mortality, levels = c("No", "Yes")),
#     
#     # Preterm birth category (based on gestational age)
#     preterm_category = case_when(
#       gestational_age < 28 ~ "Extremely preterm",
#       gestational_age >= 28 & gestational_age < 32 ~ "Very preterm",
#       gestational_age >= 32 & gestational_age < 37 ~ "Moderate to late preterm",
#       gestational_age >= 37 ~ "Term",
#       TRUE ~ NA_character_
#     ),
#     preterm_category = factor(preterm_category, levels = c("Extremely preterm", "Very preterm", "Moderate to late preterm", "Term")),
#     
#     # Stillbirth category
#     stillbirth_category = case_when(
#       gestational_age >= 20 & gestational_age < 28 ~ "Early stillbirth",
#       gestational_age >= 28 & gestational_age < 37 ~ "Late stillbirth",
#       gestational_age >= 37 ~ "Term stillbirth",
#       TRUE ~ NA_character_
#     ),
#     stillbirth_category = factor(stillbirth_category, levels = c("Early stillbirth", "Late stillbirth", "Term stillbirth")),
#     
#     # Low birth weight category
#     lbw_category = case_when(
#       birth_weight < 1500 ~ "Very low birth weight",
#       birth_weight >= 1500 & birth_weight < 2500 ~ "Low birth weight",
#       birth_weight >= 2500 ~ "Normal weight",
#       TRUE ~ NA_character_
#     ),
#     lbw_category = factor(lbw_category, levels = c("Very low birth weight", "Low birth weight", "Normal weight")),
#     
#     # Sepsis category (based on age in hours)
#     sepsis_category = case_when(
#       sepsis_onset_hours <= 72 ~ "Early Onset Sepsis",
#       sepsis_onset_hours > 72 & sepsis_onset_hours <= 672 ~ "Late Onset Sepsis",
#       TRUE ~ NA_character_
#     ),
#     sepsis_category = factor(sepsis_category, levels = c("Early Onset Sepsis", "Late Onset Sepsis")),
#     
#     # Hyperbilirubinemia category
#     bilirubin_cat = case_when(
#       bilirubin_percentile > 95 ~ "Severe",
#       bilirubin_percentile > 75 ~ "Moderate",
#       TRUE ~ "Mild"
#     ),
#     bilirubin_cat = factor(bilirubin_cat, levels = c("Mild", "Moderate", "Severe"))
#   )
# 
# 
# 
# 
# 
# 
# 


