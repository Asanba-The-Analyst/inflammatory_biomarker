my_list <- list(
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
  "cbc_pct_pct_lborres"
)
my_list <- list(
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
  "ibilirubin_lborres"
)


my_list <- list(
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
  "tb_backup_lborres"
)

my_list <- list(
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
  "hba1c_prcnt"
)


my_list <- list(
  "rbc_lbperf_3",
  "lborres",
  "rbc_g6pd_lbstdat",
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
  "tb_lbperf_2"
)


#hepatitis
my_list <- list(
  "hev_lbtstdat",
  "hev_lbperf_1",
  "hev_igm_lborres",
  "hev_lbperf_2",
  "hev_igg_lborres"
)


gest_diab_list <- list(
  "gest_diab_mhoccur",
  "gest_diab_srce_1",
  "gest_diab_srce_2",
  "gest_diab_srce_3",
  "gest_diab_proccur_1",
  "gest_diab_proccur_2",
  "gest_diab_proccur_3",
  "gest_diab_proccur_4",
  "gest_diab_proccur_88",
  "gest_diab_proccur_77",
  "gest_diab_proccur_99"
)



variables_list <- list(
  
  # 1. White Blood Cell (WBC)-Based Inflammatory Indices
  wbc_vars = list(
    raw = c(
      "cbc_neu_fcc_lborres",  # Neutrophils
      "cbc_lymph_fcc_lborres",  # Lymphocytes
      "cbc_mono_fcc_lborres",  # Monocytes
      "cbc_plate_lborres",  # Platelets
      "cbc_wbc_lborres"  # Total WBC count
    ),
    derived = c(
      "nlr",  # Neutrophil-to-Lymphocyte Ratio (Neutrophils/Lymphocytes)
      "plr",  # Platelet-to-Lymphocyte Ratio (Platelets/Lymphocytes)
      "lmr",  # Lymphocyte-to-Monocyte Ratio (Lymphocytes/Monocytes)
      "sii",  # Systemic Immune-Inflammation Index (Platelets × Neutrophils / Lymphocytes)
      "siri"  # Systemic Inflammatory Response Index (Monocytes × Neutrophils / Lymphocytes)
    )
  ),
  
  # 2. Acute-Phase Proteins (Inflammatory Markers)
  acute_phase_vars = c(
    "crp_lborres",  # C-reactive protein (mg/L)
    "agp_lborres"  # Alpha-1 acid glycoprotein (g/L)
  ),
  
  # 3. Common Infections
  infections = list(
    malaria = list(
      peripheral = c(
        "malbl_lbperf_1",  # Test performed
        "malbl_lborres",  # Results (Positive/Negative)
        "malbl_tk_ct_1",  # Species-specific counts (e.g., P. falciparum)
        "malbl_tn_ct_1"  
      ),
      placental = c(
        "placmal_lbperf_1",  # Test performed
        "placmal_lborres"  # Results (Acute/Chronic/Past infection)
      )
    ),
    uti = c(
      "ua_prot_lborres",  # Proteinuria
      "ua_leuk_lborres",  # Leukocytes
      "ua_nitrite_lborres"  # Nitrites
    ),
    tb = "tb_cnfrm_lborres",  # GeneXpert/MTB-RIF result (Positive/Negative/Inconclusive)
    other_infections = c(
      # HIV, Hepatitis B/C/E, Syphilis, Zika, Dengue, Chikungunya, Gonorrhea, Chlamydia
      # (Variables not explicitly named in the writeup)
    )
  ),
  
  # 4. Hemoglobinopathies and G6PD Status
  hemoglobin_vars = list(
    sickle_cell = c(
      "rbc_lbperf_1",  # Test performed
      "rbc_sickle_lborres"  # Results (Normal/Disease present)
    ),
    thalassemia = c(
      "rbc_lbperf_2",  # Test performed
      "rbc_thala_lborres",  # Results (Normal/Abnormal)
      paste0("rbc_thala_", 1:15)  # Subtypes (e.g., HbSS, HbAS)
    ),
    g6pd = c(
      "rbc_lbperf_3",  # Test performed
      "rbc_g6pd_lborres"  # Enzyme activity
    )
  ),
  
  # 5. Pregnancy Outcomes
  pregnancy_outcomes = list(
    gestational_diabetes = c(
      "bgluc_pretest_mmoll_lborres",  # Fasting glucose
      "bgluc_oral_1hr_mmoll_lborres",  # 1hr Oral glucose tolerance test
      "bgluc_oral_2hr_mmoll_lborres",  # 2hr Oral glucose tolerance test
      "hba1c_prcnt"  # HbA1c
    ),
    preeclampsia = "ua_prot_lborres",  # Proteinuria
    neonatal = "tbilirubin_lborres"  # Total bilirubin (hyperbilirubinemia)
  ),
  
  # 6. Covariates
  covariates = c(
    "type_visit",  # Trimester (Enrollment/ANC-20/ANC-28 etc.)
    "lb_remapp3_tri"  # ReMAPP-specific trimester
  )
)

# Print the structure (optional)
str(variables_list)

variables_list <- list(
  
  # ... (previous sections remain the same) ...
  
  # 4. Hemoglobinopathies and G6PD Status (updated)
  hemoglobin_vars = list(
    sickle_cell = c(
      "rbc_lbperf_1",          # Test performed
      "rbc_sickle_lborres",    # Results (Normal/Disease present)
      "rbc_sickle_lbstdat"     # Date of sickle cell test
    ),
    thalassemia = c(
      "rbc_lbperf_2",          # Test performed
      "rbc_thala_lborres",     # Results (Normal/Abnormal)
      "rbc_thala_lbstdat",     # Date of thalassemia test
      paste0("rbc_thala_", 1:15)  # Subtypes (HbSS, HbAS, etc.)
    ),
    g6pd = c(
      "rbc_lbperf_3",          # Test performed
      "rbc_g6pd_lborres"       # Enzyme activity
    )
  ),
  
  # ... (rest of the list remains unchanged) ...
)

# Print structure (optional)
variables_list <- list(
  
  # 1. White Blood Cell (WBC)-Based Inflammatory Indices
  wbc_vars = list(
    raw = c(
      "cbc_neu_fcc_lborres",  # Neutrophils
      "cbc_lymph_fcc_lborres",  # Lymphocytes
      "cbc_mono_fcc_lborres",  # Monocytes
      "cbc_plate_lborres",  # Platelets
      "cbc_wbc_lborres"  # Total WBC count
    ),
    derived = c(
      "nlr",  # Neutrophil-to-Lymphocyte Ratio (Neutrophils/Lymphocytes)
      "plr",  # Platelet-to-Lymphocyte Ratio (Platelets/Lymphocytes)
      "lmr",  # Lymphocyte-to-Monocyte Ratio (Lymphocytes/Monocytes)
      "sii",  # Systemic Immune-Inflammation Index (Platelets × Neutrophils / Lymphocytes)
      "siri"  # Systemic Inflammatory Response Index (Monocytes × Neutrophils / Lymphocytes)
    )
  ),
  
  # 2. Acute-Phase Proteins (Inflammatory Markers)
  acute_phase_vars = c(
    "crp_lborres",  # C-reactive protein (mg/L)
    "agp_lborres"  # Alpha-1 acid glycoprotein (g/L)
  ),
  
  # 3. Common Infections
  infections = list(
    malaria = list(
      peripheral = c(
        "malbl_lbperf_1",  # Test performed
        "malbl_lborres",  # Results (Positive/Negative)
        "malbl_tk_ct_1",  # Species-specific counts (e.g., P. falciparum)
        "malbl_tn_ct_1"  
      ),
      placental = c(
        "placmal_lbperf_1",  # Test performed
        "placmal_lborres"  # Results (Acute/Chronic/Past infection)
      )
    ),
    uti = c(
      "ua_prot_lborres",  # Proteinuria
      "ua_leuk_lborres",  # Leukocytes
      "ua_nitrite_lborres"  # Nitrites
    ),
    tb = "tb_cnfrm_lborres",  # GeneXpert/MTB-RIF result (Positive/Negative/Inconclusive)
    other_infections = c(
      # HIV, Hepatitis B/C/E, Syphilis, Zika, Dengue, Chikungunya, Gonorrhea, Chlamydia
      # (Variables not explicitly named in the writeup)
    )
  ),
  
  # 4. Hemoglobinopathies and G6PD Status
  hemoglobin_vars = list(
    sickle_cell = c(
      "rbc_lbperf_1",  # Test performed
      "rbc_sickle_lborres"  # Results (Normal/Disease present)
    ),
    thalassemia = c(
      "rbc_lbperf_2",  # Test performed
      "rbc_thala_lborres",  # Results (Normal/Abnormal)
      paste0("rbc_thala_", 1:15)  # Subtypes (e.g., HbSS, HbAS)
    ),
    g6pd = c(
      "rbc_lbperf_3",  # Test performed
      "rbc_g6pd_lborres"  # Enzyme activity
    )
  ),
  
  # 5. Pregnancy Outcomes
  pregnancy_outcomes = list(
    gestational_diabetes = c(
      "bgluc_pretest_mmoll_lborres",  # Fasting glucose
      "bgluc_oral_1hr_mmoll_lborres",  # 1hr Oral glucose tolerance test
      "bgluc_oral_2hr_mmoll_lborres",  # 2hr Oral glucose tolerance test
      "hba1c_prcnt"  # HbA1c
    ),
    preeclampsia = "ua_prot_lborres",  # Proteinuria
    neonatal = "tbilirubin_lborres"  # Total bilirubin (hyperbilirubinemia)
  ),
  
  # 6. Covariates
  covariates = c(
    "type_visit",  # Trimester (Enrollment/ANC-20/ANC-28 etc.)
    "lb_remapp3_tri"  # ReMAPP-specific trimester
  )
)

# Print the structure (optional)
str(variables_list)