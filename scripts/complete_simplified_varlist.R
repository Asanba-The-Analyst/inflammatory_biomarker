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