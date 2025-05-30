
test_performed_vars <- c(
  # CBC Panel
  "cbc_lbperf_1",  # Hemoglobin (Hb)
  "cbc_lbperf_2",  # Hematocrit (HCT)
  "cbc_lbperf_3",  # White Blood Cells (WBC)
  "cbc_lbperf_4",  # Neutrophils (%)
  "cbc_lbperf_5",  # Neutrophils (full cell count)
  "cbc_lbperf_6",  # Lymphocyte (%)
  "cbc_lbperf_7",  # Lymphocyte (full cell count)
  "cbc_lbperf_8",  # Erythrocyte count
  "cbc_lbperf_9",  # Mean cell volume (MCV)
  "cbc_lbperf_10", # Mean cell hemoglobin (MCH)
  "cbc_lbperf_11", # Mean corpuscular hemoglobin concentration (MCHC)
  "cbc_lbperf_12", # Platelets count
  "cbc_lbperf_13", # Monocyte (%)
  "cbc_lbperf_14", # Monocyte (full cell count)
  "cbc_lbperf_15", # Eosinophils (%)
  "cbc_lbperf_16", # Eosinophils (full cell count)
  "cbc_lbperf_17", # Red cell width (RDW)
  "cbc_lbperf_18", # Platelet distribution width (PDW)
  "cbc_lbperf_19", # Plateletcrit (PCT)
  
  # Liver Function Tests (LFT)
  "lft_lbperf_1",  # AST/SGOT
  "lft_lbperf_2",  # ALT/SGPT
  "lft_lbperf_3",  # Alkaline phosphatase (ALP)
  "lft_lbperf_4",  # Total Bilirubin
  "lft_lbperf_5",  # Direct Bilirubin
  "lft_lbperf_6",  # Total Protein
  "lft_lbperf_7",  # Albumin
  "lft_lbperf_8",  # Gamma GT
  "lft_lbperf_9",  # Indirect Bilirubin
  
  # Renal Function Tests
  "renal_lbperf_1", # BUN
  "renal_lbperf_2", # Serum creatinine
  "renal_lbperf_3", # Sodium
  "renal_lbperf_4", # Potassium
  "renal_lbperf_5", # Chloride
  "renal_lbperf_6", # Phosphorus
  "renal_lbperf_7", # Calcium
  "renal_lbperf_8", # Carbon dioxide
  "renal_lbperf_9", # Magnesium
  
  # Micronutrients
  "mn_lbperf_1",   # Vitamin B12 (Total Cobalamin)
  "mn_lbperf_2",   # Holotranscobalamin II
  "mn_lbperf_3",   # Folate (Blood serum)
  "mn_lbperf_4",   # Zinc
  "mn_lbperf_5",   # Iron (Hepcidin)
  "mn_lbperf_6",   # Total iron binding capacity
  "mn_lbperf_7",   # Vitamin A (Serum retinol)
  "mn_lbperf_8",   # Ferritin
  "mn_lbperf_9",   # Iodine (Tg)
  "mn_lbperf_10",  # Serum Transferrin receptor (sTfR)
  "mn_lbperf_11",  # Retinol protein binding 4 (RBP4)
  "mn_lbperf_12",  # C-reactive protein (CRP)
  "mn_lbperf_13",  # Alpha 1-acid glycoprotein (AGP)
  "mn_lbperf_14",  # Histidine-rich protein 2 (HRP)
  "mn_lbperf_15",  # Folate (RBC)
  
  # Thyroid Function Tests
  "thyroid_lbperf_1", # TSH
  "thyroid_lbperf_2", # Free T4
  "thyroid_lbperf_3", # Free T3
  
  # Blood Glucose Tests
  "bgluc_lbperf_1",   # Fasting glucose
  "bgluc_lbperf_2",   # Oral glucose-tolerance (1-hour)
  "bgluc_lbperf_3",   # Oral glucose-tolerance (2-hour)
  
  # HbA1c Test
  "hba1c_test_yn",    # HbA1c test performed
  
  # RBC Disorders
  "rbc_lbperf_1",     # Sickle cell disease
  "rbc_lbperf_2",     # Hemoglobinopathies & Thalassemias
  "rbc_lbperf_3",     # G6PD
  
  # Urinalysis
  "ua_lbperf_1",      # Protein
  "ua_lbperf_2",      # Leukocytes
  "ua_lbperf_3",      # Nitrites
  
  # TB Tests
  "tb_lbperf_1",      # Gene Xpert/MTB-RIF
  "tb_lbperf_2",      # Backup culture
  
  # Blood Lead Test
  "blead_lbperf_1",   # Blood lead test
  
  # Malaria Blood Slide
  "malbl_lbperf_1",   # Malaria blood slide
  
  # Placental Malaria
  "placmal_lbperf_1"  # Placental malaria test
)






lab_result_vars <- c(
  # CBC Panel
  "cbc_hb_lborres",        # Hemoglobin (Hb) g/dL
  "cbc_hct_lborres",       # Hematocrit (HCT) %
  "cbc_wbc_lborres",       # White Blood Cells (WBC) x10³/mm³
  "cbc_neu_pct_lborres",   # Neutrophils (%)
  "cbc_neu_fcc_lborres",   # Neutrophils (full cell count)
  "cbc_lymph_pct_lborres", # Lymphocyte (%)
  "cbc_lymph_fcc_lborres", # Lymphocyte (full cell count)
  "cbc_eryth_mm_lborres",  # Erythrocyte count x10^6/mm³
  "cbc_mcv_lborres",       # Mean cell volume (MCV) µm³
  "cbc_mch_lborres",       # Mean cell hemoglobin (MCH) pg/cell
  "cbc_mchc_gdl_lborres",  # MCHC g/dL
  "cbc_plate_lborres",     # Platelets count x10³/mm³
  "cbc_mono_pct_lborres",  # Monocyte (%)
  "cbc_mono_fcc_lborres",  # Monocyte (full cell count)
  "cbc_eos_pct_lborres",   # Eosinophils (%)
  "cbc_eos_fcc_lborres",   # Eosinophils (full cell count)
  "cbc_rdw_pct_lborres",   # Red cell width (RDW) %
  "cbc_pdw_ct_lborres",    # Platelet distribution width (PDW)
  "cbc_pct_pct_lborres",   # Plateletcrit (PCT) %
  
  # Liver Function Tests (LFT)
  "ast_ul_lborres",        # AST/SGOT (Units/L)
  "alt_ul_lborres",        # ALT/SGPT (Units/L)
  "alp_lborres",           # Alkaline phosphatase (ALP)
  "tbilirubin_lborres",    # Total Bilirubin (µmol/L)
  "dbilirubin_lborres",    # Direct Bilirubin (µmol/L)
  "tprotein_lborres",      # Total Protein (g/L)
  "albumin_lborres",       # Albumin (g/dL)
  "gammagt_lborres",       # Gamma GT (units/L)
  "ibilirubin_lborres",    # Indirect Bilirubin (µmol/L)
  
  # Renal Function Tests
  "bun_mmoll_lborres",     # BUN (mmol/L)
  "creat_umoll_lborres",   # Serum creatinine (µmol/L)
  "sodium_lborres",        # Sodium (mmol/L)
  "potassium_lborres",     # Potassium (mmol/L)
  "chloride_lborres",      # Chloride (mmol/L)
  "phosphorus_lborres",    # Phosphorus (mmol/L)
  "calcium_lborres",       # Calcium (mmol/L)
  "carb_diox_lborres",     # Carbon dioxide (mmol/L)
  "magnesium_lborres",     # Magnesium (mmol/L)
  
  # Micronutrients
  "vitb12_cob_lborres",    # Vitamin B12 (Total Cobalamin)
  "vitb12_hol_lborres",    # Holotranscobalamin II
  "folate_plasma_nmoll_lborres", # Folate (Blood serum)
  "zinc_lborres",          # Zinc (µg/dL)
  "iron_hep_lborres",      # Hepcidin (ng/mL)
  "iron_tot_ugdl_lborres", # Total iron binding capacity (µg/dL)
  "vita_ugdl_lborres",     # Vitamin A (Serum retinol)
  "ferritin_lborres",      # Ferritin (µg/dL)
  "iodine_lborres",        # Iodine (µg/L)
  "transferrin_lborres",   # Serum Transferrin receptor (sTfR)
  "rbp4_lborres",          # Retinol protein binding 4 (RBP4)
  "crp_lborres",           # C-reactive protein (CRP)
  "agp_lborres",           # Alpha 1-acid glycoprotein (AGP)
  "hrp_lborres",           # Histidine-rich protein 2 (HRP)
  "folate_rbc_nmoll_lborres", # Folate (RBC)
  
  # Thyroid Function Tests
  "thyroid_tsh_lborres",   # TSH (μIU/mL)
  "thyroid_freet4_lborres", # Free T4 (ng/dL)
  "thyroid_freet3_lborres", # Free T3 (pg/dL)
  
  # Blood Glucose Tests
  "bgluc_pretest_mmoll_lborres", # Fasting glucose (mmol/L)
  "bgluc_oral_1hr_mmoll_lborres", # Oral glucose-tolerance (1-hour)
  "bgluc_oral_2hr_mmoll_lborres", # Oral glucose-tolerance (2-hour)
  
  # HbA1c Test
  "hba1c_lborres",         # HbA1c (mmol/mol)
  "hba1c_prcnt",           # HbA1c (%)
  
  # RBC Disorders
  "rbc_sickle_lborres",    # Sickle cell disease
  "rbc_thala_lborres",     # Hemoglobinopathies & Thalassemias
  "rbc_g6pd_lborres",      # G6PD (U/g Hb)
  
  # Urinalysis
  "ua_prot_lborres",       # Protein
  "ua_leuk_lborres",       # Leukocytes
  "ua_nitrite_lborres",    # Nitrites
  
  # TB Tests
  "tb_cnfrm_lborres",      # Gene Xpert/MTB-RIF
  "tb_backup_lborres",     # Backup culture
  
  # Blood Lead Test
  "blead_lborres",         # Blood lead (µg/dL)
  
  # Malaria Blood Slide
  "malbl_lborres",         # Malaria blood slide
  
  # Placental Malaria
  "placmal_lborres"        # Placental malaria test
)