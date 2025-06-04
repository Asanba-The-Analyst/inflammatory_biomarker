variables <- c(
  # 2. Acute-Phase Proteins
  "crp_lborres", "agp_lborres",
  
  # 3. Common Infections
  "malbl_lbperf_1", "malbl_lborres", "malbl_tk_ct_1", "malbl_tn_ct_1",
  "placmal_lbperf_1", "placmal_lborres",
  "ua_prot_lborres", "ua_leuk_lborres", "ua_nitrite_lborres",
  "tb_cnfrm_lborres",
  
  # 4. Hemoglobinopathies and G6PD
  "rbc_lbperf_1", "rbc_sickle_lborres",
  "rbc_lbperf_2", "rbc_thala_lborres",
  paste0("rbc_thala_", 1:15),
  "rbc_lbperf_3", "rbc_g6pd_lborres",
  
  # 5. Pregnancy Outcomes
  "bgluc_pretest_mmoll_lborres", "bgluc_oral_1hr_mmoll_lborres", "bgluc_oral_2hr_mmoll_lborres",
  "hba1c_prcnt",
  "tbilirubin_lborres",
  
  # 6. Covariates
  "type_visit", "lb_remapp3_tri",
  
  # Objective 1 CBC
  "cbc_neu_fcc_lborres", "cbc_lymph_fcc_lborres"
)



