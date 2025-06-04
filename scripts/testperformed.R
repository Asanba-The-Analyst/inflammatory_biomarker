library(dplyr)
library(gt)
library(lubridate)

# First, process the data as specified
df_processed <- long_data %>%
  mutate(across(ends_with("_lbtstdat") | ends_with("lbstdat"), as.Date)) %>%
  mutate(across(starts_with("cbc_lbperf") | starts_with("lft_lbperf") |
                  starts_with("renal_lbperf") | starts_with("mn_lbperf") |
                  starts_with("thyroid_lbperf") | starts_with("bgluc_lbperf") |
                  starts_with("hba1c_test_yn") | starts_with("rbc_lbperf") |
                  starts_with("ua_lbperf") | starts_with("tb_lbperf") |
                  starts_with("blead_lbperf") | starts_with("malbl_lbperf") |
                  starts_with("placmal_lbperf") | starts_with("hb_poc_lbperf") |
                  starts_with("malaria_poc_lbperf") | starts_with("hiv_poc_lbperf") |
                  starts_with("syph_poc_lbperf") | starts_with("hbv_poc_lbperf") |
                  starts_with("hcv_poc_lbperf") | starts_with("bgluc_poc_lbperf") |
                  starts_with("covid_poc_lbperf"), 
                ~ case_when(
                  .x == 1 ~ "Yes",
                  .x == 0 ~ "No",
                  TRUE ~ as.character(.x)
                )))
         
         # Create summary table
         summary_table <- df_processed %>%
           select(ends_with("_lbtstdat") | ends_with("lbstdat") |
                    starts_with("cbc_lbperf") | starts_with("lft_lbperf") |
                    starts_with("renal_lbperf") | starts_with("mn_lbperf") |
                    starts_with("thyroid_lbperf") | starts_with("bgluc_lbperf") |
                    starts_with("hba1c_test_yn") | starts_with("rbc_lbperf") |
                    starts_with("ua_lbperf") | starts_with("tb_lbperf") |
                    starts_with("blead_lbperf") | starts_with("malbl_lbperf") |
                    starts_with("placmal_lbperf") | starts_with("hb_poc_lbperf") |
                    starts_with("malaria_poc_lbperf") | starts_with("hiv_poc_lbperf") |
                    starts_with("syph_poc_lbperf") | starts_with("hbv_poc_lbperf") |
                    starts_with("hcv_poc_lbperf") | starts_with("bgluc_poc_lbperf") |
                    starts_with("covid_poc_lbperf")) %>%
           # Create a summary data frame
           summarise(across(everything(), 
                            list(
                              N = ~sum(!is.na(.)),
                              Missing = ~sum(is.na(.)),
                              Yes = ~sum(. == "Yes", na.rm = TRUE),
                              No = ~sum(. == "No", na.rm = TRUE),
                              Min_Date = ~min(., na.rm = TRUE),
                              Max_Date = ~max(., na.rm = TRUE)
                            )) %>%
                       pivot_longer(everything(), 
                                    names_to = c("Variable", ".value"), 
                                    names_sep = "_") %>%
                       mutate(Yes_Pct = ifelse(N > 0, round(Yes/N*100, 1), NA),
                              No_Pct = ifelse(N > 0, round(No/N*100, 1), NA)),
                     
                     # Create GT table
                     gt_table <- summary_table %>%
                       gt() %>%
                       tab_header(
                         title = "Laboratory Test Performance Summary",
                         subtitle = "Date and binary test performance variables"
                       ) %>%
                       fmt_number(
                         columns = c(N, Missing, Yes, No),
                         decimals = 0
                       ) %>%
                       fmt_percent(
                         columns = c(Yes_Pct, No_Pct),
                         decimals = 1
                       ) %>%
                       fmt_date(
                         columns = c(Min_Date, Max_Date),
                         date_style = "yMd"
                       ) %>%
                       cols_label(
                         Variable = "Test",
                         N = "Total",
                         Missing = "Missing",
                         Yes = "Performed (n)",
                         No = "Not Performed (n)",
                         Yes_Pct = "Performed (%)",
                         No_Pct = "Not Performed (%)",
                         Min_Date = "Earliest Date",
                         Max_Date = "Latest Date"
                       ) %>%
                       tab_style(
                         style = cell_text(weight = "bold"),
                         locations = cells_column_labels()
                       ) %>%
                       tab_options(
                         table.width = "100%",
                         table.font.size = 12
                       ))
                     
                     # Display the table
                     gt_table