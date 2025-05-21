# Load libraries
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(caret)
library(missMDA)  # For missing value imputation

df<-mnh03

mnh03_variables <- c(
  "momid", "pregid", 
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
  "mosquito_net_fcorres", "tricycles_fcorres", 
  "tables_fcorres", "cabinets_fcorres", "sat_dish_fcorres", 
  "dvd_cd_fcorres", "aircon_fcorres", "tractor_fcorres", 
  "own_rent_scorres", "own_rent_spfy_scorres", 
  "house_rooms_fcorres", "house_room_child_fcorres", "land_fcorres", 
  "livestock_fcorres", "cattle_fcorres", "goat_fcorres", 
  "sheep_fcorres", "poultry_fcorres", "pig_fcorres", 
  "donkey_fcorres", "horse_fcorres", "animal_othr_fcorres", 
  "job_scorres", "stove_fcorres"
)

df <- mnh03[,mnh03_variables]

#"house_occ_tot_fcorres", "ext_wall_fcorres", 
#"floor_fcorres", "roof_fcorres", "own_rent_scorres", "animal_othr_fcorres", 
#"job_scorres", "stove_fcorres","house_rooms_fcorres"
ses_vars <- c( "electricity_fcorres", 
              "solar_fcorres", "internet_fcorres", "landline_fcorres", 
              "mobile_fcorres", "mobile_access_fcorres", "radio_fcorres", 
              "tv_fcorres", "fridge_fcorres", "computer_fcorres", 
              "watch_fcorres", "bike_fcorres", "motorcycle_fcorres", 
              "car_fcorres", "boat_fcorres", "cart_fcorres", 
              "plough_fcorres", "foam_matt_fcorres", "straw_matt_fcorres", 
              "spring_matt_fcorres", "sofa_fcorres", "lantern_fcorres", 
              "sew_fcorres", "wash_fcorres", "blender_fcorres", 
              "mosquito_net_fcorres", "tricycles_fcorres", 
              "tables_fcorres", "cabinets_fcorres", "sat_dish_fcorres", 
              "dvd_cd_fcorres", "aircon_fcorres", "tractor_fcorres", 
              "house_room_child_fcorres", "land_fcorres", 
              "livestock_fcorres", "cattle_fcorres", "goat_fcorres", 
              "sheep_fcorres", "poultry_fcorres", "pig_fcorres", 
              "donkey_fcorres", "horse_fcorres") 

df <- df %>%
  mutate(across(any_of(ses_vars), 
                ~{
                  if (is.character(.)) {
                    x <- as.factor(.)
                  } else {
                    x <- .
                  }
                  x_num <- as.numeric(as.character(x))
                  ifelse(x_num %in% c(55, 66, 77, 88), 0, x_num)
                }))
df <- df %>%
  mutate(across(any_of(ses_vars) & where(is.factor), ~as.numeric(as.character(.))))

# Select relevant SES/wealth indicators


# Drop rows with NA in any SES variable or impute as needed
df_clean <- df %>% 
  select(momid, pregid, all_of(ses_vars)) %>%
  drop_na()



# 1. Remove problematic variables (zero-variance and near-zero variance)
vars_to_remove <- c(
  "horse_fcorres",          # Zero variance (all values identical)
  "boat_fcorres", "cart_fcorres", "plough_fcorres", "donkey_fcorres",  # Extreme skew (freqRatio > 500)
  "aircon_fcorres", "tractor_fcorres", "spring_matt_fcorres", "wash_fcorres","own_rent_spfy_scorres" # High skew (freqRatio > 50)
)

ses_reduced <- df[, !colnames(df) %in% vars_to_remove]

# 2. Handle missing/infinite values
ses_clean <- ses_reduced %>%
  as.data.frame() %>%
  # Convert infinite values to NA
  mutate(across(everything(), ~ifelse(is.infinite(.), NA, .))) %>%
  # Option A: Remove rows with NA (if few missing values)
  na.omit()

# 3. Final check for zero-variance (should return empty)
zero_var <- nearZeroVar(ses_clean, saveMetrics = TRUE) %>% filter(zeroVar)
if (nrow(zero_var) > 0) warning("Zero-variance columns remaining: ", rownames(zero_var))

ses_clean <- scale(ses_clean %>% select(-momid, -pregid))
# 4. Run PCA
pca_result <- PCA(ses_clean, graph = FALSE)


# Create wealth index from PC1
wealth_index <- pca_result$ind$coord[, 1]

# Add wealth index and SES quintiles to dataset
df_clean <- df_clean %>%
  mutate(
    wealth_index = wealth_index,
    ses_quintile = ntile(wealth_index, 5)  # 1 = poorest, 5 = richest
  )

# Final output with IDs, wealth index, and quintile
final_df <- df_clean %>% select(momid, pregid, wealth_index, ses_quintile)

# View first rows
#head(final_df)
