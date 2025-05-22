# Filter and merge datasets with type_visit 1-5
datasets_with_tv <- list(mnh01, mnh04, mnh06, mnh08, mnh25) %>%
  map(~ filter(.x, type_visit %in% trimester_visits))

# Merge all visit-based datasets
trimester_long <- reduce(datasets_with_tv, full_join, by = c("pregid", "momid", "type_visit"))

# Add static baseline info (mnh00, mnh02, mnh03)
baseline <- reduce(list(mnh00, mnh02, mnh03), full_join, by = c("pregid", "momid"))

# Combine final longitudinal dataset
long_data <- left_join(trimester_long, baseline, by = c("pregid", "momid"))
