# Load required packages
library(tidyverse)
library(lubridate) # for date handling

mnh02 <- read.csv("data/mnh02.csv",check.names = FALSE)

names(mnh02) <- tolower(names(mnh02))

# mnh02$scrn_obsstdat <-mdy(mnh02$scrn_obsstdat) 
# # 'mnh02'
# eligible_participants <- mnh02 %>% 
#   # Filter for participants who returned for screening (scrn_return == 1)
#   filter(scrn_return == 1) %>% 
#   # Filter for participants who meet age requirement (age_ieorres == 1)
#   filter(age_ieorres == 1) %>% 
#   # Filter for participants with confirmed viable pregnancy <20 weeks (pc_ieorres == 1)
#   filter(pc_ieorres == 1) %>% 
#   # Filter for participants living in catchment area (catchment_ieorres == 1)
#   filter(catchment_ieorres == 1) %>% 
#   # Filter for participants planning to remain in catchment area (catch_remain_ieorres == 1)
#   filter(catch_remain_ieorres == 1) %>% 
#   # Filter for participants still willing/able to consent (consent_ieorres == 1)
#   filter(consent_ieorres == 1) %>% 
#   # Remove records with invalid screening dates (not missing/refused/etc.)
#   filter(!scrn_obsstdat %in% mdy(c("1907-07-07", "1905-05-05", "1906-06-06", "1909-09-09"))) %>% 
#   # Select only the relevant columns (optional)
#   select(scrnid,momid,pregid, scrn_obsstdat)
# 
# # View the first few rows of eligible participants
# head(eligible_participants)







# better way

mnh02 <- mnh02 %>%
  mutate(
    scrn_obsstdat = case_when(
      scrn_obsstdat %in% c("07-07-1907", "05-05-1905", "06-06-1906", "09-09-1909") ~ NA_character_,
      TRUE ~ scrn_obsstdat
    ),
    scrn_obsstdat = dmy(scrn_obsstdat)  # Now convert valid dates
  )


eligible_participants <- mnh02 %>%
  filter(
    scrn_return == 1,
    age_ieorres == 1,
    pc_ieorres == 1,
    catchment_ieorres == 1,
    catch_remain_ieorres == 1,
    consent_ieorres == 1,
    !is.na(scrn_obsstdat)  # Exclude missing/invalid dates
  ) %>% 
  # Select only the relevant columns (optional)
  select(scrnid,momid,pregid, scrn_obsstdat)


write.csv(eligible_participants,"eligible_IDs.csv")
