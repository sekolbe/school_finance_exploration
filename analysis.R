# Simple cleaning and exploration of WI school finance and census data
# S. Kolbe
# Updated 2024.07.31

# Set up ----------

library(tidyverse)

options(scipen = 999)

# Load data ----------

dist_finance <- read_csv("source_data/compcost_sum_1415_to_2223.csv")

# Clean and prep data ----------

districts <- dist_finance %>%
  pivot_longer(cols = c(starts_with("member"), starts_with("instruct"), starts_with("pupil"),
                        starts_with("admin"), starts_with("operat"), starts_with("trans"),
                        starts_with("facil"), starts_with("food")),
                names_to = c('.value', 'fiscal_year'),
                names_pattern = '(.*)_([0-9]{4})$') %>%
  rename(dist_code = DISTRICT_CODE,
         dist_name = DISTRICT_NAME,
         support = pupilstaffsupport,
         food = foodcommservice) %>%
  # Create fields for Total Current Educational Cost (TCEC), Total Education Cost (TEC), and Total District Cost (TDC)
  mutate(tcec = select(., instruction:operations) %>% rowSums(na.rm = TRUE),
         tec = select(., instruction:facility) %>% rowSums(na.rm = TRUE),
         tdc = select(., instruction:food) %>% rowSums(na.rm = TRUE)) %>%
  # Filter out state totals
  filter(dist_code != 9999)
               