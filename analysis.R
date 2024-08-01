# Simple cleaning and exploration of WI school finance and census data
# S. Kolbe
# Updated 2024.07.31

# Set up ----------

library(tidyverse)
library(sf)

options(scipen = 999)

# Load data ----------

finance <- read_csv("source_data/compcost_sum_1415_to_2223.csv")

geography <- read_sf("source_data/Wisconsin_School_Districts/WI_School_Districts.shp")

# Clean and prep data ----------

# Clean up column names
colnames(finance) <- tolower(str_replace_all(colnames(finance), " ", "_"))

# Format and add basic aggregations for finance data
dist_finance <- finance %>%
  pivot_longer(cols = c(starts_with("member"), starts_with("instruct"), starts_with("pupil"),
                        starts_with("admin"), starts_with("operat"), starts_with("trans"),
                        starts_with("facil"), starts_with("food")),
                names_to = c('.value', 'fiscal_year'),
                names_pattern = '(.*)_([0-9]{4})$') %>%
  rename(support = pupilstaffsupport,
         food = foodcommservice) %>%
  # Create fields for Total Current Educational Cost (TCEC), Total Education Cost (TEC), and Total District Cost (TDC)
  mutate(tcec = select(., instruction:operations) %>% rowSums(na.rm = TRUE),
         tec = select(., instruction:facility) %>% rowSums(na.rm = TRUE),
         tdc = select(., instruction:food) %>% rowSums(na.rm = TRUE)) %>%
  # Format district code
  mutate(district_code = str_pad(district_code, 4, side = c("left"), pad = "0")) %>%
  # Filter out state totals
  filter(district_code != 9999)

# Clean up columnn names in geography file
dist_geo <- geography %>%
  rename_all(tolower) %>%
  rename(district_code = sdid)

# Join finance and spatial data ----------

# Check for districts present in the geography but missing in the finance data
missing <- anti_join(dist_geo, dist_finance, by="district_code")

# Join geographic and finance data
district <- left_join(dist_geo, dist_finance, by = "district_code")

# Plot ----------
selected_dist <- filter(district, fiscal_year == 2023)
ggplot() +
  geom_sf(data = selected_dist) +
  theme_minimal()

