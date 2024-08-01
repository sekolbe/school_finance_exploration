# Simple cleaning and exploration of WI school finance data
# S. Kolbe
# Updated 2024.07.31

# Set up ----------

library(tidyverse)
library(sf)
library(paletteer)
library(gganimate)

options(scipen = 999)

# Load data ----------

finance <- read_csv("source_data/compcost_sum_1415_to_2223.csv")

geography <- read_sf("source_data/Wisconsin_School_Districts/WI_School_Districts.shp")

# Clean and prep data ----------

# Clean up column names
colnames(finance) <- tolower(str_replace_all(colnames(finance), " ", "_"))

# Format and add basic aggregations for finance data
dist_finance <- finance %>%
  pivot_longer(cols = c(starts_with("member"), starts_with("instruct"), 
                        starts_with("pupil"), starts_with("admin"), 
                        starts_with("operat"), starts_with("trans"),
                        starts_with("facil"), starts_with("food")),
                names_to = c('.value', 'fiscal_year'),
                names_pattern = '(.*)_([0-9]{4})$') %>%
  rename(support = pupilstaffsupport,
         food = foodcommservice) %>%
  # Create fields for Total Current Educational Cost (TCEC), Total Education Cost (TEC), and Total District Cost (TDC) per pupil
  mutate(tcec = select(., instruction:operations) %>% rowSums(na.rm = TRUE)/member,
         tec = select(., instruction:facility) %>% rowSums(na.rm = TRUE)/member,
         tdc = select(., instruction:food) %>% rowSums(na.rm = TRUE)/member) %>%
  # Format district code
  mutate(district_code = str_pad(district_code, 4, side = c("left"), pad = "0")) %>%
  # Filter out state totals
  filter(district_code != 9999)

# Clean up columnn names in geography file and reproject
dist_geo <- geography %>%
  rename_all(tolower) %>%
  rename(district_code = sdid) %>%
  st_transform(., 'EPSG:3070')

# Join finance and spatial data ----------

# Check for districts present in the geography but missing in the finance data
missing <- anti_join(dist_geo, dist_finance, by="district_code")

# Join geographic and finance data
district <- left_join(dist_geo, dist_finance, by = "district_code")

# Make gifs of finance measures ----------

# Create frames for a test gif
tcec_frames <- ggplot() + 
  # plot the state data
  geom_sf(data = district, aes(fill = tcec)) +
  scale_fill_paletteer_c("grDevices::Purple-Yellow", direction = -1) +
  labs(title = "Total Current Educational Cost (TCEC) Per Pupil", 
       subtitle = '{current_frame}',
       fill = "TCEC Per Pupil",
       caption = "Data from Wisconsin Department of Public Instruction") + 
  theme_minimal() +
  theme(legend.box.background = element_rect())+
  transition_manual(fiscal_year)
# Animate frames and save
animate(tcec_frames, fps = 10)

# Generalize code, generate gifs for other finance variables, and save
make_finance_gif <- function(my_data, my_field, my_title, my_abbr){
  tcec_frames <- ggplot() + 
    # plot the state data
    geom_sf(data = my_data, aes(fill = {{my_field}})) +
    scale_fill_paletteer_c("grDevices::Purple-Yellow", direction = -1) +
    labs(title = "{my_title} ({my_abbr}) Per Pupil", 
         subtitle = "{current_frame}",
         fill = "Per Pupil Cost",
         caption = "Data from Wisconsin Department of Public Instruction") + 
    theme_minimal() +
    theme(legend.box.background = element_rect())+
    transition_manual(fiscal_year)
  # Animate frames and save
  animate(tcec_frames, fps = 10)
  anim_save(str_c("output/Wisconsin ", my_abbr, " per pupil ", min(my_data$fiscal_year), "-",  max(my_data$fiscal_year), ".gif"))
}
make_finance_gif(district, tcec, "Total Current Educational Cost", "TCEC")
make_finance_gif(district, tec, "Total Educational Cost", "TEC")
make_finance_gif(district, tdc, "Total District Cost", "TDC")
