
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
#the following data was retrieved from the Massachusetts, Energy and Environmental Affairs Data Poral
#https://eeaonline.eea.state.ma.us/Portal/#!/search/drinking-water

#Data was filtered for Contaminant Group = PFAS, and RAW
#In R, raw water is filterd to only include surface water sites on the EEA data portal

MassachusettsRawDrinkingWater_RAW <- read_excel("~/Global Papers/North America/MassachusettsRawDrinkingWater_RAW.xlsx")

ma <- MassachusettsRawDrinkingWater_RAW %>%
  clean_names()

ma <- ma %>%
  mutate(
    location_name = str_to_lower(location_name),
    raw_or_finished = str_to_lower(raw_or_finished)
  )

surface_keywords <- c(
  "reservoir",
  "lake",
  "pond",
  "river",
  "stream",
  "brook",
  "surface",
  "intake",
  "source",
  "tributary",
  "brooke",
  "swamp",
  "raw",
  "cove", #i noticed one of the sites on concord river is called cove
  #going to add cove here in case there are multiple like that
  "res" #short for reservoir
)

##I want to exclude wells, tap water, and treated water. 
exclude_keywords <- c(
  "well",
  "tap",
  "finished",
  "distribution",
  "treatment",
  "effluent"
)

surface_pattern <- str_c(surface_keywords, collapse = "|")
exclude_pattern <- str_c(exclude_keywords, collapse = "|")

ma_surface <- ma %>%
  filter(
    str_detect(location_name, surface_pattern),
    !str_detect(location_name, exclude_pattern),
    raw_or_finished == "r"
  )

ma_surface <- ma_surface %>%
  mutate(
    sample_id = paste(
      pws_id,
      location_name,
      collected_date,
      method,
      sep = "_"
    )
  )

ma_surface <- ma_surface %>%
  mutate(
    analyte = case_when(
      str_detect(chemical_name, "OCTANOIC") ~ "PFOA",
      str_detect(chemical_name, "OCTANE SULFONIC") ~ "PFOS",
      str_detect(chemical_name, "HEXANE SULFONIC") ~ "PFHxS",
      str_detect(chemical_name, "NONANOIC") ~ "PFNA",
      TRUE ~ chemical_name
    )
  )

ma_surface %>%
  count(
    pws_id,
    location_name,
    collected_date
  ) %>%
  arrange(desc(n))

ma_surface %>%
  count(sample_id, chemical_name) %>%
  filter(n > 1)


ma_surface <- ma_surface %>%
  group_by(sample_id, chemical_name) %>%
  mutate(replicate_id = row_number()) %>%
  ungroup()

library(tidyr)

ma_wide <- ma_surface %>%
  select(
    sample_id,
    replicate_id,
    pws_id,
    pws_name,
    town,
    collected_date,
    location_name,
    raw_or_finished,
    method,
    chemical_name,
    result
  ) %>%
  pivot_wider(
    names_from = chemical_name,
    values_from = result
  )%>% 
  mutate(pws_id = as.double(pws_id))

#okay so now that i have surface water chemistry figured out, 
#i'm going to bring in my coordinate for each surface Public Water Supply 
# coords taken from MassGIS data: Public Water Supplies
# https://www.mass.gov/info-details/massgis-data-public-water-supplies

library(readr)
PWIS_coords <- read_csv("~/Global Papers/North America/Massachusetts_PublicWaterSuppy_DEP_Coordinates/PWIS coords.csv")
head(PWIS_coords)

PWIS_coords <- PWIS_coords %>% 
  rename(pws_id = PWS_ID) %>%
rename(Latitude = LATITUDE) %>%
rename(Longitude = LONGITUDE)
head(PWIS_coords)

PWIS_coords_unique <- PWIS_coords %>% distinct(pws_id, .keep_all = TRUE)
                                        
MA_PWS_PFAS <- inner_join(ma_wide, PWIS_coords_unique, by = "pws_id")

MA_PWS_PFAS <- MA_PWS_PFAS %>% 
  filter(TYPE %in% c("SW", "ESW"))

write.csv(MA_PWS_PFAS, "MA_PublicWaterSupply_PFAS.csv")
