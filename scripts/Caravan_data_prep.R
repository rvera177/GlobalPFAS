#This script is part of the global PFAS modeling project
#Created February 2, 2026 by RV
#Last edited: February 24, 2026 by RV
#Edits will be uploaded to Github for easy access

#working with global caravan data. 
#this data comes from caravan-Qual lite. Released December 11, 2025
#link to lite dataset: https://zenodo.org/records/17787066 

#Question. where did they get this data?

#Answer from the preprint description -> The Water quality data in Caravan-Qual is  compiled from several existing 
#global, regional and national databases (Figure 4 of Caravan Preprint), 
#all of which have fully open-access licenses that permit redistribution, 
#sources:
# •Global: UNEP GEMS/Water 
# •Global Freshwater Quality Archive (GEMS)
# •Global: Global River Water Quality Archive (GRQA)
# •Global: GLObal River Chemistry (GLORICH) dataset
# •Europe: NORMAN EMPODAT
# •Europe: Waterbase WISE State of Environment (Waterbase)
# •United States:Water Quality Portal (WQP)
# •China: China National Environmental Monitoring Centre (CNEMC)
# •United Kingdom: Department for Environment, Food and Rural Affairs (UK-EA)
# •Canada: Canadian Environmental Sustainability Indicators (CESI),
# •Switzerland: National Surface Water Quality Monitoring Programme (NAWA)

#"heavy" dataset can be found here: https://github.com/SustainableWaterSystems/Caravan-Qual 
#the heavy dataset includes additional streamflow and weather data, but no additional stream quality data
library(readr) #bring in the spatial coordinates with coresponding PFAS data
library(sf) #plotting the spatial objects
library(ggplot2)
library(rnaturalearth) #for plotting country  outlines
library(rnaturalearthdata)
library(dplyr)

#pull in global caravan data from my github. 
Caravan_PFOA_Raw <- read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/Global%20datasets/Caravan_PFOA.csv")
Caravan_PFOS_Raw = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/Global%20datasets/Caravan_PFOS.csv")
site_info = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/Global%20datasets/Caravan_wqms_site_info.csv")

#combine PFOA and PFOS by column names. They have the same column names so this should be easy
#the raw datasets are in micrograms per liter. Convert to nanograms per liter
Caravan_PFOA <- Caravan_PFOA_Raw %>%
  rename(PFOA = obs) %>% #change obs column to coresponding compound
  rename(`Sample Date (MM/DD/YYY)` = dates) %>%
  mutate(PFOA = PFOA* 1000) %>% #converts ug/L column to ng/L by simple multiplying
  select(-c(unit, variable)) #i don't care about the unit or variable columns, so i'm removing them

Caravan_PFOS <- Caravan_PFOS_Raw %>%
  rename(PFOS = obs) %>%
  rename(`Sample Date (MM/DD/YYY)` = dates) %>%
  mutate(PFOS = PFOS* 1000) %>% #converts ug/L column to ng/L by simple multiplying
  select(-c(unit, variable))

Caravan_PFAS = bind_rows(Caravan_PFOA, Caravan_PFOS)

#this next step might be unnescesary for a model.
#PFOA and PFOS are in seperate rows, but i have their site id
#and sample date. I'm going to combine PFOA and PFOS so we know both values 
#across a single sampling event. This may have introduced error if 
#multiple samples were collected at a site within a single day, which is definitely possible

Caravan_PFAS <- Caravan_PFAS %>%
  group_by(wqms_id, `Sample Date (MM/DD/YYY)`) %>%
  summarize(
    PFOA = sum(PFOA, na.rm = TRUE),
    PFOS = sum(PFOS, na.rm = TRUE),
    streamflow = sum(streamflow, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #Converts 0s back to NA if both original values were missing
  mutate(across(c(PFOA, PFOS, streamflow), ~na_if(., 0)))

#combine site coordinate information with PFAS concentrations
Caravan_PFAS <- left_join(Caravan_PFAS, site_info, by = "wqms_id")


#i'm going to rename columns to be consistent with other datasets
Caravan_PFAS <- Caravan_PFAS %>%
  rename(Longitude = wqms_lon) %>%
  rename(Latitude = wqms_lat) %>%
  rename(Streamflow = streamflow) %>%
  rename(`Sample Name` = wqms_id) %>%
  mutate(`Article (Author et al YYYY)` = "Caravan_2025") %>%
  mutate(`Sample Type` = "Surface Water") %>%
  #I also want to bring in all of the data from Caravan_PFAS, but it has a lot of info that i don't really need at the moment
  # cutting down on some of the columns in Caravan_PFAS
  select(c("Sample Name", "Sample Date (MM/DD/YYY)", "Sample Type", "Article (Author et al YYYY)", Longitude, Latitude, PFOA, PFOS, Streamflow))

#convert to SF object for plotting
Caravan_PFAS_sf= st_as_sf(Caravan_PFAS,
                          coords = c("Longitude", "Latitude"),  # x = Long, y = Lat
                          crs = 4326) 

#plot them on a world map
#world <- ne_countries(scale = "large", returnclass = "sf")
rivers <- ne_download(scale = 10, 
                      type = 'rivers_lake_centerlines', 
                      category = 'physical', 
                      returnclass = "sf")
#update the coordinate system to match the rivers layer
Caravan_PFAS_sf = st_transform(Caravan_PFAS_sf, st_crs(rivers))

ggplot(data = rivers) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = Caravan_PFAS_sf %>% filter(!is.na(PFOA)), color = "red", size = 2, shape = 19) +
  geom_sf(data = Caravan_PFAS_sf %>% filter(!is.na(PFOS)), add=TRUE, color = "blue", size = 2, shape = 19) +
  ggtitle("Global Map of PFOA and PFAS") +
  theme_classic()