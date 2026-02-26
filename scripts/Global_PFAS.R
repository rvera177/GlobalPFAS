

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
  select(-c(unit, variable)) #i don't care about unit or variable, so i'm removing them

Caravan_PFOS <- Caravan_PFOS_Raw %>%
  rename(PFOS = obs) %>%
  rename(`Sample Date (MM/DD/YYY)` = dates) %>%
  mutate(PFOS = PFOS* 1000) %>% #converts ug/L column to ng/L by simple multiplying
  select(-c(unit, variable))

Caravan_PFAS = bind_rows(Caravan_PFOA, Caravan_PFOS)

Caravan_PFAS <- Caravan_PFAS %>%
  group_by(wqms_id, `Sample Date (MM/DD/YYY)`) %>%
  summarize(
    PFOA = sum(PFOA, na.rm = TRUE),
    PFOS = sum(PFOS, na.rm = TRUE),
    streamflow = sum(streamflow, na.rm = TRUE),
    .groups = "drop"
  ) %>%
 # Optional: Convert 0s back to NA if both original values were missing
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
  select(c("Sample Name", "Sample Date (MM/DD/YYY)", Longitude, Latitude, PFOA, PFOS, Streamflow))
  
#convert to SF object for plotting
Caravan_PFAS_sf= st_as_sf(Caravan_PFAS,
                  coords = c("Longitude", "Latitude"),  # x = Long, y = Lat
                  crs = 4326) 

#plot them on a world map
world <- ne_countries(scale = "large", returnclass = "sf")
rivers <- ne_download(scale = 10, 
                      type = 'rivers_lake_centerlines', 
                      category = 'physical', 
                      returnclass = "sf")
#update the coordinate system to match the world map
Caravan_PFAS_sf = st_transform(Caravan_PFAS_sf, st_crs(world))

ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = Caravan_PFAS_sf %>% filter(!is.na(PFOA)), color = "red", size = 2, shape = 19) +
  geom_sf(data = Caravan_PFAS_sf %>% filter(!is.na(PFOS)), add=TRUE, color = "blue", size = 2, shape = 19) +
  ggtitle("Global Map of PFOA and PFAS") +
  theme_classic()

#Now that i have this large Caravan dataset plotted,
#I want to bring in the papers our group has been
#extracting data from

Camacho_2024 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Camacho_et_al_2024_Florida.csv")
Sims_2025 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sims_et_al_2025_%20Western_United_States.csv")
NH_DES_2026 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/NewHampshire_DES_PFAS_Data_Dump.csv")
Ahrens_2023 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Ahrens_et_al_2023_Arctic.csv")
Breitmeyer_2023 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Breitmeyer_et_al_2023_Pennsylvania.csv")
Sharma_2016 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sharma_et_al_2016_Ganges_River.csv")
Zhang_2016 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Zhang_et_al_2016_RI_NY.csv")

All_PFAS = bind_rows(Camacho_2024, Sims_2025, 
      NH_DES_2026, Breitmeyer_2023, Ahrens_2023, Sharma_2016, Caravan_PFAS, Zhang_2016)

#ggplot of an sf object won't work if their are NA's in the lat and long
All_PFAS_sf <- All_PFAS %>% #remove NA's now
  mutate(across(-all_of(c("Article (Author et al YYYY)", "Sample Name", "Sample Type",
      "Sample Date (MM/DD/YYY)", "Sample Time", "Analysis Method")), ~ as.numeric(.))) %>%
  filter(!is.na(`Latitude`), !is.na(`Longitude`))
#convert to SF object for plotting
All_PFAS_sf= st_as_sf(All_PFAS_sf,
                       coords = c("Longitude", "Latitude"),  # x = Long, y = Lat
                       crs = 4326) 

#update the coordinate system to match the world map
All_PFAS_sf = st_transform(All_PFAS_sf, st_crs(world))

#issue is probably NA's or non_numerics in the PFOS and PFOA columns
ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = All_PFAS_sf %>% filter(!is.na(PFOA)), color = "red", size = 2, shape = 19, na.rm=TRUE) +
  ggtitle("Global Map of PFOA and PFAS") +
  theme_classic()


#everything above is my own code
#the following is leaflet code made with help from UMass Gen AI because I never made this before
library(leaflet)
library(htmlwidgets)

# build HTML popup strings from attributes (safe even if different columns)
make_popup <- function(sf_obj) {
  df <- sf::st_drop_geometry(sf_obj)
  if (ncol(df) == 0) return(rep("", nrow(df)))
  apply(df, 1, function(r) {
    paste0("<b>", names(df), "</b>: ", ifelse(is.na(r), "", r), collapse = "<br/>")
  })
}


All_PFAS$popup <- make_popup(All_PFAS)

#build the leaflet map
RV_Teja_map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # world polygon
  addPolygons(data = world,
              color = "#444444", weight = 1,
              fillColor = "lightgrey", fillOpacity = 0.5,
              group = "World",
              popup = ~name) %>%
  addPolylines(data = rivers,
              color = "blue", weight = 1,
              group = "World",
              popup = ~name) %>%
  # PFOA points
  addCircleMarkers(data = All_PFAS %>% filter(!is.na(PFOS)),
                   color = "red", fillColor = "red",
                   radius = 5, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   #clusterOptions = markerClusterOptions(),
                   group = "PFOS") %>%
  # PFOS points
  addCircleMarkers(data = All_PFAS %>% filter(!is.na(PFOA)),
                   color = "blue", fillColor = "blue",
                   radius = 3, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   #clusterOptions = markerClusterOptions(),
                   group = "PFOA") %>%
  addLayersControl(overlayGroups = c("World", "PFOS", "PFOA"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "topright",
            colors = c("red", "blue"),
            labels = c("PFOS", "PFOA"),
            title = "Compounds")

# show map
RV_Teja_map


#Caravan
Caravan_PFAS$popup <- make_popup(Caravan_PFAS)

#build the leaflet map
Caravan_PFOA_PFAS_map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # world polygon
  addPolygons(data = world,
              color = "#444444", weight = 1,
              fillColor = "lightgrey", fillOpacity = 0.5,
              group = "World",
              popup = ~name) %>%
  # PFOA points
  addCircleMarkers(data = Caravan_PFAS %>% filter(!is.na(PFOS)),
                   color = "red", fillColor = "red",
                   radius = 3, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   #clusterOptions = markerClusterOptions(),
                   group = "PFOS") %>%
  # PFOS points
  addCircleMarkers(data = Caravan_PFAS %>% filter(!is.na(PFOA)),
                   color = "blue", fillColor = "blue",
                   radius = 3, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   #clusterOptions = markerClusterOptions(),
                   group = "PFOA") %>%
  addLayersControl(overlayGroups = c("World", "PFOS", "PFOA"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "topright",
            colors = c("red", "blue"),
            labels = c("PFOS", "PFOA"),
            title = "Compounds")

# show map
Caravan_PFOA_PFAS_map


# (optional) save to an HTML file
# saveWidget(Caravan_PFOA_PFAS_map, "Caravan_PFOA_PFAS_map.html", selfcontained = TRUE)

