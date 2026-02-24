

#This script is part of the global PFAS modeling project
#Created February 2, 2026 by RV
#Last edited: February 24, 2026 by RV
#Edits will be uploaded to Github for easy access

#working with global caravan data. 
#this data comes from caravan-Qual lite. Released December 11, 2025
#link to lite dataset: https://zenodo.org/records/17787066 

#Question. where did they get this data, and where did all the good stones go?

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
Caravan_PFOA_Raw <- read_csv("https://raw.githubusercontent.com/rvera177/MillRiver_PFAS/refs/heads/main/data/Caravan_PFOA.csv")
Caravan_PFOS_Raw = read_csv("https://raw.githubusercontent.com/rvera177/MillRiver_PFAS/refs/heads/main/data/Caravan_PFOS.csv")
site_info = read_csv("https://raw.githubusercontent.com/rvera177/MillRiver_PFAS/refs/heads/main/data/Caravan_wqms_site_info.csv")

#combine PFOA and PFOS by column names. They have the same column names so this should be easy
Caravan_PFAS_Raw = rbind(Caravan_PFOA_Raw,Caravan_PFOS_Raw)

#the raw datasets are in micrograms per liter. Convert to nanograms per liter
Caravan_PFAS <- Caravan_PFAS_Raw %>% 
  mutate(obs = obs* 1000) #converts ug/L column to ng/L by simple multiplying
  #make sure you don't run this twice

#combine site coordinate information with PFAS concentrations
Caravan_PFAS <- left_join(Caravan_PFAS, site_info, by = "wqms_id")

#convert to SF object for plotting
Caravan_PFAS= st_as_sf(Caravan_PFAS,
                  coords = c("wqms_lon", "wqms_lat"),  # x = Long, y = Lat
                  crs = 4326) 

#plot them on a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#update the coordinate system to match the world map
Caravan_PFAS = st_transform(Caravan_PFAS, st_crs(world))

ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = Caravan_PFAS[Caravan_PFAS$variable == "PFOS",], color = "red", size = 2, shape = 19) +
  geom_sf(data = Caravan_PFAS[Caravan_PFAS$variable == "PFOA",], add=TRUE, color = "blue", size = 2, shape = 19) +
  ggtitle("Global Map of PFOA and PFAS") +
  theme_classic()

#Now that i have this large Caravan dataset plotted,
#I want to bring in the papers our group has been
#extracting data from

Camacho_2024 = read.csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Camacho_et_al_2024_Florida.csv")
Sims_2025 = read.csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sims_et_al_2025_%20Western_United_States.csv")

All_PFAS = bind_rows(Camacho_2024, Sims_2025)

#ggplot of an sf object won't work if their are NA's in the lat and long
All_PFAS_sf <- All_PFAS %>%#remove NA's now
  filter(!is.na(Longitude..Decimal.), !is.na(Latitude..Decimal.))
#convert to SF object for plotting
All_PFAS_sf= st_as_sf(All_PFAS_sf,
                       coords = c("Longitude..Decimal.", "Latitude..Decimal."),  # x = Long, y = Lat
                       crs = 4326) 

#update the coordinate system to match the world map
All_PFAS_sf = st_transform(All_PFAS_sf, st_crs(world))

#issue is probably NA's or non_numerics in the PFOS and PFOA columns
ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = All_PFAS_sf[All_PFAS_sf$variable == "PFOS",], color = "red", size = 2, shape = 19) +
  geom_sf(data = All_PFAS_sf[All_PFAS_sf$variable == "PFOA",], add=TRUE, color = "blue", size = 2, shape = 19) +
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
  addCircleMarkers(data = Caravan_PFAS[Caravan_PFAS$variable == "PFOS",],
                   color = "red", fillColor = "red",
                   radius = 5, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   group = "PFOS",
                   clusterOptions = markerClusterOptions()) %>%
  # PFOS points
  addCircleMarkers(data = Caravan_PFAS[Caravan_PFAS$variable == "PFOA",],
                   color = "blue", fillColor = "blue",
                   radius = 5, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   group = "PFOA",
                   clusterOptions = markerClusterOptions()) %>%
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
