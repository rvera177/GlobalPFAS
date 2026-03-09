

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


#I want to bring in the papers our group has been extracting data from

Caravan_PFAS = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Caravan_PFAS.csv")
Camacho_2024 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Camacho_et_al_2024_Florida.csv")
Sims_2025 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sims_et_al_2025_%20Western_United_States.csv")
NH_DES_2026 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/NewHampshire_DES_PFAS_Data_Dump.csv")
Ahrens_2023 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Ahrens_et_al_2023_Arctic.csv")
Breitmeyer_2023 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Breitmeyer_et_al_2023_Pennsylvania.csv")
Sharma_2016 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sharma_et_al_2016_Ganges_River.csv")
Zhang_2016 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Zhang_et_al_2016_RI_NY.csv")
Scott_2009 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Scott_et_al_2009_Canada.csv")
Goodrow_2020 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Goodrow_et_al_2020_New_Jersey.csv")
Bai_Son_2021 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Bai_and_Son_2021_Renoe_LasVegas.csv")

All_PFAS = bind_rows(Camacho_2024, Sims_2025, 
      NH_DES_2026, Breitmeyer_2023, Ahrens_2023, Sharma_2016, 
      Caravan_PFAS, Zhang_2016, Scott_2009, Goodrow_2020, Bai_Son_2021)
All_PFAS_SW <- subset(All_PFAS, `Sample Type` == "Surface Water")

#ggplot of an sf object won't work if their are NA's in the lat and long
All_PFAS_sf <- All_PFAS_SW %>% #remove NA's and make numeric. if not numeric, turns into an NA
  mutate(across(-all_of(c("Article (Author et al YYYY)", "Sample Name", "Sample Type",
      "Sample Date (MM/DD/YYY)", "Sample Time", "Analysis Method")), ~ as.numeric(.))) %>%
  filter(!is.na(`Latitude`), !is.na(`Longitude`))
#convert to SF object for plotting
All_PFAS_sf= st_as_sf(All_PFAS_sf,
                       coords = c("Longitude", "Latitude"),  # x = Long, y = Lat
                       crs = 4326) 

#updating the coordinate system to match the world map
All_PFAS_sf = st_transform(All_PFAS_sf, st_crs(rivers))

ggplot(data = rivers) +
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


All_PFAS_SW$popup <- make_popup(All_PFAS_SW)

#build the leaflet map
RV_Teja_map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = rivers,
              color = "blue", weight = 1,
              group = "rivers",
              popup = ~name) %>%
  # PFOA points
  addCircleMarkers(data = All_PFAS_SW %>% filter(!is.na(PFOS)),
                   color = "red", fillColor = "red",
                   radius = 5, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   #clusterOptions = markerClusterOptions(),
                   group = "PFOS") %>%
  # PFOS points
  addCircleMarkers(data = All_PFAS_SW %>% filter(!is.na(PFOA)),
                   color = "blue", fillColor = "blue",
                   radius = 3, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   #clusterOptions = markerClusterOptions(),
                   group = "PFOA") %>%
  addLayersControl(overlayGroups = c("rivers", "PFOS", "PFOA"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "topright",
            colors = c("red", "blue"),
            labels = c("PFOS", "PFOA"),
            title = "Compounds")

# show map
RV_Teja_map


# save to an HTML file if you want
# saveWidget(RV_Teja_map, "RV_Teja_map.html", selfcontained = TRUE)

