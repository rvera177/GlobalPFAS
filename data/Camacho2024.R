library(readr)
Camacho_coords = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/CamachoEtAl2024_SiteCoords.csv")
Camacho_PFAS_DATA = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/Camacho2024_Tab7_PFASData.csv")

library(tidyverse)
#combine the coords of each Sample_ID to the corresponding PFAS data using Left_join
Camacho <- left_join(Camacho_coords, Camacho_PFAS_DATA, by ="Sample_ID") 

#now that thats done, I want to Turn the empty NA cells 
# into 0, which is a Non-Detect. This is because the location
# was tested for a compound, but didn't find a reportable level of it

#i do this with a dplyr pipe, but their are multiple ways of doing this
library(dplyr)
Camacho <- Camacho %>% 
  mutate(across(everything(), ~replace_na(as.character(.), "0")))

head(Camacho)
#okay so there is an issue where PFAS are showing as character. instead of characters
#i want to turn only the PFAS columns to numeric values, so I'm mutating everything 
#EXCEPT Sample_ID and Florida_County_Name
Camacho <- Camacho %>%
  mutate(across(-c(Sample_ID, Florida_County_Name), as.numeric))
#i like how the table is set up now, so I'm going to save as a csv.



#plotting
#convert to SF object for plotting
library(sf)
Florida_PFAS= st_as_sf(Camacho,
               coords = c("Longitude", "Latitude"),  # x = Long, y = Lat
               crs = 4326) 

plot(st_geometry(Florida_PFAS), col = "blue")


#I'm going to map PFOA, so we can see the range of concentrations spatially
#I like the wesanderson colour palletes for my maps
library(wesanderson)
pallete <- colorRampPalette(wes_palette("Zissou1", type = "continuous"))

ggplot(data = Florida_PFAS) +
  geom_sf(data = Florida_PFAS,
          aes(fill = PFOA), #you can change fill to be any column inside of Florida_PFAS
          shape = 21,
          color = "black",
          size = 2, stroke = 0.5,
          show.legend = TRUE) + 
  scale_fill_gradientn(trans = "log10", #using log10 so i can see variability better
                       colors = pallete(100),
                       na.value = "white",
                       name = "log(PFOA)") +
  theme_classic()




#i'm saving the data as a geopackage so i can make figures in ArcGIS
st_write(Florida_PFAS, "Camacho.gpkg", delete_dsn = TRUE, overwrite=TRUE)
