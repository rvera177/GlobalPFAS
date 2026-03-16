

#Helo
#this is the set up for retrieving Water Quality data data from
# the Water Quality Portal. https://www.waterqualitydata.us
#I'm 


library(dataRetrieval)
library(dplyr)

DeWitt <- readWQPdata(
  statecode = "Illinois",
  countycode = "DeWitt",
  characteristicName = "Nitrogen"
)


DeWitt <- readWQPdata(
  statecode = "Minnesota",
  characteristicName = "Perfluorovaleric acid"
)

#testing on a single location at first for a single compound, PFOA

#i'm bringing in the official State naming conventions form the WQP site
wqp_states_url <- "https://www.waterqualitydata.us/Codes/statecode?mimeType=json"
wqp_codes_raw <- fromJSON(wqp_states_url)
head(wqp_codes_raw)
wqp_state_codes <- wqp_codes_raw$codes #only keep the code
head(wqp_state_codes)


pfas <- c("Perfluorooctanoic acid")
test <- readWQPdata(
  statecode = "US:04", # this is a test for arizona
  characteristicName = pfas, #PFOA
  sampleMedia = "Water",
  siteType = "Stream")

test_coords <- whatWQPsites(
  statecode = "US:04", #Arizona code
  characteristicName = pfas,
  sampleMedia = "Water",
  siteType = "Stream"
) %>%
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)%>%
  distinct() #gets the lat and long for each location, and keeps sites that
#were measured multiple times.

test <- test %>%
  left_join(test_coords, by= "MonitoringLocationIdentifier")

test <- test %>%
  filter(
    ResultValueTypeName == "Actual",
    `ResultMeasure.MeasureUnitCode` == "ng/L",
    ResultSampleFractionText == "Total",
    ActivityTypeCode == "Sample-Routine", "Quality Control Sample-Field Replicate", "Quality Control Sample-Lab Duplicate"
  )


#okay so that worked great. Now i'm going to expand this too all US states
# just a single compound for now. 
pfas_names <- c(
  "Perfluorooctanoic acid",
  "Perfluorovaleric acid",
  "Perfluorooctanesulfonamide",
  "Perfluorobutanoate",
  "Perfluorooctanesulfonate",
  "Perfluorononanoic acid",
  "Perfluoro-1-nonanesulfonic acid",
  "Perfluorodecanoic acid",
  "Perfluorodecanoate",
  "Perfluorohexanesulfonate",
  "Perfluorohexanesulfonic acid",
  "Perfluoroheptanoic acid",
  "Perfluoroheptanoate",
  "Perfluorododecanoic acid",
  "Perfluorobutanesulfonate",
  "Perfluorohexanoic acid",
  "Perfluorohexanoate",
  "3:3 Fluorotelomer carboxylic acid",
  "Octanoic acid, 4,4,5,5,6,6,7,7,8,8,8-undecafluoro-",
  "Decanoic acid, 4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-pentadecafluoro-",
  "Fluorotelomer sulfonate 4:2",
  "6:2 Fluorotelomer sulfonate acid",
  "Fluorotelomer sulfonate 8:2",
  "N-Ethylperfluorooctanesulfonamidoethanol",
  "Sulfluramid",
  "Perfluoroundecanoic acid",
  "N-methyl perfluorooctanesulfonamidoacetic acid",
  "N-methylperfluoro-1-octanesulfonamide",
  "2-(N-Ethyl-perfluorooctanesulfonamido)acetate",
  "Perfluoro(2-ethoxyethane)sulfonic acid",
  "4,8-dioxa-3H-perfluorononanoate",
  "4,8-Dioxa-3H-perfluorononanoic acid",
  "Perfluoro-3-methoxypropanoic acid",
  "Perfluoro(4-methoxybutanoic) acid",
  "Hexafluoropropylene oxide dimer acid",
  "Perfluorotridecanoic acid",
  "Perfluoro-3,6-dioxaheptanoic acid",
  "1-Heptanesulfonic acid, 1,1,2,2,3,3,4,4,5,5,6,6,7,7,7-pentadecafluoro-",
  "11-chloroeicosafluoro-3-oxaundecane-1-sulfonic acid",
  "9-Chlorohexadecafluoro-3-oxanone-1-sulfonic acid"
)

DeWitt <- readWQPdata(
  statecode = "New York",
  characteristicName = "Perfluorododecanoate
"
)

# c(
#   checked. Perfluorooctanoic acid = PFOA
#   checked. Perfluorovaleric acid = PFPeA
#   checked. Perfluorooctanesulfonamide = PFOSA
#   checked. Perfluorobutanoate = PFBA
#   checked. Perfluorooctanesulfonate = PFOS
#   checked. Perfluorononanoic acid = PFNA
#   checked. "Perfluoro-1-nonanesulfonic acid" = PFNS
#   
#   checked. Perfluorodecanoic acid = PFDA
#   checked. Perfluorodecanoate= PFDA
#   
#   checked. Perfluorohexanesulfonate = PFHxS
#   checked. Perfluorohexanesulfonic acid = PFHxS "both"
#   
#   checked. Perfluoroheptanoic acid = PFHpA
#   checked. Perfluoroheptanoate = PFHpA
#   
#   checked. Perfluorododecanoic acid = PFDoA
#   checked. Perfluorobutanesulfonate = PFBS
#   
#   checked. Perfluorohexanoic acid = PFHxA
#   checked. Perfluorohexanoate = PFHxA
#   
#   checked. '3:3 Fluorotelomer carboxylic acid' = 3:3 FTCA
#   checked. "Octanoic acid, 4,4,5,5,6,6,7,7,8,8,8-undecafluoro-" = 5:3 FTCA
#   checked. 'Decanoic acid, 4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-pentadecafluoro-' = 7:3 FTCA
#   
#   checked. "Fluorotelomer sulfonate 4:2" = 4:2 FTS
#   checked. "6:2 Fluorotelomer sulfonate acid" = 6:2 FTS
#   checked. "Fluorotelomer sulfonate 8:2" = 8:2 FTS
#   checked. N-Ethylperfluorooctanesulfonamidoethanol = N-EtFOSE
#   checked. Sulfluramid = N-EtFOSA
#   checked. Perfluoroundecanoic acid = PFUnA
#   checked. N-methyl perfluorooctanesulfonamidoacetic acid = MeFOSAA
#   checked. N-methylperfluoro-1-octanesulfonamide = N-MeFOSA
#   checked. '2-(N-Ethyl-perfluorooctanesulfonamido)acetate' = N-EtFOSAA
#   
#   checked. "Perfluoro(2-ethoxyethane)sulfonic acid" = PFEESA
#   
#   checked. '4,8-dioxa-3H-perfluorononanoate' = ADONA
#   checked. "4,8-Dioxa-3H-perfluorononanoic acid" = ADONA
#   
#   checked. "Perfluoro-3-methoxypropanoic acid" = PFMPA
#   checked. "Perfluoro(4-methoxybutanoic) acid" = PFMBA
#   checked. "Hexafluoropropylene oxide dimer acid" =  HFPO-DA
#   checked. Perfluorotridecanoic acid =  PFTrDA
#   checked. "Perfluoro-3,6-dioxaheptanoic acid" = NFDHA
#   checked. "1-Heptanesulfonic acid, 1,1,2,2,3,3,4,4,5,5,6,6,7,7,7-pentadecafluoro-" = PFHpS
#   
#   checked. "11-chloroeicosafluoro-3-oxaundecane-1-sulfonic acid" = 11Cl-PF3OUdS
#   checked. "9-Chlorohexadecafluoro-3-oxanone-1-sulfonic acid" = 9Cl-PF3ONS
# )


state_list <- wqp_state_codes$value
pfas_data_list <- list()
failed_states <- c() # Keep track of errors
sampletype <- c("Stream", "Estuary", "Lake, Reservoir, Impoundment", 
                "Spring", "Facility", "Wetland", "Ocean")
activity_types <- c(
  "Sample-Routine","Quality Control Sample-Field Replicate",
  "Quality Control Sample-Lab Duplicate")
#this takes about 5-10 minutes since i'm running for multiple compounds
#run for loop, Grab a coffee and stretch.
for (st in state_list) {
  message("Fetching data for: ", st)
  
  Sys.sleep(4) 
  
  result <- tryCatch({
    raw_data <- readWQPdata(
      statecode = st, 
      characteristicName = pfas_names,
      sampleMedia = "Water",
      siteType = sampletype
    )
    
    if (nrow(raw_data) > 0) {
      site_coords <- whatWQPsites(
        statecode = st, 
        characteristicName = pfas_names,
        sampleMedia = "Water",
        siteType = sampletype
      ) %>%
        select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>%
        distinct()
      
      processed_data <- raw_data %>%
        left_join(site_coords, by = "MonitoringLocationIdentifier") %>%
        # Logic: Keep rows that are ng/L OR are explicitly labeled "Not Detected"
        filter(
          (`ResultMeasure.MeasureUnitCode` == "ng/L" | ResultDetectionConditionText == "Not Detected"),
          ResultSampleFractionText == "Total",
          ActivityTypeCode %in% activity_types
        )
      
      if (nrow(processed_data) > 0) {
        pfas_data_list[[st]] <- processed_data
      }
    }
    "success"
  }, error = function(e) {
    message("Error in state ", st, ": ", e$message)
    return(st) # Record the failed state
  })
  
  if (result != "success") {
    failed_states <- c(failed_states, result)
  }
}

all_WQP_data <- pfas_data_list %>%
  map_dfr(~mutate(.x, across(everything(), as.character)))


all_WQP_data <-all_WQP_data %>%
  mutate(
    ResultMeasureValue = case_when(
      ResultDetectionConditionText == "Not Detected" ~ "ND",
      TRUE ~ as.character(ResultMeasureValue))) %>%
  mutate(ResultMeasureValue = ifelse(ResultMeasureValue == "ND", "0", ResultMeasureValue)) %>%
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue))


#okay so i only care about a few columns. not everything here. 
all_WQP_data <-all_WQP_data %>%
  select(
    MonitoringLocationIdentifier,
    LatitudeMeasure,
    LongitudeMeasure,
    ActivityStartDate,  
    `ResultAnalyticalMethod.MethodIdentifier`,
    OrganizationIdentifier,
    CharacteristicName,
    ResultMeasureValue,                
  )


# okay so now i'm turning results into columns. 
pfas_map <- tibble(
  CharacteristicName = pfas_names,
  ShortName <- c(
    "PFOA", "PFPeA", "PFOSA", "PFBA", "PFOS", "PFNA", "PFNS",
    "PFDA", "PFDA",          # both forms map to PFDA
    "PFHxS", "PFHxS",        # both map to PFHxS
    "PFHpA", "PFHpA",
    "PFDoA", "PFBS",
    "PFHxA", "PFHxA",
    "3:3 FTCA", "5:3 FTCA", "7:3 FTCA",
    "4:2 FTS", "6:2 FTS", "8:2 FTS",
    "N-EtFOSE", "N-EtFOSA", "PFUnA",
    "MeFOSAA", "N-MeFOSA", "N-EtFOSAA",
    "PFEESA",
    "ADONA", "ADONA",       # both spellings map to ADONA
    "PFMPA", "PFMBA", "HFPO-DA",
    "PFTrDA", "NFDHA", "PFHpS",
    "11Cl-PF3OUdS", "9Cl-PF3ONS"
  )
)

# left_join mapping. If CharacteristicName variations exist, consider fuzzy matching or using str_detect patterns.
all_WQP_data <- all_WQP_data %>%
  left_join(pfas_map, by = "CharacteristicName")
key_cols <- c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure",
              "ActivityStartDate", "OrganizationIdentifier", "ActivityIdentifier")
wide_pfas <- all_WQP_data %>%
  filter(!is.na(ShortName)) %>%                 # only compounds we mapped
  select(any_of(c(key_cols, "ShortName", "ResultMeasureValue"))) %>%
  group_by(across(any_of(key_cols)), ShortName) %>%
  summarize(value = mean(as.numeric(ResultMeasureValue), na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = ShortName,
    values_from = value,
    values_fill = NA_real_)




#plotting what we got
remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
# Load U.S. state boundaries
USA <- ne_states(country = "united states of america", returnclass = "sf")

# Clipping data to whats inside of the usa
all_WQP_sf <- st_as_sf(all_WQP_data, coords = c("LongitudeMeasure", "LatitudeMeasure"),crs = st_crs(4326))

WQP_PFAS <- st_join(all_WQP_sf, USA, join = st_intersects, left = FALSE)
#okay so i have 4417 data points in the united staes
ggplot(data = USA) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = WQP_PFAS, color = "red", size = 2, shape = 19, na.rm=TRUE) +
  ggtitle("United States Map of PFOA") +
  theme_classic()

# crop in lon/lat
conus_bbox <- st_bbox(c(xmin = -125, xmax = -66, ymin = 24, ymax = 50), crs = st_crs(4326))
USA <- st_crop(USA, conus_bbox)
WQP_PFAS <- st_crop(WQP_PFAS, conus_bbox)

ggplot() +
  geom_sf(data = USA, fill = "white", color = "gray2") +
  geom_sf(data = WQP_PFAS , color = "black",fill  = "red",  size = 2, stroke = 0.5,  shape = 21, na.rm = TRUE) +
  theme_classic()
