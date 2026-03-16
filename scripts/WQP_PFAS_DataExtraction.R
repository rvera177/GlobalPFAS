

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
  statecode = "Illinois",
  countycode = "DeWitt",
  characteristicName = "Perfluorooctanoic acid"
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
    ActivityTypeCode == "Sample-Routine"
  )


#okay so that worked great. Now i'm going to expand this too all US states
# just a single compound for now. 
pfas_names <- c("Perfluorooctanoic acid")
state_list <- wqp_state_codes$value
pfas_data_list <- list()
failed_states <- c() # Keep track of errors
sampletype <- c("Stream", "Estuary", "Lake, Reservoir, Impoundment", 
                "Spring", "Facility", "Wetland", "Ocean")
for (st in state_list) {
  message("Fetching data for: ", st)
  
  Sys.sleep(2) 
  
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
          ActivityTypeCode == "Sample-Routine"
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

#okay so i only care about a few columns. not everything here. 

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
