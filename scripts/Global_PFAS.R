

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
library(dplyr) #need this for pipes

#I want to bring in the papers our group has been extracting data from

Caravan_PFAS = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Caravan_PFAS_2026.csv")
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
Teymoorian_2021 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Teymoorian_2025_Montreal.csv")
Maine_DEP_2026 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/MaineDEP_2026_Datadump_cleaned.csv")
WQP_USA_2026 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/WQP_USA_Data_complete.csv")
Hayworth_2022 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Hayworth_et_al_2022_Alabama_cleaned.csv")
Dunn_2023 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Dunn_et_al_2023_RhodeIsland_complete.csv")
AustraliaMap_2026 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Australia_Government_PFAS_CHEM_MAP_Clean.csv")
Forster_2024 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Forster_et_al_2024_SouthCarolina_cleaned.csv")
Penland_2020 = read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Penland_2020_SC_NC_cleaned.csv")

All_PFAS = bind_rows(Camacho_2024, Sims_2025, NH_DES_2026, 
          Breitmeyer_2023, Ahrens_2023, Sharma_2016, Caravan_PFAS,
          Zhang_2016, Scott_2009, Goodrow_2020, Bai_Son_2021, 
          Teymoorian_2021, Maine_DEP_2026, WQP_USA_2026, 
          Hayworth_2022, Dunn_2023, AustraliaMap_2026, Forster_2024, Penland_2020)

All_PFAS_SW <- subset(All_PFAS, `Sample Type` == "Surface Water")

#this is the total number of sites
All_PFAS_SW %>%
  distinct(Latitude, Longitude) %>%
  nrow()

#ggplot of an sf object won't work if their are NA's in the lat and long
All_PFAS_sf <- All_PFAS_SW %>% #remove NA's and make numeric. if not numeric, turns into an NA
  mutate(across(-all_of(c("Article (Author et al YYYY)", "Sample Name", "Sample Type",
      "Sample Date (MM/DD/YYY)", "Sample Time", "Analysis Method")), ~ as.numeric(.))) %>%
  filter(!is.na(`Latitude`), !is.na(`Longitude`))
#convert to SF object for plotting
All_PFAS_sf= st_as_sf(All_PFAS_sf,
                       coords = c("Longitude", "Latitude"),  # x = Long, y = Lat
                       crs = 4326) 


#everything above is my own code
#the following is leaflet code made with help from UMass Gen AI because I never made this before
library(leaflet)
library(htmlwidgets)

# Build popup only from data columns
make_popup <- function(df) {
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  
  df_chr <- as.data.frame(lapply(df, function(x) {
    ifelse(is.na(x), "", as.character(x))
  }), check.names = FALSE)
  
  apply(df_chr, 1, function(r) {
    paste0("<b>", names(df_chr), "</b>: ", r, collapse = "<br/>")
  })
}
All_PFAS_SW <- All_PFAS_SW %>% select(-any_of("popup"))
All_PFAS_SW$popup <- make_popup(All_PFAS_SW)
# Build the leaflet map
RV_Teja_map <- leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data = All_PFAS_SW %>% filter(!is.na(PFOA)),
                   lng = ~Longitude,   # ← added
                   lat = ~Latitude,    # ← added
                   color = "red", fillColor = "red",
                   radius = 5, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   group = "PFOA") %>%
  addCircleMarkers(data = All_PFAS_SW %>% filter(!is.na(PFOS)),
                   lng = ~Longitude,   # ← added
                   lat = ~Latitude,    # ← added
                   color = "blue", fillColor = "blue",
                   radius = 3, stroke = FALSE, fillOpacity = 0.9,
                   popup = ~popup,
                   group = "PFOS") %>%
  addLayersControl(overlayGroups = c("PFOA", "PFOS"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "topright",
            colors = c("red", "blue"),
            labels = c("PFOA", "PFOS"),
            title = "Compounds")

RV_Teja_map


#okay so now i want to start experimenting with an ML model
#starting off with just the united states, so i'm going to bound
#my data to only points in the US
remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
# Load U.S. state boundaries

USA_states <- ne_states(country = "united states of america", returnclass = "sf")

# Clipping data to whats inside of the usa
US_PFAS <- st_join(All_PFAS_sf, USA_states, join = st_intersects, left = FALSE)
#okay so i have 5470 data points in the united states
ggplot(data = USA_states) +
  geom_sf(fill = "gray95", color = "gray20") +
  geom_sf(data = US_PFAS %>% filter(!is.na(PFOS)), color = "red", size = 2, shape = 19, na.rm=TRUE) +
  ggtitle("United States Map of PFOA") +
  theme_classic()

# crop in lon/lat
conus_bbox <- st_as_sfc(st_bbox(c(xmin = -125, xmax = -66, ymin = 22, ymax = 51), crs = st_crs(4326)))
USA_states_crop <- st_crop(USA_states, conus_bbox)
US_PFAS_crop <- st_crop(US_PFAS, conus_bbox)

ggplot() +
  geom_sf(data = USA_states_crop, fill = "white", color = "gray2") +
  geom_sf(data = US_PFAS_crop %>% filter(!is.na(PFOA)), color = "black",fill  = "red",  size = 2, stroke = 0.5,  shape = 21, na.rm = TRUE) +
  theme_classic()

#only looking at New england huc2
library(nhdplusTools)
library(dataRetrieval)

# Get all HUC2 boundaries directly
huc2 <- st_read("C:/Users/Marston User/Documents/Global Papers/HUC_Boundaries/Watershed_Boundary_Dataset_HUC_2s_Source_Resilience_Climate_GovDataset/HU02.shp")


huc2 %>% st_drop_geometry() %>% select(huc2, name)

# Filter HUC2 to New England only (huc2 == "01")
ne_huc2 <- huc2 %>%
  filter(huc2 == "01")

# Match CRS to your PFAS sf object
ne_huc2 <- st_transform(ne_huc2, st_crs(All_PFAS_sf))

# Clip PFAS observations to New England
sf_use_s2(FALSE)
NE_PFAS <- st_intersection(All_PFAS_sf, ne_huc2)
sf_use_s2(TRUE)

# Quick check
cat("New England observations:", nrow(NE_PFAS), "\n")
cat("Unique sites:", NE_PFAS %>% distinct(geometry) %>% nrow(), "\n")

# Plot to verify
ggplot() +
  geom_sf(data = ne_huc2, fill = "gray95", color = "gray20") +
  geom_sf(data = NE_PFAS %>% filter(!is.na(PFOS)), 
          color = "black", fill = "red",
          size = 2, stroke = 0.5, shape = 21, na.rm = TRUE) +
  ggtitle("New England PFAS observations (HUC2 = 01)") +
  theme_classic()

#----------getting the comids for all my sites------------------------
library(StreamCatTools)
library(nhdplusTools)
# okay so now i am making the ML model for all of new england
# i am going to be using streamCat data. To do this, 
# I need the comids of each location with an observation. 
# I need to do this in chunks, because if i try to do all observations in NE_PFAS
# in one go, it will crash, or the website won't work.
n <- nrow(NE_PFAS)
chunk_size <- 100
chunks <- split(seq_len(n), ceiling(seq_len(n)/chunk_size))

# prepare COMID column if not present
if (!("COMID" %in% names(NE_PFAS))) NE_PFAS$COMID <- NA_integer_

get_one_comid_sf <- function(i, max_tries = 5, base_sleep = 0.5) {
  pt <- NE_PFAS[i, ]    # single-row sf (works in your session)
  for (t in seq_len(max_tries)) {
    Sys.sleep(base_sleep * t)            # backoff
    res <- tryCatch(nhdplusTools::discover_nhdplus_id(pt),
                    error = function(e) e)
    if (inherits(res, "error")) {
      message(sprintf("idx %d attempt %d error: %s", i, t, res$message))
      next
    }
    if (!is.null(res) && length(res) > 0) {
      if (is.atomic(res)) return(as.integer(res[[1]]))
      if (is.data.frame(res) && "comid" %in% names(res)) return(as.integer(res$comid[1]))
      if (is.list(res) && !is.null(res$comid)) return(as.integer(res$comid))
    } else {
      message(sprintf("idx %d attempt %d returned empty", i, t))
    }
  }
  NA_integer_
}

# Main loop with save/resume behavior
for (ci in seq_along(chunks)) {
  idx <- chunks[[ci]]
  # skip chunk if file already saved (resume)
  chunk_file <- sprintf("comid_chunk_%03d.rds", ci)
  if (file.exists(chunk_file)) {
    message("Skipping chunk ", ci, " (found ", chunk_file, ")")
    # if you want, load and assign from file here
    saved <- readRDS(chunk_file)
    NE_PFAS$COMID[saved$idx] <- saved$comids
    next
  }
  message(sprintf("Processing chunk %d/%d (rows %d-%d)", ci, length(chunks), min(idx), max(idx)))
  comids_chunk <- NE_PFAS$COMID[idx]
  
  for (j in seq_along(idx)) {
    i <- idx[j]
    # skip already-populated entries (safe resume)
    if (!is.na(NE_PFAS$COMID[i])) next
    comids_chunk[j] <- get_one_comid_sf(i, max_tries = 4, base_sleep = 0.5)
    # small regular pause to stay polite
    if (j %% 25 == 0) Sys.sleep(0.3)
  }
  
  # assign back to main sf
  NE_PFAS$COMID[idx] <- comids_chunk
  
  # save chunk for resume
  saveRDS(list(idx = idx, comids = comids_chunk), chunk_file)
  saveRDS(NE_PFAS, "NE_PFAS_with_COMID_progress.rds")  # optional full save
  
  message(sprintf("Completed chunk %d — assigned so far: %d (NA: %d)",
                  ci, sum(!is.na(NE_PFAS$COMID)), sum(is.na(NE_PFAS$COMID))))
  Sys.sleep(1)
}

# final save
saveRDS(NE_PFAS, file = "NE_PFAS_with_COMID_final.rds")
message("Done. Total assigned COMIDs: ", sum(!is.na(NE_PFAS$COMID)), "/", n)
write.csv(NE_PFAS, "NE_PFAS_with_COMIDS.csv") #saves the COMIDS as a csv


# ensure COMID numeric and remove NA
NE_PFAS$COMID <- as.integer(NE_PFAS$COMID)
#now the NE_PFAS data set has a column called COMID. I'll use this to get StreamCat data.
comids_all <- na.omit(NE_PFAS$COMID)
unique_comids <- unique(comids_all)

#okay ,so now i got comids for all my sites. 
#now i want to get all 

#you can pull in the US_PFAS data i was using if you want to run through my ML model.
#US_PFAS <- read_csv("https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/US_PFAS_with_COMIDS.csv")

#these are all the metrics in StreamCat
SC_Metrics = sc_get_metric_names(category = NULL, aoi = NULL, year = NULL, dataset = NULL)
SC_Metrics
#these are the name of all streamcat metrics.
#168 metrics, with different AOI and Year combinations

#this is an example of wat we want to do with the data
# It is still a work in progress. 


# StreamCat example; Define the list of COMIDs
example_comids <- NE_PFAS$COMID

library(StreamCatTools)
# Pull the data using the sc_get_data function
df <- sc_get_data(
  metric = 'PctUrbMd2006,DamDens',
  aoi = 'catchment,watershed',
  comid = paste(example_comids, collapse = ",")
)
# View the results
head(df)


library(tidyr)
library(stringr)
library(purrr)
library(data.table) 
 


#-----Stuck on this Section!--------------

#skip this and go to line 500 if you want to see the model for the US. 

# At this point, i was trying to get the code to return to me both Cat and Ws scale data. 
# i didn't figure it out though.
desired_aois <- c("Cat","Ws")
SCm <- SC_Metrics %>% rename_with(tolower)    # your tibble
sites_df <- NE_PFAS
out_dir <- "streamcat_chunks"
dir.create(out_dir, showWarnings = FALSE)
chunk_size <- 150

# expand AOI like "Cat, Ws" -> rows
SCm <- SCm %>%
  mutate(aoi = ifelse(is.na(aoi) | aoi == "", NA_character_, aoi)) %>%
  separate_rows(aoi, sep = ",") %>%
  mutate(aoi = str_trim(aoi))

# compute numeric year where possible
SCm <- SCm %>% mutate(year_num = suppressWarnings(as.numeric(year)))

# keep only desired AOIs
SCm2 <- SCm %>% filter(!is.na(aoi) & aoi %in% desired_aois)

# choose most recent numeric year per metric+aoi (as you requested)
metric_year_choice <- SCm2 %>%
  group_by(metric, aoi) %>%
  summarize(
    year = {
      yrs <- year_num[!is.na(year_num)]
      if (length(yrs) == 0) NA_integer_ else max(yrs)
    },
    .groups = "drop"
  )

# Build the request_metric string for sc_get_data:
# - remove the [AOI] token from the Metric name (we pass aoi separately)
# - if Metric contains [Year] and we have a chosen year, replace it with the year (e.g., "pestic[Year][AOI]" -> "pestic2019")
# - if Metric contains [Year] but year is NA, skip that metric (warn)
# To do that we need the original Metric text; join back to get it
metric_meta <- SCm2 %>% distinct(metric, .keep_all = TRUE) %>% select(metric)

combos <- metric_year_choice %>%
  left_join(SCm2 %>% select(metric, metric_text = metric), by = "metric") %>% # metric_text may be same as metric; adjust if different column exists
  distinct(metric, aoi, year)

# If your SC_Metrics actually stores the placeholder strings in column 'metric' (e.g. "pestic[Year][AOI]"),
# above is fine. If the placeholder text is in a different column, replace metric_text accordingly.

# Build request_metric and drop those we cannot form
combos <- combos %>%
  rowwise() %>%
  mutate(
    metric_template = metric,                              # adjust if template is in a different column
    has_year_token = str_detect(metric_template, "\\[Year\\]"),
    has_aoi_token  = str_detect(metric_template, "\\[AOI\\]"),
    request_metric = {
      tmp <- metric_template
      if (has_aoi_token) tmp <- str_remove_all(tmp, "\\[AOI\\]")
      if (has_year_token) {
        if (is.na(year)) {
          NA_character_   # cannot form metric name without year -> will be dropped
        } else {
          tmp <- str_replace(tmp, "\\[Year\\]", as.character(year))
        }
      }
      # clean up leftover brackets/spaces if any
      tmp <- str_remove_all(tmp, "\\[|\\]")
      tmp <- str_replace_all(tmp, "\\s+", "")
      tmp
    }
  ) %>%
  ungroup()

# warn and drop metrics we couldn't construct (those that required a year but had NA)
if (any(is.na(combos$request_metric))) {
  warning("Dropping ", sum(is.na(combos$request_metric)), " metric(s) because they require a numeric Year but none was available.")
  combos <- combos %>% filter(!is.na(request_metric))
}

# Now group metrics by aoi + (year or NA) so we can request many at once
groups_df <- combos %>%
  group_by(aoi, year) %>%
  summarize(metrics = list(unique(request_metric)), .groups = "drop")

# prepare comid chunks
comid_col_name <- if ("COMID" %in% names(sites_df)) "COMID" else if ("comid" %in% names(sites_df)) "comid" else stop("NE_PFAS must contain COMID or comid")
unique_comids <- unique(as.integer(sites_df[[comid_col_name]]))
chunks <- split(unique_comids, ceiling(seq_along(unique_comids)/chunk_size))

# UPDATED wrapper: do NOT pass a year argument to sc_get_data
get_sc_chunk <- function(comid_vec, metric_vector, aoi_string = NULL,
                         max_tries = 5, base_sleep = 1.0) {
  tries <- 0
  while (tries < max_tries) {
    tries <- tries + 1
    Sys.sleep(base_sleep * tries)
    args <- list(comid = comid_vec, metric = metric_vector)
    if (!is.null(aoi_string) && !is.na(aoi_string) && nzchar(aoi_string)) args$aoi <- aoi_string
    res <- tryCatch(do.call(StreamCatTools::sc_get_data, args),
                    error = function(e) {
                      message(sprintf("sc_get_data error (try %d): %s", tries, conditionMessage(e)))
                      message("Full error object:"); print(e)
                      NULL
                    })
    if (!is.null(res) && (is.data.frame(res) || length(res) > 0)) return(res)
    Sys.sleep(base_sleep * tries)
  }
  NULL
}

# iterate groups and request
for (g in seq_len(nrow(groups_df))) {
  aoi_val <- groups_df$aoi[g]
  year_val <- groups_df$year[g]
  metrics_vec <- unlist(groups_df$metrics[g])
  
  aoi_tag <- tolower(aoi_val)
  year_tag <- if (is.na(year_val)) "YEARnone" else paste0("Y", year_val)
  group_tag <- paste0(aoi_tag, "_", year_tag)
  
  for (i in seq_along(chunks)) {
    comid_chunk <- chunks[[i]]
    chunk_file <- file.path(out_dir, sprintf("streamcat_%s_chunk_%03d.rds", group_tag, i))
    if (file.exists(chunk_file)) next
    
    message(sprintf("Group %s: requesting chunk %d/%d (n=%d) — metrics: %s",
                    group_tag, i, length(chunks), length(comid_chunk), paste(metrics_vec, collapse = ", ")))
    
    res_df <- get_sc_chunk(comid_chunk,
                           metric_vector = metrics_vec,
                           aoi_string = aoi_val,
                           max_tries = 6, base_sleep = 1.0)
    
    if (is.null(res_df)) {
      saveRDS(NULL, chunk_file)
    } else {
      names(res_df) <- tolower(names(res_df))
      if (!"comid" %in% names(res_df)) {
        if ("COMID" %in% toupper(names(res_df))) names(res_df)[toupper(names(res_df)) == "COMID"] <- "comid" else stop("sc_get_data returned no COMID")
      }
      res_df$comid <- as.integer(res_df$comid)
      saveRDS(as.data.frame(res_df), chunk_file)
    }
    Sys.sleep(2)
  }
}


# Combine files by group, rename metric columns with suffix _<aoi>_Y<year> so names don't clash
chunk_files <- list.files(out_dir, pattern = "^streamcat_.*_chunk_\\d{3}\\.rds$", full.names = TRUE)
files_by_group <- split(chunk_files, gsub("^(.*/)?streamcat_([^_]+_[^_]+)_chunk_\\d{3}\\.rds$", "\\2", chunk_files))

group_dfs <- map(files_by_group, function(files) {
  lst <- lapply(files, function(f) {
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(obj)) return(NULL)
    as.data.frame(obj)
  })
  lst <- Filter(Negate(is.null), lst)
  if (length(lst) == 0) return(NULL)
  dfg <- data.table::rbindlist(lst, fill = TRUE)
  names(dfg) <- tolower(names(dfg))
  if (!"comid" %in% names(dfg)) stop("No comid in combined group")
  dfg$comid <- as.integer(dfg$comid)
  dfg <- dfg[!duplicated(dfg$comid), ]
  # rename metric cols to include group tag suffix
  metric_cols <- setdiff(names(dfg), "comid")
  # derive group tag from the first file name (safe because all files here share the group tag)
  group_tag <- gsub("^(.*/)?streamcat_([^_]+_[^_]+)_chunk_\\d{3}\\.rds$", "\\2", files[[1]])
  new_names <- paste0(metric_cols, "_", group_tag)
  setnames(dfg, old = metric_cols, new = new_names)
  dfg
})

group_dfs <- Filter(Negate(is.null), group_dfs)
if (length(group_dfs) == 0) stop("No group data to combine!")

# merge all groups by comid into a wide table
streamcat_wide <- reduce(group_dfs, function(x, y) merge(x, y, by = "comid", all = TRUE))

# join back to NE_PFAS (sites_df)
streamcat_wide$comid <- as.integer(streamcat_wide$comid)
sites_df[[comid_col_name]] <- as.integer(sites_df[[comid_col_name]])
sites_out <- merge(sites_df, streamcat_wide, by.x = comid_col_name, by.y = "comid", all.x = TRUE)

# Save
saveRDS(streamcat_wide, file = file.path(out_dir, "streamcat_all_wide.rds"))
saveRDS(sites_out, file = file.path(out_dir, "NE_PFAS_streamcat_joined.rds"))
write.csv(sites_out, "NE_PFAS_streamcat.csv", row.names = FALSE)

message("Done. Results saved and joined to NE_PFAS.")










#---------Machine learning ---------------
# Required packages
library(tidymodels)   # core modeling
library(vip)          # variable importance
library(pdp)          # partial dependence
library(data.table)   # fast binds
library(sf)           # if using spatial folds

set.seed(2026)

# 1) Data prep ------------------------------------------------------------
# Assume US_PFAS is your sf and has a numeric PFAS concentration column "PFAS_conc"
# and StreamCat predictor columns. Adjust these names as needed.

df <- as.data.frame(US_PFAS)   # keep geometry in US_PFAS if needed elsewhere
df <- df[!is.na(df$PFOA), ]    # drop missing targets

# Target transform (regression)
df$y_raw <- df$PFOA
df$y <- log1p(df$y_raw)   # model log(target + 1)

# Choose numeric predictors (replace list with your actual columns)
candidate_preds <- c("npdesdensws", "pctimp2019ws",
                     "pcturbhi2019ws", "fertws", "manurews", "superfunddensws",
                     "huden2010ws", "rdcrsws", "pestic2019ws", "mast2019ws", "minedensws",
                     "wwtminordensws", "wwtpmajordensws", "pctagdrainagews", "pctmxfst2019ws")

preds <- intersect(candidate_preds, names(df))

model_df <- df[, c("y", preds, "COMID")]  # keep COMID if you want to inspect later
set.seed(42)
n_noise <- 5
for (k in seq_len(n_noise)) {
  df[[paste0("noise", k)]] <- rnorm(nrow(df), mean = 0, sd = 1)
}

# 2) Train / test split ---------------------------------------------------
set.seed(123)
split <- initial_split(model_df, prop = 0.8, strata = NULL)  # optionally stratify if needed
train <- training(split)
test  <- testing(split)

# If spatial autocorrelation is a concern, use spatial blocking (recommended):
# library(blockCV)
# train_sf <- US_PFAS[rownames(train), ]   # or match by COMID
# sb <- spatialBlock(speciesData = train_sf, theRange = 50000, k = 5, selection = "random")
# train$spatial_fold <- sb$foldID
# Then build resamples with rsample::manual_rset (I can show that if you want).

# 3) Recipe (preprocessing) -----------------------------------------------
rec <- recipe(y ~ ., data = train) %>%
  update_role(COMID, new_role = "id") %>%   # keep COMID out of modeling features
  step_rm(all_of("COMID")) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors())                 # remove near-zero-variance predictors

# 4) Model specification (ranger random forest via parsnip) ----------------
rf_spec <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger", importance = "permutation", num.threads = 2) %>%  # set num.threads to available cores
  set_mode("regression")

# 5) Workflow --------------------------------------------------------------
wf <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)

# 6) Resampling and tuning ------------------------------------------------
cv <- vfold_cv(train, v = 5)   # or use spatial folds if available

# Parameter grid using dials
library(dials)
library(ranger)
mtry_param <- mtry(range = c(1, length(preds)))
min_n_param <- min_n(range = c(2, 20))
params <- parameters(mtry_param, min_n_param)

grid <- grid_latin_hypercube(params, size = 20)   # 20 param combos to try; increase as needed

# Tune
control <- control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE)
set.seed(234)
tune_res <- tune_grid(wf, resamples = cv, grid = grid, metrics = metric_set(rmse, rsq), control = control)

collect_metrics(tune_res)   # overview



# 1) get best params (name the metric explicitly)
best <- tune::select_best(tune_res, metric = "rmse")
best
# alternatively inspect top results
tune::show_best(tune_res, metric = "rmse", n = 10)

# 2) finalize workflow with best params and fit on full training data
final_wf <- tune::finalize_workflow(wf, best)
final_fit <- parsnip::fit(final_wf, data = train)

# 3) evaluate on test set
preds_test <- predict(final_fit, test) %>% bind_cols(test)
preds_test <- preds_test %>% mutate(pred_y = expm1(.pred), true_y = expm1(y))

library(yardstick)
rmse_val <- rmse_vec(truth = preds_test$true_y, estimate = preds_test$pred_y)
rsq_val  <- rsq_vec(truth = preds_test$true_y, estimate = preds_test$pred_y)
rmse_val; rsq_val

# also full tables
rmse(data = preds_test, truth = true_y, estimate = pred_y)
rsq(data = preds_test, truth = true_y, estimate = pred_y)

# 4) variable importance (ranger model via parsnip)
ranger_fit <- extract_fit_parsnip(final_fit)$fit
vip::vip(ranger_fit, num_features = 20, geom = "col")

# 5) partial dependence example (optional)
if ("npdesdensws" %in% preds) {
  pd <- pdp::partial(ranger_fit, pred.var = "npdesdensws", train = as.data.frame(train[, preds]))
  plot(pd)
}

# 6) save final model & predictions
saveRDS(final_fit, file = "rf_final_workflow_PFOA.rds")
data.table::fwrite(as.data.frame(preds_test), "rf_PFOA_test_predictions.csv")
# 12) Notes & tuning tips -------------------------------------------------
# - If performance is poor, try increasing grid size or using grid_random/Latin hypercube with more samples.
# - You can also tune 'trees' (but 1000 is often enough) or use ranger's 'sample.fraction'.
# - For spatial CV: generate spatial blocks with blockCV and then create a manual resample set for tune_grid (I can provide that code).
# - To speed up tuning, set allow_par = TRUE in control_grid and use future/furrr backend; be mindful of API calls and memory.


# Libraries
library(tidymodels)
library(yardstick)
library(ggplot2)
library(dplyr)
library(vip)
library(pdp)
library(data.table)
library(sf)
library(viridis)

# ---------------------------
# 0. Ensure predictions exist
# ---------------------------
if (!exists("preds_test")) {
  if (!exists("final_fit") || !exists("test")) stop("final_fit or test not found in session.")
  preds_test <- predict(final_fit, test) %>% bind_cols(test)
}

# Ensure columns: .pred (predicted log1p), y (true log1p) exist in preds_test
if (!(".pred" %in% names(preds_test)) || !("y" %in% names(preds_test))) {
  stop("preds_test must contain .pred and y columns (model predictions and true log1p target).")
}

# preds_test must contain pred_log/.pred, true_log/y, pred_y (expm1(.pred)), true_y (expm1(y))
# If you don't have those, create them first:
preds_test <- preds_test %>%
  mutate(pred_log = .pred,
         true_log = y,
         pred_y = expm1(.pred),
         true_y = expm1(y))

# Bias (mean error)
bias_log  <- mean(preds_test$pred_log  - preds_test$true_log,  na.rm = TRUE)
bias_orig <- mean(preds_test$pred_y    - preds_test$true_y,     na.rm = TRUE)

# Median bias (robust)
medbias_log  <- median(preds_test$pred_log  - preds_test$true_log,  na.rm = TRUE)
medbias_orig <- median(preds_test$pred_y    - preds_test$true_y,     na.rm = TRUE)

# Mean absolute error & RMSE (if you want quick re-check)
mae_log  <- mean(abs(preds_test$pred_log  - preds_test$true_log),  na.rm = TRUE)
mae_orig <- mean(abs(preds_test$pred_y    - preds_test$true_y),     na.rm = TRUE)
rmse_orig <- sqrt(mean((preds_test$pred_y - preds_test$true_y)^2, na.rm = TRUE))

# Percent bias (relative to mean observed) - useful when scale varies
pct_bias_orig <- 100 * bias_orig / mean(preds_test$true_y, na.rm = TRUE)

# Print nicely
cat("Bias (log1p):", round(bias_log, 4), "Median bias (log1p):", round(medbias_log,4), "\n")
cat("Bias (orig):", round(bias_orig,4), "Median bias (orig):", round(medbias_orig,4), "\n")
cat("MAE (orig):", round(mae_orig,4), "RMSE (orig):", round(rmse_orig,4), "Pct bias:", round(pct_bias_orig,2), "%\n")

# Save metrics
fwrite(as.data.frame(metrics_orig), "rf_test_metrics_original_scale.csv")

# ---------------------------
# 2. Observed vs Predicted plot
# ---------------------------
rmse_val <- rmse_vec(truth = preds_test$true_y, estimate = preds_test$pred_y)
rsq_val  <- rsq_vec(truth = preds_test$true_y, estimate = preds_test$pred_y)

p_obs_pred <- ggplot(preds_test, aes(x = true_y, y = pred_y)) +
  geom_point(alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Observed PFOA (ng/L)", y = "Predicted PFOA (ng/L)",
       subtitle = paste0("RMSE=", round(rmse_val, 2), "; R2=", round(rsq_val, 3))) +
  theme_minimal()
p_obs_pred
ggsave("rf_obs_vs_pred.png", p_obs_pred, width = 6, height = 5, dpi = 300)

# ---------------------------
# 3. Residual diagnostics
# ---------------------------
p_resid_vs_pred <- ggplot(preds_test, aes(x = pred_y, y = resid)) +
  geom_point(alpha = 0.5) + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicted PFOA", y = "Residual (obs - pred)") + theme_minimal()
ggsave("rf_resid_vs_pred.png", p_resid_vs_pred, width = 6, height = 4, dpi = 300)

p_resid_hist <- ggplot(preds_test, aes(x = resid)) + geom_histogram(bins = 40, fill = "gray70") + theme_minimal()
ggsave("rf_resid_hist.png", p_resid_hist, width = 6, height = 4, dpi = 300)

p_resid_qq <- ggplot(preds_test, aes(sample = resid)) + stat_qq() + stat_qq_line() + theme_minimal()
ggsave("rf_resid_qq.png", p_resid_qq, width = 5, height = 5, dpi = 300)

# ---------------------------
# 4. Spatial residual map (requires US_PFAS with COMID & geometry)
# ---------------------------
if (exists("US_PFAS") && "COMID" %in% names(preds_test) && "COMID" %in% names(US_PFAS)) {
  test_sf <- left_join(US_PFAS, preds_test %>% dplyr::select(COMID, pred_y, true_y, resid), by = "COMID")
  # crop map bbox if you have USA_states_crop; otherwise plot full
  basemap <- if (exists("USA_states_crop")) USA_states_crop else st_transform(US_PFAS, st_crs(4326)) %>% st_union() %>% st_as_sf()
  p_map <- ggplot() +
    geom_sf(data = basemap, fill = "gray95", color = "gray60") +
    geom_sf(data = test_sf %>% filter(!is.na(resid)), aes(color = resid), size = 2) +
    scale_color_viridis_c(option = "A", direction = -1) +
    labs(color = "Residual\n(obs - pred)") + theme_minimal()
  ggsave("rf_spatial_residuals.png", p_map, width = 8, height = 5, dpi = 300)
} else {
  message("Skipping spatial residual map: US_PFAS or COMID missing.")
}

# ---------------------------
# 5. Cross-validated tuning summary (if tune_res exists)
# ---------------------------
if (exists("tune_res")) {
  tune_summary <- collect_metrics(tune_res) %>% filter(.metric %in% c("rmse","rsq")) %>% arrange(.metric, mean)
  print(tune_summary)
  fwrite(as.data.frame(tune_summary), "rf_tuning_metrics_summary.csv")
  if (!exists("best")) best <- tune::select_best(tune_res, metric = "rmse")
  cat("Best hyperparameters:\n"); print(best)
}

# ---------------------------
# 6. Permutation importance (vi_permute) & noise baseline
# ---------------------------
# Extract ranger fit
ranger_fit <- extract_fit_parsnip(final_fit)$fit

# Prepare processed training data used by the recipe
rec_prep <- prep(rec, training = train)
train_proc <- juice(rec_prep)   # contains y and preprocessed predictors
X <- as.data.frame(train_proc %>% select(-y))
y <- train_proc$y

# Prediction wrapper for ranger
pred_wrapper <- function(object, newdata) predict(object, data = newdata)$predictions

set.seed(101)
vi_perm <- vip::vi_permute(
  object = ranger_fit,
  pred_wrapper = pred_wrapper,
  train = X,
  target = y,
  metric = "rmse",
  nsim = 30,
  progress = TRUE
)

# Convert to percent importance and compute noise threshold
vi_perm <- as.data.frame(vi_perm)
vi_perm$abs_imp <- abs(vi_perm$Importance)
vi_perm$percent <- 100 * vi_perm$abs_imp / sum(vi_perm$abs_imp)
noise_rows <- vi_perm[grepl("^noise", vi_perm$Variable), ]
noise_cutoff95 <- if (nrow(noise_rows) > 0) as.numeric(quantile(noise_rows$abs_imp, probs = 0.95)) else 0
vi_perm$above_noise95 <- vi_perm$abs_imp > noise_cutoff95

# Save and print
fwrite(vi_perm, "rf_permutation_importance.csv")
print(vi_perm[order(-vi_perm$abs_imp), ])

# Plot percent importances with noise threshold line
vi_plot_df <- vi_perm %>% arrange(abs_imp)
vi_plot_df$feature_type <- ifelse(grepl("^noise", vi_plot_df$Variable), "noise", "real")
vi_plot_df$Variable <- factor(vi_plot_df$Variable, levels = vi_plot_df$Variable)

p_vi <- ggplot(vi_plot_df, aes(x = Variable, y = percent, fill = feature_type)) +
  geom_col() + coord_flip() +
  geom_hline(yintercept = 100 * noise_cutoff95 / sum(vi_perm$abs_imp), linetype = "dashed", color = "red") +
  labs(y = "Percent permutation importance", x = NULL) + theme_minimal()

ggsave("rf_permutation_importance.png", p_vi, width = 7, height = 6, dpi = 300)

# ---------------------------
# 7. Partial dependence (back-transformed)
# ---------------------------
# pick a predictor to inspect; change variable name if desired
pd_var <- "npdesdensws"
if (pd_var %in% colnames(X)) {
  pd <- pdp::partial(ranger_fit, pred.var = pd_var, train = X, progress = "text")
  pd$yhat_orig <- expm1(pd$yhat)
  p_pd <- ggplot(pd, aes_string(x = pd_var, y = "yhat_orig")) +
    geom_line(size = 1) +
    geom_rug(data = train, aes_string(x = pd_var), inherit.aes = FALSE, alpha = 0.2) +
    labs(x = pd_var, y = "Predicted PFOA (ng/L, back-transformed)",
         title = paste0("PDP: ", pd_var)) +
    theme_minimal()
  ggsave(paste0("rf_pdp_", pd_var, ".png"), p_pd, width = 6, height = 4, dpi = 300)
} else {
  message("PDP: predictor ", pd_var, " not found in processed training predictors.")
}

# ---------------------------
# 8. Extra metrics & save
# ---------------------------
# NRMSE, median absolute error, Nash-Sutcliffe efficiency (original scale)
nrmse <- rmse_vec(preds_test$true_y, preds_test$pred_y) / (max(preds_test$true_y, na.rm = TRUE) - min(preds_test$true_y, na.rm = TRUE))
medae <- median(abs(preds_test$true_y - preds_test$pred_y), na.rm = TRUE)
nse <- 1 - sum((preds_test$true_y - preds_test$pred_y)^2, na.rm = TRUE) / sum((preds_test$true_y - mean(preds_test$true_y, na.rm = TRUE))^2, na.rm = TRUE)

cat("NRMSE:", nrmse, "MedAE:", medae, "NSE:", nse, "\n")

# Save key objects
saveRDS(final_fit, "rf_final_workflow.rds")
saveRDS(vi_perm, "rf_vi_permute.rds")
saveRDS(preds_test, "rf_preds_test.rds")
















#chat 
# Required packages
library(tidymodels)   # core modeling
library(vip)          # variable importance
library(pdp)          # partial dependence
library(data.table)   # fast binds
library(sf)           # if using spatial folds
library(dials)
library(ranger)

set.seed(2026)

# 1) Data prep ------------------------------------------------------------
df <- as.data.frame(US_PFAS)   # keep geometry in US_PFAS if needed elsewhere
df <- df[!is.na(df$PFOA), ]    # drop missing targets

# Target transform (regression)
df$y_raw <- df$PFOA
df$y <- log1p(df$y_raw)   # model log(target + 1)

# Choose numeric predictors (replace list with your actual columns)
candidate_preds <- c("npdesdensws", "pctimp2019ws",
                     "pcturbhi2019ws", "fertws", "manurews", "superfunddensws",
                     "huden2010ws", "rdcrsws", "pestic2019ws", "mast2019ws", "minedensws",
                     "wwtminordensws", "wwtpmajordensws", "pctagdrainagews", "pctmxfst2019ws")

# Add white-noise predictors BEFORE selecting preds so they get included
set.seed(42)
n_noise <- 5
noise_names <- paste0("noise", seq_len(n_noise))
for (k in seq_len(n_noise)) {
  df[[noise_names[k]]] <- rnorm(nrow(df), mean = 0, sd = 1)
}

# Build final predictor list including noise
preds <- intersect(candidate_preds, names(df))
preds <- c(preds, noise_names)
preds <- intersect(preds, names(df))  # ensure they exist

# Create model_df (include COMID for bookkeeping)
model_df <- df[, c("y", preds, "COMID")]

# 2) Train / test split ---------------------------------------------------
set.seed(123)
split <- initial_split(model_df, prop = 0.8, strata = NULL)  # optionally stratify if needed
train <- training(split)
test  <- testing(split)

# 3) Recipe (preprocessing) -----------------------------------------------
rec <- recipe(y ~ ., data = train) %>%
  update_role(COMID, new_role = "id") %>%   # keep COMID out of modeling features
  step_rm(all_of("COMID")) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors())                 # remove near-zero-variance predictors

# 4) Model specification (ranger random forest via parsnip) ----------------
rf_spec <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger", importance = "permutation", num.threads = 2) %>%  # set num.threads to available cores
  set_mode("regression")

# 5) Workflow --------------------------------------------------------------
wf <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)

# 6) Resampling and tuning ------------------------------------------------
cv <- vfold_cv(train, v = 5)   # or use spatial folds if available

# Parameter grid: set mtry range based on the current number of predictors
n_preds <- length(preds)
mtry_param <- mtry(range = c(1, n_preds))
min_n_param <- min_n(range = c(2, 20))
params <- parameters(mtry_param, min_n_param)

grid <- grid_latin_hypercube(params, size = 20)   # 20 param combos to try; increase as needed

# Tune
control <- control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE)
set.seed(234)
tune_res <- tune_grid(wf, resamples = cv, grid = grid, metrics = metric_set(rmse, rsq), control = control)

collect_metrics(tune_res)   # overview

# 1) get best params (name the metric explicitly)
best <- tune::select_best(tune_res, metric = "rmse")
best
# alternatively inspect top results
tune::show_best(tune_res, metric = "rmse", n = 10)

# 2) finalize workflow with best params and fit on full training data
final_wf <- tune::finalize_workflow(wf, best)
final_fit <- parsnip::fit(final_wf, data = train)

# 3) evaluate on test set
preds_test <- predict(final_fit, test) %>% bind_cols(test)
preds_test <- preds_test %>% mutate(pred_y = expm1(.pred), true_y = expm1(y))

library(yardstick)
rmse_val <- rmse_vec(truth = preds_test$true_y, estimate = preds_test$pred_y)
rsq_val  <- rsq_vec(truth = preds_test$true_y, estimate = preds_test$pred_y)
rmse_val; rsq_val

# also full tables
rmse(data = preds_test, truth = true_y, estimate = pred_y)
rsq(data = preds_test, truth = true_y, estimate = pred_y)

# 4) variable importance (ranger model via parsnip)
ranger_fit <- extract_fit_parsnip(final_fit)$fit
vip::vip(ranger_fit, num_features = 20, geom = "col")




# 5) partial dependence example (optional)
if ("npdesdensws" %in% preds) {
  pd <- pdp::partial(ranger_fit, pred.var = "npdesdensws", train = as.data.frame(train[, preds]))
  plot(pd)
}

# 6) save final model & predictions
saveRDS(final_fit, file = "rf_final_workflow_PFOA_with_noise.rds")
data.table::fwrite(as.data.frame(preds_test), "rf_PFOA_test_predictions_with_noise.csv")




# 0. Inspect what StreamCat-like columns actually exist in df
# (run this to see what columns look like)
names(df)[grepl("npdes|pctimp|pcturb|fert|manure|superfund|huden|rdcrs|pestic|conn|imp", names(df), ignore.case = TRUE)]

# 1. Explicitly find which candidate_preds exist in df
available <- candidate_preds[candidate_preds %in% names(df)]
message("Candidate preds found in df: ", paste(available, collapse = ", "))
# If none found, fall back to pattern-match guess:
if (length(available) == 0) {
  available <- names(df)[grepl("(_ws$)|ws$|npdes|pctimp|pcturb|fert|manure|superfund|huden|rdcrs|pestic", 
                               names(df), ignore.case = TRUE)]
  message("Guessed preds by pattern: ", paste(available, collapse = ", "))
}

# 2. Ensure noise columns exist and include them
noise_names <- paste0("noise", seq_len(n_noise))   # n_noise from earlier
# create noise if missing
set.seed(42)
for (nm in noise_names) if (!nm %in% names(df)) df[[nm]] <- rnorm(nrow(df))

# 3. Build final preds including noise and streamcat predictors
preds <- unique(c(available, noise_names))
message("Final predictors to use (count = ", length(preds), "): ", paste(preds, collapse = ", "))

# 4. Rebuild model_df and re-split (this ensures train/test contain the intended predictors)
model_df <- df[, c("y", preds, "COMID"), drop = FALSE]
set.seed(123)
split <- initial_split(model_df, prop = 0.8)
train <- training(split)
test  <- testing(split)

# 5. Recreate recipe and quickly check what variables survive preprocessing
rec <- recipe(y ~ ., data = train) %>%
  update_role(COMID, new_role = "id") %>%
  step_rm(all_of("COMID")) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors())

rec_prep <- prep(rec, training = train)
juice_df <- juice(rec_prep)
message("Columns after recipe preprocessing (juice):")
print(colnames(juice_df))


# quick RF (no tuning) to check predictive signal
n_preds <- ncol(juice_df) - 1   # subtract y
quick_mtry <- max(1, floor(sqrt(n_preds)))

rf_quick <- rand_forest(mtry = quick_mtry, trees = 500, min_n = 5) %>%
  set_engine("ranger", importance = "permutation", num.threads = 2) %>%
  set_mode("regression")

wf_quick <- workflow() %>% add_recipe(rec) %>% add_model(rf_quick)
fit_quick <- fit(wf_quick, data = train)

preds_test_q <- predict(fit_quick, test) %>% bind_cols(test) %>%
  mutate(pred_y = expm1(.pred), true_y = expm1(y))

rmse(data = preds_test_q, truth = true_y, estimate = pred_y)
rsq(data = preds_test_q, truth = true_y, estimate = pred_y)

# variable importance
ranger_fit_q <- extract_fit_parsnip(fit_quick)$fit
vip::vip(ranger_fit_q, num_features = min(25, n_preds))



# 7) (Optional) compute permutation importances against noise baseline
# Example using vip::vi_permute (nsim increases stability)
rec_prep <- prep(rec, training = train)
x_train_processed <- bake(rec_prep, new_data = juice(rec_prep))
pred_wrapper <- function(object, newdata) predict(object, data = newdata)$predictions

set.seed(101)
vi_perm <- vip::vi_permute(
  object = ranger_fit,
  pred_wrapper = pred_wrapper,
  train = as.data.frame(x_train_processed %>% select(-y)),
  target = x_train_processed$y,
  metric = "rmse",
  nsim = 30,
  progress = TRUE
)

# Add percent importance and noise threshold
vi_perm$abs_imp <- abs(vi_perm$Importance)
vi_perm$percent <- 100 * vi_perm$abs_imp / sum(vi_perm$abs_imp)
noise_rows <- vi_perm[grep("^noise", vi_perm$Variable), ]
noise_cutoff95 <- as.numeric(quantile(noise_rows$abs_imp, probs = 0.95))
vi_perm$above_noise95 <- vi_perm$abs_imp > noise_cutoff95
print(vi_perm[order(-vi_perm$abs_imp), ])
