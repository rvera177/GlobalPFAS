
#conus 2: back at it
getwd()
setwd("C:/Users/Ruli's computer/OneDrive/Documents/Soil&Water lab/GlobalPFAS")


# STEP 1: Bring in our beautiful datasets

library(readr)
library(dplyr)
library(tibble)

# Define all dataset URLs on github
dataset_catalog <- tribble(
  ~dataset_name,        ~url,                                                                                                                   ~expected_region,
  "Caravan_PFAS",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Caravan_PFAS_2026_standardized.csv",         "Global",
  "Camacho_2024",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Camacho_et_al_2024_Florida.csv",  "USA",
  "Sims_2025",          "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sims_et_al_2025_%20Western_United_States.csv", "USA",
  "NH_DES_2026",        "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/NewHampshire_DES_PFAS_Data_Dump.csv", "USA",
  "Breitmeyer_2023",    "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Breitmeyer_et_al_2023_Pennsylvania.csv", "USA",
  "Zhang_2016",         "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Zhang_et_al_2016_RI_NY.csv",     "USA",
  "Goodrow_2020",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Goodrow_et_al_2020_New_Jersey.csv", "USA",
  "Bai_Son_2021",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Bai_and_Son_2021_Renoe_LasVegas.csv", "USA",
  "Maine_DEP_2026",     "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/MaineDEP_2026_Datadump_cleaned.csv", "USA",
  "WQP_USA_2026",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/North%20America/WQP_USA_Data_cleaned.csv",   "USA",
  "Viticoski_2022",      "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Hayworth_et_al_2022_Alabama_cleaned.csv", "USA",
  "Dunn_2023",          "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Dunn_et_al_2023_RhodeIsland_complete.csv", "USA",
  "Forster_2024",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Forster_et_al_2024_SouthCarolina_cleaned.csv", "USA",
  "Penland_2020",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Penland_2020_SC_NC_cleaned.csv", "USA",
  "Labad_2025",         "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Labad_et_al_2025_Georgia.csv",   "USA",
  "Webb_2026",          "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Webb_et_al_2026.csv",  "USA",
  "Colorado_DPH_2026",  "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Colorado_DPH.csv",              "USA",
  "Scott_2009",         "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Scott_et_al_2009_Canada.csv",   "Canada",
  "Teymoorian_2021",    "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Teymoorian_2025_Montreal.csv",  "Canada",
  "Ahrens_2023",        "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Ahrens_et_al_2023_Arctic.csv",  "Arctic",
  "Sharma_2016",        "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sharma_et_al_2016_Ganges_River.csv", "India",
  "AustraliaMap_2026",  "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Australia_Government_PFAS_CHEM_MAP_Clean.csv", "Australia",
  "Woodward_2026",     "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Woodward_et_al_California_2026.csv", "USA",
  "MA_PWS_2026", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/MassachusettsSurfaceWaterSupply_PFAS_Cleaned.csv", "USA",
  "Michigan_MPART_2026", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Michican_MPART_PFAS_Final.csv", "USA",
  "Petre_2022", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Petre_et_al_2022_North_Carolina.csv", "USA",
  "NC_Neuse_2020", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/North_Carolina_DWR_NeuseBasin_2020_clean.csv", "USA",
  "MassDEP_2024", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/MassDEP2024.csv", "USA",
  "Beisner_2025", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Beisner_2025_NewMexico_cleaned.csv", "USA",
  "Ahrens_2016", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Ahrens_2016_LakeTana_Ethiopia_Clean.csv", "Ethiopia",
  "Ahrens_2016", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Ahrens_2016_LakeTana_Ethiopia_Clean.csv", "Ethiopia",
  "Duru_2026", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Duru_et_al_2026_Maryland.csv", "USA",
  "Hansen_2002", "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Hansen_et_al_2002_Tennessee_River.csv", "USA",
  "Quezada_2023","https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/QuezadaDavalos_2023_ColoradoSprings.csv","USA"
)

library(stringr)

# PFAS compounds names standardized across my datasets
all_pfas <- c(
  "PFOS",
  "PFOA",
  "ADONA",
  "FBSA",
  "FOSAA",
  "GenX",
  "N_EtFOSA",
  "N_EtFOSE",
  "N_MeFOSE",
  "NEtFOSAA",
  "NFDHA",
  "NMeFOSA",
  "NMeFOSAA",
  "PFBA",
  "PFBS",
  "PFDA",
  "PFDoA",
  "PFDoS",
  "PFDS",
  "PFECA_G",
  "PFEESA",
  "PFESA_BP_1",
  "PFESA_BP_2",
  "PFHpA",
  "PFHpS",
  "PFHxA",
  "PFHxDA",
  "PFHxS",
  "PFMBA",
  "PFMOAA",
  "PFMOBA",
  "PFMOPrA",
  "PFNA",
  "PFNS",
  "PFO2HxA",
  "PFO3OA",
  "PFO4DA",
  "PFODA",
  "PFOSA",
  "PFPA",
  "PFPeA",
  "PFPeS",
  "PFTeDA",
  "PFTrDA",
  "PFUnDA",
  "X10_2_FTS",      # was X10.2.FTS
  "X11_Cl_PF3OUdS", # was X11.Cl.PF3OUdS
  "X3_3_FTCA",
  "X4_2_FTS",       # was X4.2.FTS
  "X5_3_FTCA",      # was X5.3.FTCA
  "X6_2_FTS",       # was X6.2.FTS
  "X7_3_FTCA",      # was X7.3.FTCA
  "X8_2_FTS",       # was X8.2.FTS
  "X9_Cl_PF3ONS"    # was X9.Cl.PF3ONS
)

standardize_names <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    str_remove("^X(?=[0-9])")
}

# normalized-name -> canonical all_pfas name
compound_lookup <- setNames(all_pfas, standardize_names(all_pfas))

# Function to load and standardize each dataset
load_and_standardize <- function(dataset_name, url, expected_region) {
  
  cat("Loading:", dataset_name, "...\n")
  
  # Load CSV
  df <- read_csv(url, show_col_types = FALSE)
  
  norm_names <- standardize_names(names(df))
  matched    <- norm_names %in% names(compound_lookup)
  names(df)[matched] <- compound_lookup[norm_names[matched]]
  
  # Extract year and month from date columns (handle variable naming)
  if ("Sample Date (MM/DD/YYY)" %in% names(df)) {
    parsed_date <- as.Date(df$`Sample Date (MM/DD/YYY)`, format = "%m/%d/%Y")
    df <- df %>%
      mutate(
        year  = as.integer(format(parsed_date, "%Y")),
        month = as.integer(format(parsed_date, "%m"))
      )
  } else if ("Sampling Year" %in% names(df)) {
    df <- df %>%
      mutate(year = as.integer(`Sampling Year`))
  }
  
  # Filter to Surface Water only
  if ("Sample Type" %in% names(df)) {
    df <- df %>% filter(`Sample Type` == "Surface Water")
  }
  
  # Select core columns: essential spatial + temporal + PFAS compounds + source tracking
  df_clean <- df %>%
    dplyr::select(
      any_of(c("Latitude", "Longitude", "year", "month", "Sample Date (MM/DD/YYY)", "Sample Time", all_pfas))
    ) %>%
    # Add metadata columns
    mutate(
      dataset_source = dataset_name,
      expected_region = expected_region,
      .before = Latitude
    )
  
  cat("  → Loaded:", nrow(df_clean), "observations\n")
  
  return(df_clean)
}

# Load all datasets
all_data_list <- mapply(
  load_and_standardize,
  dataset_name = dataset_catalog$dataset_name,
  url = dataset_catalog$url,
  expected_region = dataset_catalog$expected_region,
  SIMPLIFY = FALSE
)

known_core_cols <- c("dataset_source", "expected_region", "Latitude", "Longitude",
                     "year", "month", "Sample Date (MM/DD/YYY)", "Sample Time",
                     "Sample Type", "Sampling Year")

unmatched_cols <- all_data_list %>%
  lapply(names) %>%
  unlist() %>%
  unique() %>%
  setdiff(c(known_core_cols, all_pfas))

if (length(unmatched_cols) > 0) {
  cat("Unmatched columns found — check these:\n")
  print(unmatched_cols)
}


# Combine into single dataframe
global_pfas_raw <- bind_rows(all_data_list)
colnames(global_pfas_raw)
cat("GLOBAL DATABASE SUMMARY\n")
cat("Total observations:", nrow(global_pfas_raw), "\n")
cat("Datasets loaded:", length(all_data_list), "\n")
cat("Year range:", min(global_pfas_raw$year, na.rm = TRUE), 
    "to", max(global_pfas_raw$year, na.rm = TRUE), "\n\n")

# Show breakdown by dataset and region
cat("Observations by dataset:\n")
print(global_pfas_raw %>%
        group_by(dataset_source, expected_region) %>%
        summarise(n_obs = n(), .groups = "drop") %>%
        arrange(desc(n_obs)))

# Count unique sites by unique coordinate pairs
unique_global_sites <- global_pfas_raw %>%
  distinct(Latitude, Longitude) %>%
  nrow()

cat("Total unique sites (unique coordinate pairs):", unique_global_sites, "\n\n")

# Breakdown of unique sites per dataset
cat("Unique sites by dataset:\n")
print(global_pfas_raw %>%
        group_by(dataset_source, expected_region) %>%
        summarise(
          n_obs        = n(),
          n_unique_sites = n_distinct(paste(Latitude, Longitude)),
          .groups = "drop"
        ) %>%
        arrange(desc(n_unique_sites)))


# PFAS dataset visualization

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(patchwork)
library(viridis)

#-------- 1. Assign a continent to every observation through spatial join-----------


pfas_sf <- global_pfas_raw %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

world_continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(continent) %>%
  st_make_valid()

pfas_sf <- st_join(pfas_sf, world_continents, join = st_intersects) %>%
  mutate(continent = if_else(is.na(continent), "Unknown/Ocean", continent))

global_pfas_raw <- pfas_sf %>%
  st_drop_geometry()

# ----------2. Samples-by-year bar chart--------------

samples_by_year <- global_pfas_raw %>%
  filter(!is.na(year)) %>%
  count(year, continent)

p_year <- ggplot(samples_by_year, aes(x = year, y = n, fill = continent)) +
  geom_col(width = 0.8) +
  scale_fill_viridis_d(option = "turbo", name = "Continent") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "PFAS Surface Water Samples by Year",
    x = "Year",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

p_year
#ggsave("samples_by_year.png", p_year, width = 10, height = 5, dpi = 300)

# ------3. Spatial distribution: patchwork of two site maps-------------
#    A) sampling frequency per site (binned)
#    B) mean PFOA concentration per site

world_base <- ne_countries(scale = "medium", returnclass = "sf")

site_summary <- global_pfas_raw %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(Latitude, Longitude) %>%
  summarise(
    n_samples = n(),
    mean_PFOA = mean(PFOA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_samples_bin = cut(
      n_samples,
      breaks = c(0, 2, 5, 15, 30, Inf),
      labels = c("1-2", "2-5", "5-15", "15-30", ">30"),
      right = TRUE
    )
  )

p_freq <- ggplot() +
  geom_sf(data = world_base, fill = "grey80", color = "grey70", linewidth = 0.2) +
  geom_point(
    data = site_summary %>% arrange(n_samples),
    aes(x = Longitude, y = Latitude, color = n_samples_bin),
    size = 0.8, alpha = 0.9
  ) +
  scale_color_viridis_d(option = "plasma", name = "Observations\nper site") +
  coord_sf(expand = FALSE, ylim = c(-60, 90)) +
  labs(title = "Site Distribution and Number of Observations per Site") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(), 
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    title = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 10))
  )

p_freq

?scale_color_viridis_d
# NOTE: assumes PFOA is in ng/L — adjust the axis label if your units differ.
# Log color scale because concentrations typically span orders of magnitude.
p_pfoa <- ggplot() +
  geom_sf(data = world_base, fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_point(
    data = site_summary %>% filter(!is.na(mean_PFOA), mean_PFOA > 0),
    aes(x = Longitude, y = Latitude, color = mean_PFOA),
    size = 0.8, alpha = 0.7
  ) +
  scale_color_viridis_c(option = "magma", trans = "log10",
                        name = "Mean PFOA\n(ng/L, log)") +
  coord_sf(expand = FALSE) +
  labs(title = "Mean PFOA Concentration") +
  theme_minimal(base_size = 10) +
  theme(axis.title = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom")

p_spatial <- p_freq + p_pfoa +
  plot_annotation(title = "PFAS Surface Water Monitoring Sites")

p_spatial
p_freq
#ggsave("spatial_distribution.png", p_spatial, width = 12, height = 6, dpi = 300)



pfos_counts <- global_pfas_raw %>%
  filter(!is.na(PFOS), continent != "Unknown/Ocean") %>%
  group_by(continent) %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(paste(Latitude, Longitude)),
    pfos_detected = sum(PFOS > 0),
    pfos_pct_detected = round(100 * sum(PFOS > 0) / n(), 1),
    pfos_mean = round(mean(PFOS, na.rm = TRUE), 2),
    pfos_median = round(median(PFOS, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))

# Calculate totals row
totals_row <- data.frame(
  continent = "TOTAL",
  n_obs = sum(pfos_counts$n_obs),
  n_sites = n_distinct(paste(global_pfas_raw$Latitude, global_pfas_raw$Longitude)),
  pfos_detected = sum(pfos_counts$pfos_detected),
  pfos_pct_detected = round(100 * sum(pfos_counts$pfos_detected) / sum(pfos_counts$n_obs), 1),
  pfos_mean = round(mean(global_pfas_raw$PFOS, na.rm = TRUE), 2),
  pfos_median = round(median(global_pfas_raw$PFOS, na.rm = TRUE), 2)
)

# Bind totals row to table
pfos_counts <- bind_rows(pfos_counts, totals_row)

print(pfos_counts)
#write_csv(pfos_counts, "pfos_counts.csv")


# Find top 4 sites by observation count
top_4_sites <- global_pfas_raw %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(Latitude, Longitude) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  slice_max(n_obs, n = 4) %>%
  mutate(site_id = paste0("Site ", row_number(), "\n(", round(Latitude, 2), ", ", round(Longitude, 2), ")\nn=", n_obs))

# Filter to top 4 sites and prepare data
major_compounds <- c("PFOS", "PFOA", "PFNA", "PFHxS", "PFBS", "PFDA")

top_sites_data <- global_pfas_raw %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  inner_join(top_4_sites %>% select(Latitude, Longitude, site_id), 
             by = c("Latitude", "Longitude")) %>%
  select(site_id, `Sample Date (MM/DD/YYY)`, all_of(major_compounds)) %>%
  mutate(
    sample_date = as.Date(`Sample Date (MM/DD/YYY)`, format = "%m/%d/%Y")
  ) %>%
  filter(!is.na(sample_date)) %>%
  pivot_longer(all_of(major_compounds), 
               names_to = "compound", 
               values_to = "concentration") %>%
  filter(!is.na(concentration), concentration > 0)

# Plot
p_top_sites <- ggplot(top_sites_data, aes(x = sample_date, y = concentration, color = compound)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.6) +
  facet_wrap(~site_id, scales = "free_y", ncol = 2) +
  scale_color_viridis_d(option = "turbo") +
  scale_y_log10() +
  labs(
    title = "PFAS Concentrations Over Time at Top 4 Monitoring Sites",
    x = "Sample Date",
    y = "Concentration (ng/L, log scale)",
    color = "Compound"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p_top_sites
