

#Helo
#this is the set up for retrieving Water Quality data data from
# the Water Quality Portal. https://www.waterqualitydata.us
#I'm trying to get all their data.


library(dataRetrieval)
library(dplyr)
library(jsonlite)
library(purrr)
library(sf)
library(rnaturalearth)
library(ggplot2)

#test
DeWitt <- readWQPdata(
  statecode = "Illinois",
  countycode = "DeWitt",
  characteristicName = "Nitrogen"
)

#testing on a single location at first for a single compound, PFOA
DeWitt <- readWQPdata(
  statecode = "Illinois",
  countycode = "DeWitt",
  characteristicName = "Perfluorooctanoic acid"
)

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



#--PFAS Retrieval -------------------
library(dataRetrieval)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(jsonlite)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)  # one-time: remotes::install_github("ropensci/rnaturalearthhires")
library(ggplot2)

# WQP characteristic "types" that hold PFAS data. Both are needed: which one a
# state's data sits under varies by provider. (The PFOA/PFOS-specific types
# returned no data in any state tested.)
pfas_type <- c("Organics, PFAS", "PFAS,Perfluorinated Alkyl Substance")

sampletype <- c("Stream", "Estuary", "Lake, Reservoir, Impoundment",
                "Spring", "Wetland", "Ocean")

cache_dir <- "wqp_cache_pfas"
# unlink(cache_dir, recursive = TRUE)   # uncomment to wipe the cache and re-pull
dir.create(cache_dir, showWarnings = FALSE)

# State codes straight from WQP; keep only US codes (states + territories)
wqp_codes_raw <- fromJSON("https://www.waterqualitydata.us/Codes/statecode?mimeType=json")
state_list    <- grep("^US:", wqp_codes_raw$codes$value, value = TRUE)

# WQX3 column names -> the legacy names used in the rest of this script.
# any_of() means a wrong guess doesn't error here; section 4 checks for gaps.
wqx3_to_wqx2 <- c(
  MonitoringLocationIdentifier = "Location_Identifier",
  LatitudeMeasure              = "Location_LatitudeStandardized",
  LongitudeMeasure             = "Location_LongitudeStandardized",
  ActivityStartDate            = "Activity_StartDate",
  ActivityTypeCode             = "Activity_TypeCode",
  OrganizationIdentifier       = "Org_Identifier",
  CharacteristicName           = "Result_Characteristic",
  ResultMeasureValue           = "Result_Measure",
  `ResultMeasure.MeasureUnitCode` = "Result_MeasureUnit",
  ResultDetectionConditionText = "Result_ResultDetectionCondition",
  ResultSampleFractionText     = "Result_SampleFraction",
  ResultValueTypeName          = "Result_MeasureValueType",
  `ResultAnalyticalMethod.MethodIdentifier` = "ResultAnalyticalMethod_Identifier"
)

# -----------------------------------------------------------------------------
# 2. RETIRED-NAME LOOKUP (built once, saved as CSV)
#    Retired WQP names embed their replacement: "<old>***retired***use <new>".
#    Data can hold either the full string or just the "<old>" stem, so both are
#    keys. Edit pfas_name_map.csv by hand if you want to add a `compound` column.
# -----------------------------------------------------------------------------

chars <- fromJSON("https://www.waterqualitydata.us/Codes/characteristicname?mimeType=json")$codes

retired_full <- chars %>%
  filter(grepl("\\*\\*\\*retired\\*\\*\\*", value, ignore.case = TRUE)) %>%
  transmute(
    CharacteristicName = value,
    canonical_name = trimws(sub("(?i)^.*\\*\\*\\*retired\\*\\*\\*\\s*(use\\s+)?", "",
                                value, perl = TRUE))
  )

retired_map <- bind_rows(
  retired_full,
  mutate(retired_full,
         CharacteristicName = trimws(sub("\\*\\*\\*retired\\*\\*\\*.*$", "", CharacteristicName)))
) %>%
  distinct(CharacteristicName, .keep_all = TRUE)

write.csv(retired_map, "pfas_name_map.csv", row.names = FALSE)
# later sessions can skip the build above and just: retired_map <- read.csv("pfas_name_map.csv")

# -----------------------------------------------------------------------------
# 3. PULL ALL PFAS, ONE STATE AT A TIME (cached, retried, split if needed)
# -----------------------------------------------------------------------------

# One request with retries. Returns a data frame (empty tibble if no data),
# or NULL if it kept failing (e.g. WQP's "INCOMPLETE DATA" error).
pull_chunk <- function(st, tp, lo = NULL, hi = NULL, tries = 3) {
  args <- list(statecode = st, characteristicType = tp, sampleMedia = "Water",
               siteType = sampletype, service = "ResultWQX3",
               dataProfile = "basicPhysChem", ignore_attributes = TRUE)
  if (!is.null(lo)) args$startDateLo <- lo      # WQP date format: MM-DD-YYYY
  if (!is.null(hi)) args$startDateHi <- hi
  for (i in seq_len(tries)) {
    out <- tryCatch(do.call(readWQPdata, args), error = function(e) e)
    if (!inherits(out, "error")) return(if (is.null(out)) tibble() else out)
    message("  retry ", i, " (", st, " / ", tp, "): ", conditionMessage(out))
    Sys.sleep(5 * i)
  }
  NULL
}

year_chunks <- c(list(c("01-01-1900", "12-31-2009")),
                 lapply(2010:2026, function(y) c(paste0("01-01-", y), paste0("12-31-", y))))

# One state: one request per type; a type that still fails is split by year.
# Returns NULL if anything failed, so a partial state is never cached.
pull_state <- function(st) {
  parts <- map(pfas_type, function(tp) {
    d <- pull_chunk(st, tp)
    if (!is.null(d)) return(d)
    message("  splitting ", st, " / ", tp, " by year")
    ch <- map(year_chunks, ~ pull_chunk(st, tp, .x[1], .x[2]))
    if (any(map_lgl(ch, is.null))) return(NULL)
    map_dfr(ch, ~ mutate(.x, across(everything(), as.character)))
  })
  if (any(map_lgl(parts, is.null))) return(NULL)
  map_dfr(parts, ~ mutate(.x, across(everything(), as.character)))
}

raw_list      <- list()
failed_states <- c()

for (st in state_list) {
  f <- file.path(cache_dir, paste0(gsub(":", "_", st), ".rds"))
  if (file.exists(f)) { raw_list[[st]] <- readRDS(f); next }
  message("Fetching data for: ", st)
  Sys.sleep(2)
  
  raw_data <- pull_state(st)
  if (is.null(raw_data)) { failed_states <- c(failed_states, st); next }
  
  raw_data <- raw_data %>% rename(any_of(wqx3_to_wqx2))
  saveRDS(raw_data, f)
  raw_list[[st]] <- raw_data
}

failed_states   # anything here was NOT saved; just rerun this section to retry it

# -----------------------------------------------------------------------------
# 4. COMBINE, RENAME RETIRED NAMES, AND LOOK BEFORE FILTERING
# -----------------------------------------------------------------------------

raw_all <- raw_list %>%
  keep(~ nrow(.x) > 0) %>%
  map_dfr(~ mutate(.x, across(everything(), as.character)))

# Fail early, with a clear message, if a guessed WQX3 column name didn't map
required <- c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure",
              "ActivityStartDate", "ActivityTypeCode", "CharacteristicName",
              "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
              "ResultDetectionConditionText", "ResultSampleFractionText")
missing_cols <- setdiff(required, names(raw_all))
if (length(missing_cols)) {
  stop("Not found after rename: ", paste(missing_cols, collapse = ", "),
       "\nCheck names(raw_all) and fix wqx3_to_wqx2 in section 1.")
}

# Keep the provider's original name, then swap retired names for current ones
raw_all <- raw_all %>%
  left_join(retired_map, by = "CharacteristicName") %>%
  mutate(CharacteristicName_raw = CharacteristicName,
         CharacteristicName     = coalesce(canonical_name, CharacteristicName)) %>%
  select(-canonical_name)

# Look at these BEFORE deciding filters (NA fractions matter: filtering
# == "Total" also drops every record where the fraction was left blank)
raw_all %>% count(ResultSampleFractionText, sort = TRUE)
raw_all %>% count(ActivityTypeCode, sort = TRUE)
raw_all %>% count(ResultSampleFractionText, ActivityTypeCode, sort = TRUE)
raw_all %>% count(`ResultMeasure.MeasureUnitCode`, sort = TRUE)
raw_all %>% count(CharacteristicName, sort = TRUE) %>% as_tibble() %>% print(n = 100)

grep("Detection|Quantitation|Limit", names(raw_all), value = TRUE)
raw_all %>% count(DetectionLimit_TypeA, DetectionLimit_MeasureUnitA, sort = TRUE)

raw_all %>% count(CharacteristicName, sort = TRUE) %>% write.csv("pfas_names_seen.csv", row.names = FALSE)

grep("Location_Type", names(raw_all), value = TRUE)
raw_all %>% count(Location_Type, sort = TRUE) %>% as_tibble() %>% print(n = 40)

# -----------------------------------------------------------------------------
# 5. FILTER, CLEAN VALUES, FIX COORDINATES
# -----------------------------------------------------------------------------

whole_water    <- c("Total", "Unfiltered", "Total Recoverable")
dissolved      <- c("Dissolved", "Filtered, lab")
keep_fractions <- c(whole_water, dissolved)
keep_activity  <- c("Sample-Routine", "Sample-Integrated Vertical Profile")

censored_pat <- "^Not Detected|^Below|^Present Below"
drop_pat     <- "Systematic Contamination|Not Reported|^Present Above"

mdl_threshold <- 0.5   # ng/L: non-detects with a limit below this become 0

# guard: stop if the limit columns didn't come through under these names
if (!all(c("DetectionLimit_MeasureA", "DetectionLimit_MeasureUnitA") %in% names(raw_all)))
  stop("Detection limit columns not found; check the grep output from step 1.")
for (nm in c("DetectionLimit_MeasureB", "DetectionLimit_MeasureUnitB",
             "DetectionLimit_TypeA", "DetectionLimit_TypeB"))
  if (!nm %in% names(raw_all)) raw_all[[nm]] <- NA_character_

to_ng_l <- function(val, unit) {
  val <- suppressWarnings(as.numeric(val)); unit <- tolower(unit)
  case_when(unit == "ng/l" ~ val,
            unit == "ug/l" ~ val * 1000,
            TRUE ~ NA_real_)      # unknown or missing unit -> can't use the limit
}

all_WQP_data <- raw_all %>%
  filter(
    (`ResultMeasure.MeasureUnitCode` == "ng/L" | grepl(censored_pat, ResultDetectionConditionText)),
    !grepl(drop_pat, ResultDetectionConditionText),                    # see below
    is.na(ResultSampleFractionText) | ResultSampleFractionText %in% keep_fractions,
    ActivityTypeCode %in% keep_activity,
    !grepl("^Facility", Location_Type)                                 # new; use "^Facility|Storm" to also drop stormwater
  ) %>%
  mutate(
    nondetect = grepl(censored_pat, ResultDetectionConditionText),
    value_raw = suppressWarnings(as.numeric(ResultMeasureValue)),
    dl_ng_l   = coalesce(to_ng_l(DetectionLimit_MeasureA, DetectionLimit_MeasureUnitA),
                         to_ng_l(DetectionLimit_MeasureB, DetectionLimit_MeasureUnitB)),
    fraction_group = case_when(ResultSampleFractionText %in% whole_water ~ "whole",
                               ResultSampleFractionText %in% dissolved   ~ "dissolved",
                               ResultSampleFractionText == "Supernate"   ~ "supernate",
                               TRUE ~ "unknown"),
    ResultMeasureValue = case_when(
      !nondetect                     ~ value_raw,
      dl_ng_l < mdl_threshold        ~ 0,          # limit is low enough to call it 0
      TRUE                           ~ NA_real_    # limit >= threshold, or no usable limit
    ),
    across(c(LatitudeMeasure, LongitudeMeasure), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  filter(nondetect | !is.na(value_raw))    # drop only detected results with no numeric value

# Recover missing coordinates from the station service (only for those sites)
missing_ids <- all_WQP_data %>%
  filter(is.na(LatitudeMeasure) | is.na(LongitudeMeasure)) %>%
  distinct(MonitoringLocationIdentifier) %>% pull()
message(length(missing_ids), " sites missing coordinates")

if (length(missing_ids) > 0) {
  get_sites <- possibly(function(ids) {
    readWQPdata(siteid = ids, service = "StationWQX3", ignore_attributes = TRUE) %>%
      rename(any_of(wqx3_to_wqx2)) %>%
      select(MonitoringLocationIdentifier, lat_fix = LatitudeMeasure, lon_fix = LongitudeMeasure) %>%
      mutate(across(c(lat_fix, lon_fix), as.numeric)) %>%
      distinct()
  }, otherwise = NULL)
  
  fix <- split(missing_ids, ceiling(seq_along(missing_ids) / 50)) %>%
    map(get_sites) %>% compact() %>% bind_rows() %>%
    distinct(MonitoringLocationIdentifier, .keep_all = TRUE)
  
  if (nrow(fix) > 0) {
    all_WQP_data <- all_WQP_data %>%
      left_join(fix, by = "MonitoringLocationIdentifier") %>%
      mutate(LatitudeMeasure  = coalesce(LatitudeMeasure,  lat_fix),
             LongitudeMeasure = coalesce(LongitudeMeasure, lon_fix)) %>%
      select(-lat_fix, -lon_fix)
  }
}

all_WQP_data <- all_WQP_data %>%
  filter(!is.na(LatitudeMeasure), !is.na(LongitudeMeasure))

# Final columns. Fraction and activity type stay in so you can test how
# including/excluding them changes the model.
all_WQP_data <- all_WQP_data %>%
  select(
    MonitoringLocationIdentifier,
    LatitudeMeasure,
    LongitudeMeasure,
    ActivityStartDate,
    any_of(c("ResultAnalyticalMethod.MethodIdentifier", "OrganizationIdentifier")),
    CharacteristicName,
    CharacteristicName_raw,
    ResultSampleFractionText,
    fraction_group,
    ActivityTypeCode,
    ResultMeasureValue,
    nondetect,
    dl_ng_l,                                           
    any_of(c("DetectionLimit_TypeA", "DetectionLimit_TypeB"))
  )

saveRDS(all_WQP_data, "all_WQP_data.rds")



# =============================================================================
# LONG -> WIDE: one column per PFAS compound, all in ng/L
# Run after section 5 of wqp_pfas_pipeline.R (needs `all_WQP_data`, including
# the `fraction_group` column from the fraction edit).
# =============================================================================

library(dplyr)
library(tidyr)
library(tibble)

stopifnot("fraction_group" %in% names(all_WQP_data))

# -----------------------------------------------------------------------------
# 1. NAME -> SHORT COMPOUND NAME
#    Rules run on a normalised name (lower case, no spaces, "-1-" removed) and
#    the first match wins. Chain-length names (PFOA, PFOS, PFNA, ...) and
#    fluorotelomers (FTS_6_2, FTCA_5_3, ...) are built from the name itself.
#    "plus total oxidizable precursors" names get a _TOP suffix (a different
#    measurement). Isotope-labelled standards get "LABELED_STD"; pesticides and
#    other fluorinated non-targets get "NONTARGET" (both dropped from the wide table).
# -----------------------------------------------------------------------------

compound_rules <- tribble(
  ~pattern, ~compound,
  "n-methyl.*acet", "NMeFOSAA",
  "n-ethyl.*acet", "NEtFOSAA",
  "n-methyl.*(ethanol|hydroxyethyl)", "NMeFOSE",
  "n-ethyl.*(ethanol|hydroxyethyl)", "NEtFOSE",
  "n-methyl.*sulfonamid", "NMeFOSA",
  "n-ethyl.*sulfonamid|^sulfluramid", "NEtFOSA",
  "^perfluorooctanesulfonamide", "PFOSA",
  "^perfluorohexanesulfonamide", "FHxSA",
  "^perfluorobutylsulfonamide|^perfluorobutanesulfonamide", "FBSA",
  "^1h,1h,2h,2h-perfluorohexanesulfon", "FTS_4_2",
  "^1h,1h,2h,2h-perfluorooctanesulfon", "FTS_6_2",
  "^1h,1h,2h,2h-perfluorodecanesulfon", "FTS_8_2",
  "^1h,1h,2h,2h-perfluorododecanesulfon", "FTS_10_2",
  "^1-?hexanesulfonicacid,3,3,4,4", "FTS_4_2",
  "^1-?octanesulfonicacid,3,3,4,4", "FTS_6_2",
  "^1-?decanesulfonicacid,3,3,4,4", "FTS_8_2",
  "^3-perfluoropropylpropano", "FTCA_3_3",
  "^3-perfluoropentylpropano", "FTCA_5_3",
  "^3-perfluoroheptylpropano", "FTCA_7_3",
  "^octanoicacid,4,4,5,5", "FTCA_5_3",
  "^decanoicacid,4,4,5,5", "FTCA_7_3",
  "^2h,2h,3h,3h-perfluorooctano", "FTCA_5_3",
  "hexafluoropropyleneoxide|^perfluoro\\(2-propoxypropano", "HFPO_DA",
  "^4,8-dioxa-3h-perfluorononano|adona", "ADONA",
  "^9-chlorohexadecafluoro", "PF3ONS_9Cl",
  "^11-chloroeicosafluoro", "PF3OUdS_11Cl",
  "^perfluoro-3,6-dioxaheptano|^methylperfluoro-3,6-dioxaheptano", "NFDHA",
  "^perfluoro\\(2-ethoxyethane\\)sulfon", "PFEESA",
  "^perfluoro-3-methoxypropano", "PFMPA",
  "^perfluoro\\(4-methoxybutano", "PFMBA",
  "^perfluorovaleric", "PFPeA",
  "^pfoaion|^pfoa$", "PFOA",
  "^pfosion|^pfos$", "PFOS",
  "^4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-pentadecafluorod", "FTCA_7_3",
  "^bis\\(3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,10", "diPAP_8_2",
  "^1,1,2,2-tetrafluoro-2-\\(pentafluoroethox", "PFEESA",
  "^cyclohexanesulfonicacid,1,2,2,3,3,4,5,5,6,6-decafluoro", "PFECHS",
  "^perfluoropalmitic", "PFHxDA",
  "^perfluorostearic", "PFODA",
  "hexaflumuron|flubendiamide|^perfluoro\\(4-isopropyltoluene", "NONTARGET"
)

carb <- c(propan = "Pr", butan = "B", pentan = "Pe", hexan = "Hx", heptan = "Hp", octan = "O",
          nonan = "N", decan = "D", undecan = "UnD", dodecan = "DoD",
          tridecan = "TrD", tetradecan = "TeD", hexadecan = "HxD", octadecan = "OD")
sulf <- c(propane = "Pr", butane = "B", pentane = "Pe", hexane = "Hx", heptane = "Hp", octane = "O",
          nonane = "N", decane = "D", undecane = "Un", dodecane = "Do")

first_group <- function(x, pat) {            # first capture group, or NA
  m <- regmatches(x, regexec(pat, x))
  vapply(m, function(z) if (length(z) > 1) z[2] else NA_character_, character(1))
}

to_compound <- function(name) {
  nm  <- tolower(gsub("[[:space:]]", "", name))
  nm  <- gsub("-1-", "", nm, fixed = TRUE)
  lab <- grepl("13c|18o|-d[0-9]+|^d[0-9]+-", nm)
  top <- grepl("plustotaloxidizableprecursors", nm)
  nm  <- sub("plustotaloxidizableprecursors.*$", "", nm)
  
  nm <- sub("^(potassium|ammonium|sodium|lithium)", "", nm)              # salt forms
  
  # CAS-style names for the fully fluorinated acids -> perfluoro<chain> form
  nm <- sub("^1-?([a-z]+ane)sulfonamide,1,1,2,2.*$", "perfluoro\\1sulfonamide", nm)
  nm <- sub("^1-?([a-z]+ane)sulfonicacid,1,1,2,2.*$", "perfluoro\\1sulfonicacid", nm)
  nm <- sub("^([a-z]+an)oicacid,2,2,3,3.*$", "perfluoro\\1oicacid", nm)
  
  out <- rep(NA_character_, length(nm))
  for (i in seq_len(nrow(compound_rules))) {          # explicit rules, first match wins
    hit <- is.na(out) & grepl(compound_rules$pattern[i], nm)
    out[hit] <- compound_rules$compound[i]
  }
  
  ratio <- sub(":", "_", first_group(nm, "([0-9]+:[0-9]+)"))               # 6:2 -> 6_2
  stem_c <- first_group(nm, paste0("^perfluoro(", paste(names(carb), collapse = "|"), ")(oate|oic)"))
  stem_s <- first_group(nm, paste0("^perfluoro(", paste(names(sulf), collapse = "|"), ")sulfon(ate|ic)"))
  
  out <- coalesce(
    out,
    case_when(grepl("sulfon|fts", nm) & !is.na(ratio)    ~ paste0("FTS_", ratio),
              grepl("carboxyl|ftca", nm) & !is.na(ratio) ~ paste0("FTCA_", ratio),
              TRUE ~ NA_character_),
    ifelse(is.na(stem_c), NA_character_, paste0("PF", unname(carb[stem_c]), "A")),
    ifelse(is.na(stem_s), NA_character_, paste0("PF", unname(sulf[stem_s]), "S"))
  )
  
  out <- ifelse(top & !is.na(out), paste0(out, "_TOP"), out)
  out[lab] <- "LABELED_STD"
  out
}

# -----------------------------------------------------------------------------
# 2. APPLY AND REVIEW
# -----------------------------------------------------------------------------
long <- all_WQP_data %>% mutate(compound = to_compound(CharacteristicName))

# a) names no rule recognised: add a rule (or send them to me) and rerun
long %>% filter(is.na(compound)) %>%
  count(CharacteristicName, sort = TRUE) %>% as_tibble() %>% print(n = 50)

# b) the full name -> compound table, for a quick read-through
long %>% distinct(CharacteristicName, compound) %>% arrange(compound, CharacteristicName) %>%
  write.csv("pfas_compound_map.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. WIDE TABLE
#    One row per site x date x fraction_group, one column per compound (ng/L).
#    TOP-assay and labelled-standard rows are left out of the main table.
#    0  = non-detect with a limit under the threshold
#    NA = not measured, OR a non-detect whose limit was too high to trust
# -----------------------------------------------------------------------------

long <- long %>%
  filter(!is.na(compound), !grepl("_TOP$", compound), compound != "LABELED_STD", compound != "NONTARGET") %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate))

# if a site-date has several records for one compound (different labs, replicates,
# linear/branched isomers), keep the highest so a detection beats a non-detect
agg <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

cells <- long %>%
  group_by(MonitoringLocationIdentifier, ActivityStartDate, fraction_group, compound) %>%
  summarise(value = agg(ResultMeasureValue), n_records = n(), .groups = "drop")

message(sum(cells$n_records > 1), " of ", nrow(cells),
        " site-date-compound cells had more than one record")

site_xy <- long %>%
  distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>%
  distinct(MonitoringLocationIdentifier, .keep_all = TRUE)

wide <- cells %>%
  select(-n_records) %>%
  pivot_wider(names_from = compound, values_from = value) %>%
  left_join(site_xy, by = "MonitoringLocationIdentifier")

id_cols <- c("MonitoringLocationIdentifier", "LatitudeMeasure", "LongitudeMeasure",
             "ActivityStartDate", "fraction_group")
wide <- wide %>% select(all_of(id_cols), sort(setdiff(names(wide), id_cols)))

pfas_cols <- setdiff(names(wide), id_cols)                 
n_before  <- nrow(wide)                                         
wide <- wide %>% filter(if_any(all_of(pfas_cols), ~ !is.na(.x))) 
message(n_before - nrow(wide), " rows with no usable PFAS value removed")

dim(wide)
colSums(!is.na(wide[pfas_cols])) %>% sort(decreasing = TRUE)
saveRDS(wide, "pfas_wide.rds")
#write.csv(wide, "pfas_wide.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 6. MAP
# -----------------------------------------------------------------------------

USA <- ne_states(country = "united states of america", returnclass = "sf")

all_WQP_sf <- st_as_sf(wide,
                       coords = c("LongitudeMeasure", "LatitudeMeasure"),
                       crs = st_crs(4326))

# keep only points that fall inside a US state polygon
WQP_PFAS <- st_join(all_WQP_sf, USA, join = st_intersects, left = FALSE)
nrow(WQP_PFAS)

# crop to the lower 48
conus_bbox <- st_bbox(c(xmin = -125, xmax = -66, ymin = 20, ymax = 50), crs = st_crs(4326))
USA_conus  <- st_crop(USA, conus_bbox)
WQP_conus  <- st_crop(WQP_PFAS, conus_bbox)

ggplot() +
  geom_sf(data = USA_conus, fill = "white", color = "gray2") +
  geom_sf(data = WQP_conus, color = "black", fill = "red",
          size = 2, stroke = 0.5, shape = 21, na.rm = TRUE) +
  ggtitle("PFAS sampling locations in the WQP") +
  theme_classic()



# PFOA records inside the lower 48
pfoa_long <- long %>%
  filter(compound == "PFOA",
         between(LongitudeMeasure, -125, -66),
         between(LatitudeMeasure, 20, 50)) %>%
  mutate(yr = as.integer(format(ActivityStartDate, "%Y")))

# One row per site. n_dates counts distinct sampling dates, not records, so
# several fractions or replicates on one day don't inflate it.
pfoa_sites <- pfoa_long %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Longitude  = first(LongitudeMeasure),
            Latitude   = first(LatitudeMeasure),
            n_dates    = n_distinct(ActivityStartDate),
            n_years    = n_distinct(yr),
            first_date = min(ActivityStartDate),
            last_date  = max(ActivityStartDate),
            .groups = "drop") %>%
  mutate(span_years = as.numeric(last_date - first_date) / 365.25,
         span_bin   = cut(span_years, breaks = c(-Inf, 0, 1, 5, Inf),
                          labels = c("One date", "<= 1 yr", "1-5 yrs", "> 5 yrs")))

pfoa_sites %>% count(span_bin)     # how many sites are one-off vs repeat-sampled

map_theme <- theme_minimal(base_size = 10) +
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

# -----------------------------------------------------------------------------
# 1. Your map, coloured by how long each site was sampled
# -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = USA_conus, fill = "grey80", color = "grey70", linewidth = 0.2) +
  geom_point(
    data = pfoa_sites %>% arrange(span_bin),          # long-record sites drawn on top
    aes(x = Longitude, y = Latitude, color = span_bin),
    size = 0.8, alpha = 0.9
  ) +
  scale_color_viridis_d(option = "plasma", name = "Sampling span\nper site") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "PFOA: time between first and last sample at each site") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  map_theme

# -----------------------------------------------------------------------------
# 2. How many sites were sampled each year
# -----------------------------------------------------------------------------
pfoa_long %>%
  group_by(yr) %>%
  summarise(n_sites = n_distinct(MonitoringLocationIdentifier), .groups = "drop") %>%
  ggplot(aes(yr, n_sites)) +
  geom_col(fill = "grey30") +
  labs(title = "PFOA: sites sampled per year", x = NULL, y = "Sites") +
  theme_minimal(base_size = 12)

# -----------------------------------------------------------------------------
# 3. Map by time period (adjust the breaks after looking at plot 2)
# -----------------------------------------------------------------------------
period_sites <- pfoa_long %>%
  mutate(period = cut(yr, breaks = c(-Inf, 2010, 2019, 2022, Inf),
                      labels = c("<= 2015", "2016-2019", "2020-2022", "2023+"))) %>%
  distinct(MonitoringLocationIdentifier, period, LongitudeMeasure, LatitudeMeasure)

ggplot() +
  geom_sf(data = USA_conus, fill = "grey80", color = "grey70", linewidth = 0.2) +
  geom_point(data = period_sites,
             aes(x = LongitudeMeasure, y = LatitudeMeasure),
             size = 0.6, alpha = 0.8, color = "firebrick") +
  facet_wrap(~ period) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "PFOA: sites sampled in each period") +
  map_theme

# -----------------------------------------------------------------------------
# 4. Sampling dates at repeat-sampled sites (one row per site, one dot per date)
#    Restricted to sites with >= min_dates dates; thousands of rows are unreadable.
# -----------------------------------------------------------------------------
min_dates <- 5

repeat_sites <- pfoa_sites %>% filter(n_dates >= min_dates) %>% arrange(first_date)
message(nrow(repeat_sites), " sites with at least ", min_dates, " sampling dates")

pfoa_long %>%
  semi_join(repeat_sites, by = "MonitoringLocationIdentifier") %>%
  distinct(MonitoringLocationIdentifier, ActivityStartDate) %>%
  mutate(site = factor(MonitoringLocationIdentifier,
                       levels = rev(repeat_sites$MonitoringLocationIdentifier))) %>%
  ggplot(aes(ActivityStartDate, site)) +
  geom_point(size = 0.4, alpha = 0.7) +
  labs(title = paste0("PFOA: sampling dates at sites with ", min_dates, "+ dates"),
       x = NULL, y = "Sites (ordered by first sample)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())





major_compounds <- c("PFOA", "PFOS")
nd_floor <- 0.5    # ng/L: where non-detects (stored as 0) are drawn on the log axis

# Top 4 sites by number of sampling dates with a usable PFOA or PFOS value.
# Grouped by site ID rather than lat/lon, and with_ties = FALSE so it is exactly 4.
top_4_sites <- wide %>%
  filter(!is.na(PFOA) | !is.na(PFOS)) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Latitude  = first(LatitudeMeasure),
            Longitude = first(LongitudeMeasure),
            n_obs     = n_distinct(ActivityStartDate),
            .groups = "drop") %>%
  slice_max(n_obs, n = 12, with_ties = FALSE) %>%
  mutate(site_id = paste0(MonitoringLocationIdentifier, "\n(",
                          round(Latitude, 2), ", ", round(Longitude, 2), ")\nn=", n_obs))

top_sites_data <- wide %>%
  inner_join(top_4_sites %>% select(MonitoringLocationIdentifier, site_id),
             by = "MonitoringLocationIdentifier") %>%
  select(site_id, sample_date = ActivityStartDate, fraction_group, all_of(major_compounds)) %>%
  pivot_longer(all_of(major_compounds), names_to = "compound", values_to = "concentration") %>%
  filter(!is.na(concentration)) %>%
  mutate(
    site_id   = factor(site_id, levels = top_4_sites$site_id),        # busiest site first
    status    = if_else(concentration == 0, "Non-detect", "Detected"),
    conc_plot = if_else(concentration == 0, nd_floor, concentration)  # zeros can't go on a log axis
  )

ggplot(top_sites_data, aes(x = sample_date, y = conc_plot, color = compound)) +
  geom_line(aes(linetype = fraction_group,
                group = interaction(compound, fraction_group)),      # don't join across fractions
            linewidth = 0.8, alpha = 0.7) +
  geom_point(aes(shape = status), size = 2, alpha = 0.6) +
  facet_wrap(~site_id, scales = "free_y", ncol = 2) +
  scale_color_viridis_d(option = "turbo") +
  scale_shape_manual(values = c("Detected" = 16, "Non-detect" = 1), name = NULL) +
  scale_y_log10() +
  labs(
    title = "PFOA and PFOS Over Time at the Top 4 Monitoring Sites",
    x = "Sample Date",
    y = "Concentration (ng/L, log scale)",
    color = "Compound",
    linetype = "Fraction",
    caption = paste0("Open symbols are non-detects (limit < 1 ng/L), drawn at ", nd_floor, " ng/L")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )







# these columns are optional in `long`; if missing they count as one unknown value
for (nm in c("OrganizationIdentifier", "ResultAnalyticalMethod.MethodIdentifier")) {
  if (!nm %in% names(long)) { long[[nm]] <- NA_character_; message(nm, " not in long") }
}

keys      <- c("MonitoringLocationIdentifier", "ActivityStartDate", "fraction_group", "compound")
floor_val <- 0.5     # ng/L, where non-detects (0) are drawn on log axes

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

dup_cells <- cells %>% filter(n_records > 1) %>% select(all_of(keys))

dup_summary <- long %>%
  semi_join(dup_cells, by = keys) %>%
  group_by(across(all_of(keys))) %>%
  summarise(n         = n(),
            n_names   = n_distinct(CharacteristicName_raw),
            n_orgs    = n_distinct(OrganizationIdentifier),
            n_methods = n_distinct(`ResultAnalyticalMethod.MethodIdentifier`),
            v_min     = safe_min(ResultMeasureValue),
            v_max     = safe_max(ResultMeasureValue),
            .groups = "drop") %>%
  mutate(why = case_when(n_names > 1   ~ "Different name strings",
                         n_orgs > 1    ~ "Different organizations",
                         n_methods > 1 ~ "Different methods",
                         TRUE          ~ "Same name, org and method"))
# `why` is the first reason that applies; a cell can have more than one

# -----------------------------------------------------------------------------
# Numbers first: how far apart are the records within a cell?
# fold = highest / lowest value (non-detects counted as floor_val)
# -----------------------------------------------------------------------------
dup_summary %>%
  filter(v_max > 0) %>%
  mutate(fold = v_max / pmax(v_min, floor_val)) %>%
  group_by(why) %>%
  summarise(cells = n(),
            median_fold   = median(fold, na.rm = TRUE),
            pct_within_2x = round(100 * mean(fold <= 2, na.rm = TRUE), 1),
            .groups = "drop")

# -----------------------------------------------------------------------------
# 1. What kind of duplicates are they?
# -----------------------------------------------------------------------------
dup_summary %>%
  count(why) %>%
  ggplot(aes(reorder(why, n), n)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  labs(title = "Cells with more than one record: what differs between the records",
       x = NULL, y = "Site-date-compound cells") +
  theme_minimal(base_size = 12)

# -----------------------------------------------------------------------------
# 2. Do the records agree? Lowest vs highest value per cell (1:1 line dashed).
#    Points on the left edge are cells where one record is a non-detect and the
#    other is a detection.
# -----------------------------------------------------------------------------
dup_summary %>%
  filter(!is.na(v_min), !is.na(v_max), v_max > 0) %>%
  mutate(lo = if_else(v_min == 0, floor_val, v_min)) %>%
  ggplot(aes(lo, v_max)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(alpha = 0.35, size = 1) +
  scale_x_log10() + scale_y_log10() +
  coord_equal() +
  facet_wrap(~ why) +
  labs(title = "Agreement between records in the same cell",
       x = "Lowest value (ng/L, non-detects at 0.5)", y = "Highest value (ng/L)") +
  theme_minimal(base_size = 11)

dup_summary %>%
  filter(v_max > 0, v_max / pmax(v_min, floor_val) > 2) %>%
  count(why, nd_partner = v_min == 0)

# -----------------------------------------------------------------------------
# 3. Which compounds have the most duplicated cells?
# -----------------------------------------------------------------------------
cells %>%
  group_by(compound) %>%
  summarise(n_cells = n(), pct_dup = 100 * mean(n_records > 1), .groups = "drop") %>%
  filter(n_cells >= 100) %>%
  slice_max(pct_dup, n = 20) %>%
  ggplot(aes(reorder(compound, pct_dup), pct_dup)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  labs(title = "Share of cells with more than one record, by compound",
       x = NULL, y = "% of site-date cells (compounds with 100+ cells)") +
  theme_minimal(base_size = 12)

