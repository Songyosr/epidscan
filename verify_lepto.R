# Verification script: Run epid_satscan with leptospirosis data
# and display detailed results for manual review

library(dplyr)
library(tidyr)
library(sf)
library(rsatscan)  # Required for ssenv
devtools::load_all(".")

# --- Load Case Data ---
cases <- readRDS("data/cases_prepared.rds")
message("Loaded ", nrow(cases), " case records")
message("Date range: ", min(cases$date), " to ", max(cases$date))

# --- Load Population Data ---
pop_raw <- readRDS("data/population_cache/thailand_pop_2024.rds")

# Calculate total population per area (sum all age groups)
pop <- pop_raw |>
  rowwise() |>
  mutate(
    total_pop = sum(c_across(starts_with("male_")), c_across(starts_with("female_")), na.rm = TRUE)
  ) |>
  ungroup() |>
  # Extract tambon code (first 6 digits of areacode which is 8-digit village code)
  mutate(location_id = substr(areacode, 1, 6)) |>
  group_by(location_id) |>
  summarise(pop = sum(total_pop, na.rm = TRUE), .groups = "drop")

message("Loaded population data for ", nrow(pop), " locations")

# --- Load Geographic Data ---
geo <- st_read("data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp", quiet = TRUE) |>
  filter(ADM1_PCODE == "TH90") |>  # Songkhla province
  mutate(tambon_code = gsub("^TH", "", ADM3_PCODE)) |>
  select(tambon_code, ADM3_EN, geometry)

message("Loaded ", nrow(geo), " geographic boundaries")

# --- Prepare Master Data ---
# Get unique location-date combinations
dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
locations <- unique(geo$tambon_code)

# Create full grid
full_grid <- expand.grid(
  location_id = locations,
  date = dates,
  stringsAsFactors = FALSE
) |>
  mutate(year = as.integer(format(date, "%Y")))

# Aggregate cases by location and date (location_id already exists in cases)
cases_agg <- cases |>
  group_by(location_id, date) |>
  summarise(cases = n(), .groups = "drop") |>
  mutate(location_id = as.character(location_id))

# Get centroids for each location
geo_centroids <- geo |>
  mutate(
    centroid = st_centroid(geometry),
    lat = st_coordinates(centroid)[, 2],
    long = st_coordinates(centroid)[, 1]
  ) |>
  st_drop_geometry() |>
  select(tambon_code, lat, long) |>
  mutate(location_id = as.character(tambon_code))

# Join everything
master_data <- full_grid |>
  left_join(cases_agg, by = c("location_id", "date")) |>
  left_join(pop, by = "location_id") |>
  left_join(geo_centroids, by = "location_id") |>
  mutate(cases = replace_na(cases, 0L)) |>
  filter(!is.na(pop), !is.na(lat), !is.na(long))

message("Master data prepared: ", nrow(master_data), " rows")
message("Total cases: ", sum(master_data$cases))

# --- Run SatScan Analysis ---
message("\n=== Running SatScan Analysis ===\n")

# Set SatScan path (single argument: full path to executable)
set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")

result <- epid_satscan(
  data = master_data,
  id_col = location_id,       # Use unquoted column name (NSE)
  obs_col = cases,            # Use unquoted column name (NSE)
  pop_col = pop,              # Use unquoted column name (NSE)
  date_col = date,            # Use unquoted column name (NSE)
  lat_col = lat,              # Use unquoted column name (NSE)
  long_col = long,            # Use unquoted column name (NSE)
  type = "space-time",
  model = "poisson",
  time_precision = "day",
  MonteCarloReps = 999,  # Full Monte Carlo
  verbose = TRUE
)

# --- Display Results ---
message("\n=== ANALYSIS RESULTS ===\n")

if ("CLUSTER" %in% names(result)) {
  clusters_found <- result |>
    filter(!is.na(CLUSTER)) |>
    distinct(CLUSTER, P_VALUE, REL_RISK)
  
  message("Clusters detected: ", nrow(clusters_found))
  
  if (nrow(clusters_found) > 0) {
    print(clusters_found)
    
    # Show cluster membership
    cluster_summary <- result |>
      filter(!is.na(CLUSTER)) |>
      group_by(CLUSTER) |>
      summarise(
        n_locations = n_distinct(location_id),
        n_days = n_distinct(date),
        total_cases = sum(cases),
        total_pop = sum(pop) / n_distinct(date),  # Avg pop
        p_value = first(P_VALUE),
        rel_risk = first(REL_RISK),
        .groups = "drop"
      ) |>
      arrange(CLUSTER)
    
    message("\nCluster Summary:")
    print(cluster_summary)
    
    # Save results for further analysis
    saveRDS(result, "verification_result.rds")
    message("\nFull results saved to verification_result.rds")
  }
} else {
  message("No CLUSTER column found in result.")
  print(names(result))
}
