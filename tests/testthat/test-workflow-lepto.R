library(testthat)
library(dplyr)
library(sf)
library(epidscan)
library(readr)
library(lubridate)
library(tidyr)
library(rsatscan)

# This test file replicates the workflow from 02_SatScan_Lepto.Rmd
# but uses the new epid_satscan function and available data in data/

test_that("Full Leptospirosis Workflow Test", {
    # skip_on_cran()

    # 1. Load Data
    # Cases
    cases_path <- system.file("extdata", "cases_prepared.rds", package = "epidscan")
    if (cases_path == "") skip("cases_prepared.rds not found")
    cases_raw <- readRDS(cases_path)

    # Population
    pop_path <- system.file("extdata", "population_cache", "thailand_pop_2024.rds", package = "epidscan")
    if (pop_path == "") skip("thailand_pop_2024.rds not found")
    pop_raw <- readRDS(pop_path)

    # Shapefile for Geo
    shp_path <- system.file("extdata", "thai_bound", "tha_admbnda_adm3_rtsd_20220121.shp", package = "epidscan")
    if (shp_path == "") {
        skip("Shapefile not found")
    }
    shp <- st_read(shp_path, quiet = TRUE)

    # 2. Data Preparation

    # A. Cases: Aggregate by location (Tambon) and Date
    # Filter date range to valid 2024 data
    cases <- cases_raw |>
        mutate(
            date = as.Date(onset_date),
            location_id = as.character(location_id)
        ) |>
        filter(year(date) == 2024) |>
        group_by(location_id, date) |>
        summarise(cases = n(), .groups = "drop")

    # B. Population: Aggregate from Village (8 digit) to Tambon (6 digit)
    # Calculate total population per row first (sum of all gender/age cols)
    pop_cols <- grep("^(male|female)_g", names(pop_raw), value = TRUE)

    pop_agg <- pop_raw |>
        rowwise() |>
        mutate(total_pop = sum(c_across(all_of(pop_cols)), na.rm = TRUE)) |>
        ungroup() |>
        mutate(
            tambon_code = substr(areacode, 1, 6),
            year = as.numeric(as.character(year)) # Ensure numeric
        ) |>
        group_by(tambon_code, year) |>
        summarise(pop = sum(total_pop, na.rm = TRUE), .groups = "drop") |>
        rename(location_id = tambon_code)

    # C. Geo: Extract Centroids from Shapefile
    geo <- shp |>
        mutate(
            location_id = gsub("^TH", "", ADM3_PCODE), # Remove TH prefix
            centroid = st_centroid(geometry)
        ) |>
        mutate(
            lat = st_coordinates(centroid)[, 2],
            long = st_coordinates(centroid)[, 1]
        ) |>
        st_drop_geometry() |>
        select(location_id, lat, long) |>
        as.data.frame() # epid_satscan might expect DF

    # 3. Validation of IDs
    # Ensure locations in cases exist in geo and pop
    valid_locs <- intersect(geo$location_id, pop_agg$location_id)

    cases <- cases |> filter(location_id %in% valid_locs)
    pop_agg <- pop_agg |> filter(location_id %in% valid_locs)
    geo <- geo |> filter(location_id %in% valid_locs)

    expect_gt(nrow(cases), 0)
    expect_gt(nrow(pop_agg), 0)
    expect_gt(nrow(geo), 0)

    # Prepare Master Data (Full Grid)
    # This ensures Population is defined for every day (avoiding SatScan interpolation weirdness with sparse data)
    # and Cases are 0 where appropriate.

    unique_locs <- unique(geo$location_id)
    dates <- seq(as.Date("2024-01-01"), as.Date("2024-11-30"), by = "day")

    grid <- expand.grid(location_id = unique_locs, date = dates, stringsAsFactors = FALSE)

    # Join Cases
    # cases has (location_id, date, cases)

    # Join Pop and Geo
    # pop_agg (location_id, year, pop)
    # geo (location_id, lat, long)

    master_data <- grid |>
        as_tibble() |>
        mutate(year = year(date)) |>
        left_join(cases, by = c("location_id", "date")) |>
        mutate(cases = replace_na(cases, 0)) |>
        left_join(pop_agg, by = c("location_id", "year")) |>
        left_join(geo, by = "location_id") |>
        arrange(location_id, date)

    expect_equal(nrow(master_data), length(unique_locs) * length(dates))

    # 4. Run epid_satscan

    # Set SatScan path (Mac specific check)
    ss_path <- "/Applications/SaTScan.app/Contents/MacOS/SaTScan"
    if (file.exists(ss_path)) {
        set_satscan_path(ss_path)
    } else {
        skip("SatScan executable not found at default Mac location")
    }

    # Create a directory for output
    out_dir <- tempdir()

    # Using verbose=TRUE to see what happens
    result <- epid_satscan(
        data = master_data,
        type = "space-time",
        model = "poisson",
        pop_col = pop,
        # geo = geo, # REMOVE THIS: epid_satscan doesn't take 'geo' arg
        id_col = location_id,
        date_col = date,
        obs_col = cases,
        lat_col = lat, # ADD THIS
        long_col = long, # ADD THIS
        time_precision = "day",
        output_dir = out_dir,
        verbose = TRUE,
        # Extra params for ss.options
        MaxSpatialSizeInPopulationAtRisk = 25,
        MaxTemporalSize = 30,
        MonteCarloReps = 0 # Run fast without p-values for testing
    )

    # 5. Check Results
    # epid_satscan returns the input data (or simplified unique locations) joined with cluster results
    expect_s3_class(result, "data.frame")

    print("Result column names:")
    print(colnames(result))

    # Check if cluster columns were added
    if ("CLUSTER" %in% colnames(result)) {
        n_clusters <- sum(!is.na(result$CLUSTER))
        print(paste("Number of location-days in clusters:", n_clusters))

        if (n_clusters > 0) {
            print("Sample of cluster results:")
            print(head(result |> filter(!is.na(CLUSTER))))
        }
    } else {
        print("No CLUSTER column found in result - likely no clusters detected or join failed.")
    }
})
