test_that("Real Data Workflow with New API", {
    skip_on_cran()
    # library(rsatscan) # Removed

    # 1. Load Data
    cases_path <- system.file("extdata", "cases_prepared.rds", package = "epidscan")
    if (cases_path == "") skip("cases_prepared.rds not found")
    cases_raw <- readRDS(cases_path)

    pop_path <- system.file("extdata", "population_cache", "thailand_pop_2024.rds", package = "epidscan")
    pop_path2 <- system.file("extdata", "population_cache", "thailand_pop_2025.rds", package = "epidscan")
    if (pop_path == "") skip("thailand_pop_2024.rds not found")
    pop_raw <- readRDS(pop_path)

    if (file.exists(pop_path2)) {
        pop_raw2 <- readRDS(pop_path2)
        pop_raw <- dplyr::bind_rows(pop_raw, pop_raw2)
    }

    # Check for shapefile
    # shp_files <- list.files(system.file("extdata", package="epidscan"), pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    # if (length(shp_files) == 0) skip("Shapefile not found in extdata")
    # shp_path <- shp_files[1]

    shp_path <- "/Users/tonn/Library/CloudStorage/GoogleDrive-s.rajborirug@gmail.com/My Drive/research/lepto/data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp"
    if (!file.exists(shp_path)) skip(paste0("Shapefile not found at ", shp_path))

    # 2. Data Preparation

    # A. Cases: Aggregate by location and Date
    cases <- cases_raw |>
        dplyr::mutate(
            date = as.Date(onset_date),
            location_id = as.character(location_id)
        ) |>
        dplyr::filter(lubridate::year(date) >= 2024) |>
        dplyr::group_by(location_id, date) |>
        dplyr::summarise(cases = dplyr::n(), .groups = "drop")

    # B. Population: Aggregate
    pop_cols <- grep("^(male|female)_g", names(pop_raw), value = TRUE)
    pop_agg <- pop_raw |>
        dplyr::rowwise() |>
        dplyr::mutate(total_pop = sum(dplyr::c_across(dplyr::all_of(pop_cols)), na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            tambon_code = substr(areacode, 1, 6),
            year = as.numeric(as.character(year))
        ) |>
        dplyr::group_by(tambon_code, year) |>
        dplyr::summarise(pop = sum(total_pop, na.rm = TRUE), .groups = "drop") |>
        dplyr::rename(location_id = tambon_code)

    # C. Geo
    shp <- sf::st_read(shp_path, quiet = TRUE)
    geo_sf <- shp |>
        dplyr::mutate(
            location_id = gsub("^TH", "", ADM3_PCODE)
        ) |>
        dplyr::select(location_id)

    # Intersection
    valid_locs <- intersect(unique(cases$location_id), unique(pop_agg$location_id))
    valid_locs <- intersect(valid_locs, unique(geo_sf$location_id))

    cases <- cases |> dplyr::filter(location_id %in% valid_locs)
    pop_agg <- pop_agg |> dplyr::filter(location_id %in% valid_locs)
    geo_sf <- geo_sf |> dplyr::filter(location_id %in% valid_locs)

    # 3. USE NEW API

    # Prep Objects
    cas_obj <- prep_cas(cases, loc_id = location_id, time = date, cases = cases, time_precision = "month")
    pop_obj <- prep_pop(pop_agg, loc_id = location_id, time = year, pop = pop)
    geo_obj <- prep_geo(geo_sf, loc_id = location_id)

    # SatScan Path
    # Check common locations
    ss_path <- "/Applications/SaTScan.app/Contents/MacOS/SaTScan"
    if (!file.exists(ss_path)) skip("SaTScan executable not found")
    set_satscan_path(ss_path)

    # Run
    # Use temporary dir for safety
    # Run
    # Use temporary dir for safety
    res <- satscanr(
        cas = cas_obj,
        pop = pop_obj,
        geo = geo_obj,
        AnalysisType = 4, # Prospective Space-Time
        ModelType = 0, # Poisson
        ProspectiveStartDate = "2025/01/01",
        verbose = TRUE,
        # Short run for testing
        MonteCarloReps = 0
    )

    expect_s3_class(res, "satscan_result")

    # Check that we got results
    # SaTScan completed, so cluster_summary should exist (even if empty rows if no clusters, but usually non-null)
    expect_false(is.null(res$cluster_summary))
    expect_false(is.null(res$location_summary))

    # Show results for user demo
    print(res)
    print(summary(res))
})

test_that("Real Data: Template + Tweak Hierarchy", {
    skip_on_cran()

    # 1. Reuse Data Prep (Copied from above for independence, or we can rely on file existence)
    # Ideally we'd wrap setup in a helper, but for now we trust previous test passed or skip if data missing.
    cases_path <- system.file("extdata", "cases_prepared.rds", package = "epidscan")
    if (cases_path == "") skip("cases_prepared.rds not found")
    cases <- readRDS(cases_path) |>
        dplyr::mutate(date = as.Date(onset_date), location_id = as.character(location_id)) |>
        dplyr::filter(lubridate::year(date) >= 2024) |>
        dplyr::group_by(location_id, date) |>
        dplyr::summarise(cases = dplyr::n(), .groups = "drop")

    pop_path <- system.file("extdata", "population_cache", "thailand_pop_2024.rds", package = "epidscan")
    if (pop_path == "") skip("pop not found")
    pop_raw <- readRDS(pop_path)
    pop_cols <- grep("^(male|female)_g", names(pop_raw), value = TRUE)
    pop_agg <- pop_raw |>
        dplyr::rowwise() |>
        dplyr::mutate(total_pop = sum(dplyr::c_across(dplyr::all_of(pop_cols)), na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate(location_id = substr(areacode, 1, 6), year = as.numeric(as.character(year))) |>
        dplyr::group_by(location_id, year) |>
        dplyr::summarise(pop = sum(total_pop, na.rm = TRUE), .groups = "drop")

    valid_locs <- intersect(unique(cases$location_id), unique(pop_agg$location_id))
    cases <- cases |> dplyr::filter(location_id %in% valid_locs)
    pop_agg <- pop_agg |> dplyr::filter(location_id %in% valid_locs)

    shp_path <- "/Users/tonn/Library/CloudStorage/GoogleDrive-s.rajborirug@gmail.com/My Drive/research/lepto/data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp"
    if (!file.exists(shp_path)) skip("Shapefile not found")
    # For geo we can just make a dummy if we are testing params?
    # But let's use real geo to be safe.
    geo_sf <- sf::st_read(shp_path, quiet = TRUE) |>
        dplyr::mutate(location_id = gsub("^TH", "", ADM3_PCODE)) |>
        dplyr::select(location_id) |>
        dplyr::filter(location_id %in% valid_locs)

    # Prep
    cas_obj <- prep_cas(cases, loc_id = location_id, time = date, cases = cases, time_precision = "month")
    pop_obj <- prep_pop(pop_agg, loc_id = location_id, time = year, pop = pop)
    geo_obj <- prep_geo(geo_sf, loc_id = location_id)

    ss_path <- "/Applications/SaTScan.app/Contents/MacOS/SaTScan"
    if (!file.exists(ss_path)) skip("SaTScan executable not found")
    set_satscan_path(ss_path)

    # 2. Verify: Template + Tweak
    # Create a complete PRM template with specific overrides for testing hierarchy
    library(epidscan)
    prm <- prm_defaults() # Get full template
    prm <- prm_set(prm, MaxTemporalSize = "10", MaxSpatialSizeInPopulationAtRisk = "10")
    prm_file <- tempfile(fileext = ".prm")
    prm_write(prm, prm_file)

    # Run with:
    # - prm_path (sets MaxTemp=10, MaxSpatial=10)
    # - Explicit Override: MaxSpatialSizeInPopulationAtRisk = 20 (User Tweak)
    # Expected Result:
    # - MaxTemporalSize = 10 (From PRM, overrides default 50, not overridden by user)
    # - MaxSpatialSize = 20 (From User, overrides PRM 10)

    res <- satscanr(
        cas = cas_obj, pop = pop_obj, geo = geo_obj,
        prm_path = prm_file,
        MaxSpatialSizeInPopulationAtRisk = 20, # OVERRIDE (Explicit name now)
        verbose = TRUE,
        MonteCarloReps = 0
    )

    # ASSERTIONS
    # Note: satscan binary output parsing might have type behavior nuances
    expect_equal(as.numeric(res$prm$MaxTemporalSize), 10)
    expect_equal(as.numeric(res$prm$MaxSpatialSizeInPopulationAtRisk), 20)
})
