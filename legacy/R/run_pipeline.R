#' Modular Pipeline Orchestrator
#'
#' This script manages the execution of the SatScan analysis pipeline.
#' Supports two modes:
#' - Simple: Direct SatScan analysis with raw population
#' - Stratified: Expectation modeling with covariates
#'
#' @param mode Character. "simple" or "stratified".
#' @param shapefile_path Character. Path to shapefile.
#' @param admin_level Character. Admin level (default "ADM3").
#' @param province_code Character. Optional province filter.
#' @param case_file Character. Path to case data.
#' @param case_location_col Character. Column name for location ID.
#' @param case_date_col Character. Column name for date.
#' @param start_date Character. Start date (YYYY-MM-DD).
#' @param end_date Character. End date (YYYY-MM-DD).
#' @param strata_vars Character vector. Stratification variables.
#' @param age_breaks Numeric vector. Age breaks.
#' @param age_labels Character vector. Age labels.
#' @param pop_years Numeric vector. Population years.
#' @param time_precision Character. "day", "week", "month", "year".
#' @param update_geo Logical. Force update geo data.
#' @param update_pop Logical. Force update pop data.
#' @param update_cases Logical. Force update case data.
#' @param run_model Logical. Run expectation model.
#' @param run_analysis Logical. Run SatScan.
#' @param satscan_config List. SatScan configuration.
#' @param output_dir Character. Output directory.
#'
#' @export

run_pipeline <- function(
    # Mode selection
    mode = c("simple", "stratified"),
    # Geographic parameters
    shapefile_path,
    admin_level = "ADM3",
    province_code = NULL,
    # Case parameters
    case_file,
    case_location_col = "location_id",
    case_date_col = "date",
    start_date,
    end_date,
    strata_vars = NULL,
    age_breaks = c(0, 15, 60, Inf),
    age_labels = c("0-14", "15-59", "60+"),
    # Population parameters
    pop_years,
    time_precision = c("day", "week", "month", "year"),
    # Update flags
    update_geo = FALSE,
    update_pop = FALSE,
    update_cases = TRUE,
    run_model = TRUE,
    run_analysis = TRUE,
    # SatScan Configuration
    satscan_config = NULL,
    # Output directory
    output_dir = "data/derived") {
    mode <- match.arg(mode)
    time_precision <- match.arg(time_precision)

    message("========================================")
    message("Starting Pipeline Execution")
    message("Mode: ", mode)
    message("Time Precision: ", time_precision)
    message("========================================")

    date_range <- c(start_date, end_date)

    # 1. Geographic Data (Static/Rare)
    if (update_geo) {
        message("\n[1/6] Processing Geographic Data...")
        process_geo(
            shp_path = shapefile_path,
            admin_level = admin_level,
            province_filter = province_code,
            output_dir = output_dir
        )
    } else {
        if (!file.exists(file.path(output_dir, "geo_lookup.rds"))) {
            warning("Geo data missing. Forcing update.")
            process_geo(
                shp_path = shapefile_path,
                admin_level = admin_level,
                province_filter = province_code,
                output_dir = output_dir
            )
        } else {
            message("\n[1/6] Skipping Geographic Data (already exists)")
        }
    }

    # 2. Population Data
    if (update_pop) {
        message("\n[2/6] Processing Population Data...")

        process_population(
            years = pop_years,
            output_dir = output_dir,
            mode = mode,
            spatial_extent = if (!is.null(province_code)) province_code else NULL,
            case_data = case_file, # For auto-detection & majority vote
            age_breaks = age_breaks,
            age_labels = age_labels,
            cache_dir = "data/population_cache"
        )
    } else {
        pop_file <- file.path(
            output_dir,
            if (mode == "simple") "pop_aggregate.rds" else "pop_stratified.rds"
        )
        if (!file.exists(pop_file)) {
            warning("Population data missing. Forcing update.")

            process_population(
                years = pop_years,
                output_dir = output_dir,
                mode = mode,
                spatial_extent = if (!is.null(province_code)) province_code else NULL,
                case_data = case_file,
                age_breaks = strata_vars,
                cache_dir = "data/population_cache"
            )
        } else {
            message("\n[2/6] Skipping Population Data (already exists)")
        }
    }

    # 3. Case Data
    if (update_cases) {
        message("\n[3/6] Processing Case Data...")
        process_cases(
            case_file = case_file,
            output_dir = output_dir,
            start_date = start_date,
            end_date = end_date,
            mode = mode,
            date_col = case_date_col,
            strata_vars = strata_vars,
            age_breaks = age_breaks,
            age_labels = age_labels,
            construct_location = (case_location_col != "location_id"),
            location_components = if (case_location_col == "tambon_code") {
                list(prefix = "chw_code", middle = "amp_code", suffix = "tmb_code")
            } else {
                NULL
            }
        )
    } else {
        message("\n[3/6] Skipping Case Data")
    }

    # 4. Expectation Modeling - REMOVED (moved to dev branch)
    # Stratified mode now directly feeds stratified .cas and .pop to SatScan
    message("\n[4/6] Skipping Expectation Model (removed from this branch)")

    # [5/6] Run SatScan
    if (run_analysis) {
        message("\n[5/6] Running SatScan Analysis...")
        satscan_output_dir <- file.path(output_dir, "satscan_results")
        geo_file <- file.path(output_dir, "geo_lookup.rds")

        run_satscan_module(
            input_dir = output_dir, output_dir = satscan_output_dir,
            start_date = start_date, end_date = end_date,
            satscan_config = satscan_config
        )

        # 6. Post-Processing
        message("\n[6/6] Post-Processing Results...")
        post_process(
            results_dir = satscan_output_dir,
            geo_lookup_path = geo_file,
            output_dir = output_dir
        )
    } else {
        message("\n[5/6] Skipping SatScan Analysis")
        message("\n[6/6] Skipping Post-Processing")
    }

    message("\n========================================")
    message("Pipeline Execution Complete")
    message("========================================")
}

# Example Usage (commented out - uncomment to run):
# run_pipeline(
#   mode = "simple",
#   shapefile_path = "data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp",
#   province_code = "90",
#   case_file = "data/cases_raw.rds",  # Must have location_id or admin code columns
#   case_location_col = "tambon_code",  # Use "location_id" if already constructed
#   case_date_col = "onset_date",
#   start_date = "2025-01-01",
#   end_date = "2025-12-31",
#   pop_years = 2025,  # Year(s) to load from cache
#   strata_vars = c(0, 18, 60, Inf),  # For stratified mode
#   update_geo = FALSE,
#   update_pop = TRUE,
#   update_cases = TRUE,
#   run_analysis = TRUE
# )
