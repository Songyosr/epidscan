#' Process Population Data Module - Refactored
#'
#' Processes population data for SatScan analysis using modular helper functions.
#' Assumes population cache exists (use download_population_cache.R to create).
#'
#' @param years Numeric vector of years to load from cache
#' @param output_dir Output directory for processed files
#' @param mode Analysis mode: "simple" or "stratified"
#' @param spatial_extent Province codes vector or NULL (auto-detect from cases)
#' @param case_data Case data for auto-detection & majorityvote (optional)
#' @param age_breaks Age group breaks for stratified mode (default: c(0,15,60,Inf))
#' @param age_labels Labels for age groups (default: c("0-14", "15-59", "60+"))
#' @param year_select_fn User function for year selection (optional placeholder)
#' @param cache_dir Cache directory path (default: "data/population_cache")
#'
#' @return List with paths to output files
#'
#' @examples
#' # Simple mode with auto-detection
#' process_population(
#'     years = 2023:2025,
#'     output_dir = "data/derived",
#'     mode = "simple",
#'     case_data = "data/cases_prepared.rds"
#' )
#'
#' # Stratified mode with specific provinces
#' process_population(
#'     years = 2025,
#'     output_dir = "data/derived",
#'     mode = "stratified",
#'     spatial_extent = c("90", "91")
#' )
#'
#' @importFrom dplyr mutate group_by summarise %>%
#' @export
process_population <- function(years,
                               output_dir,
                               mode = "simple",
                               spatial_extent = NULL,
                               case_data = NULL,
                               age_breaks = c(0, 15, 60, Inf),
                               age_labels = c("0-14", "15-59", "60+"),
                               year_select_fn = NULL,
                               cache_dir = "data/population_cache") {
    message("=== Processing Population Data (", mode, " mode) ===")

    # Ensure output directory exists
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # 1. Validate cache
    if (!validate_cache(years, cache_dir)) {
        stop("Cache validation failed. See warnings above.")
    }

    # 2. Load from cache
    pop_data <- load_from_cache(years, cache_dir)

    # 3. Determine spatial extent
    if (is.null(spatial_extent)) {
        if (!is.null(case_data)) {
            spatial_extent <- detect_spatial_extent(case_data)
        } else {
            stop("Either spatial_extent or case_data must be provided")
        }
    }

    # 4. Filter to spatial extent
    pop_data <- filter_spatial_extent(pop_data, spatial_extent)

    # 5. Select year (majority vote or custom)
    pop_data <- select_year_majority(pop_data, case_data, year_select_fn)

    # 6. Mode-specific processing
    pop_data <- if (mode == "simple") {
        aggregate_by_location(pop_data)
    } else if (mode == "stratified") {
        pop_data <- decode_moph_to_long(pop_data, age_breaks, age_labels)

        # Aggregate to Tambon level (6 digits) to match geometry
        # This ensures compatibility with the ADM3 shapefile
        pop_data <- pop_data %>%
            mutate(location_id = substr(location_id, 1, 6)) %>%
            group_by(location_id, year, age_group, sex) %>%
            summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
    } else {
        stop("mode must be 'simple' or 'stratified'")
    }

    # 7. Validate
    pop_data <- validate_population_data(pop_data, mode)

    # 8. Save RDS
    rds_file <- if (mode == "simple") "pop_aggregate.rds" else "pop_stratified.rds"
    rds_path <- file.path(output_dir, rds_file)
    saveRDS(pop_data, rds_path)
    message("Saved: ", rds_path)

    # 9. Write SatScan file
    pop_path <- file.path(output_dir, "satscan.pop")
    write_satscan_pop(pop_data, pop_path, mode)

    return(list(
        pop_data = rds_path,
        satscan_pop = pop_path
    ))
}
