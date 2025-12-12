#' Process Cases Module - Refactored with Composable Functions
#'
#' Processes case data for SatScan analysis using modular helper functions.
#' Supports both simple (univariate) and stratified (age/sex) modes.
#'
#' @param case_file Path to case data file (.xlsx, .csv, or .rds)
#' @param output_dir Output directory for processed files
#' @param start_date Analysis start date (YYYY-MM-DD)
#' @param end_date Analysis end date (YYYY-MM-DD)
#' @param mode Analysis mode: "simple" or "stratified"
#' @param date_col Name of date column (default: "onset_date")
#' @param strata_vars Optional strata variables (not used directly, kept for compatibility)
#' @param age_breaks Age group breaks for stratified mode (default: c(0,15,60,Inf))
#' @param age_labels Labels for age groups (default: c("0-14", "15-59", "60+"))
#' @param construct_location Logical. Build location_id from raw columns? (default: FALSE)
#' @param location_components Named list for location construction (if construct_location=TRUE)
#'   Example: list(prefix="chw_code", middle="amp_code", suffix="tmb_code")
#'
#' @return List with paths to output files
#'
#' @importFrom dplyr filter %>%
#' @importFrom utils read.table write.table
#' @export
process_cases <- function(case_file,
                          output_dir,
                          start_date,
                          end_date,
                          mode = "simple",
                          date_col = "onset_date",
                          strata_vars = NULL,
                          age_breaks = c(0, 15, 60, Inf),
                          age_labels = c("0-14", "15-59", "60+"),
                          construct_location = FALSE,
                          location_components = NULL) {
    message("=== Processing Case Data (", mode, " mode) ===")

    # Load data
    data_raw <- readRDS(case_file)
    message("Loaded ", nrow(data_raw), " cases.")

    # Build pipeline
    data <- data_raw

    # Step 1: Construct location ID if needed
    if (construct_location) {
        if (is.null(location_components)) {
            stop("location_components must be provided when construct_location=TRUE")
        }
        data <- build_location_code(
            data,
            prefix_col = location_components$prefix,
            middle_col = location_components$middle,
            suffix_col = location_components$suffix
        )
    }

    # Step 2: Ensure we have location_id column
    if (!"location_id" %in% names(data)) {
        stop("Data must have 'location_id' column or construct_location must be TRUE")
    }

    # Step 3: Standardize date
    if (!date_col %in% names(data)) stop("Date column '", date_col, "' not found")
    if (date_col != "date") {
        data$date <- as.Date(data[[date_col]])
    } else {
        data$date <- as.Date(data$date)
    }

    # Step 4: Filter by date range
    data <- data %>%
        filter(date >= as.Date(start_date) & date <= as.Date(end_date))
    message("After date filtering: ", nrow(data), " cases")

    # Step 5: Standardize sex (for stratified mode)
    if (mode == "stratified") {
        data <- standardize_sex(data)
    }

    # Step 6: Validate locations
    geo_lookup_path <- file.path(output_dir, "geo_lookup.rds")
    data <- validate_locations(data, geo_lookup_path)

    # Mode-specific processing
    if (mode == "simple") {
        # Aggregate by location + date
        aggregated <- aggregate_simple(data, location_col = "location_id", date_col = "date")

        # Save RDS
        saveRDS(aggregated, file.path(output_dir, "cases_aggregate.rds"))

        # Write SatScan file
        cas_path <- file.path(output_dir, "satscan.cas")
        write_satscan_cas(aggregated, cas_path, mode = "simple")

        return(list(
            cases_data = file.path(output_dir, "cases_aggregate.rds"),
            satscan_cas = cas_path
        ))
    } else if (mode == "stratified") {
        # Create age groups
        data <- create_age_groups(
            data,
            age_col = "age",
            breaks = age_breaks,
            labels = age_labels
        )

        # Aggregate by location + date + strata
        aggregated <- aggregate_stratified(
            data,
            location_col = "location_id",
            date_col = "date",
            strata_cols = c("age_group", "sex")
        )

        # Save RDS
        saveRDS(aggregated, file.path(output_dir, "cases_stratified.rds"))

        # Write SatScan file
        cas_path <- file.path(output_dir, "satscan.cas")
        write_satscan_cas(aggregated, cas_path, mode = "stratified")

        return(list(
            cases_data = file.path(output_dir, "cases_stratified.rds"),
            satscan_cas = cas_path
        ))
    } else {
        stop("mode must be 'simple' or 'stratified'")
    }
}
