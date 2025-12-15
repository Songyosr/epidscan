#' Set SatScan Executable Path
#'
#' Sets the path to the SatScan executable for the current session.
#'
#' @param path Character. Absolute path to SatScan executable or directory.
#'
#' @export
#' @examples
#' \dontrun{
#' set_satscan_path("C:/Progra~1/SaTScan/SaTScan.exe")
#' }
set_satscan_path <- function(path) {
    if (!file.exists(path)) {
        warning("Path does not exist: ", path)
    }
    options(epidscan.satscan_path = path)
    message("SatScan path set to: ", path)
}

#' Get SatScan Executable Path
#'
#' Retrieves the configured SatScan path.
#'
#' @return Character path or NULL if not set.
#' @export
get_satscan_path <- function() {
    getOption("epidscan.satscan_path")
}

#'
#' Creates a configuration list for SatScan analysis with default or custom parameters.
#'
#' @param analysis_type Integer. Analysis type (4 = Prospective Space-Time)
#' @param model_type Integer. Model type (0 = Discrete Poisson)
#' @param start_date Character. Start date in "YYYY/MM/DD" format
#' @param end_date Character. End date in "YYYY/MM/DD" format
#' @param max_spatial_size Numeric. Maximum spatial cluster size (% of population at risk)
#' @param max_temporal_size Integer. Maximum temporal cluster size (days)
#' @param case_file Character. Name of case file
#' @param pop_file Character. Name of population file
#' @param geo_file Character. Name of coordinates file
#' @param scan_areas Integer. Areas to scan (1 = High rates, 2 = Low rates, 3 = Both)
#' @param ... Additional SatScan parameters
#'
#' @return Named list of SatScan parameters suitable for ss.options()
#'
#' @examples
#' \dontrun{
#' params <- create_satscan_params(
#'     start_date = "2025/01/01",
#'     end_date = "2025/11/30",
#'     max_spatial_size = 25
#' )
#' }
#' @export
create_satscan_params <- function(
  analysis_type = 4, # Prospective Space-Time
  model_type = 0, # Discrete Poisson
  start_date = NULL,
  end_date = NULL,
  max_spatial_size = 25,
  max_temporal_size = 30,
  case_file = "lepto.cas",
  pop_file = "lepto.pop",
  geo_file = "lepto.geo",
  scan_areas = 1, # High rates
  ...
) {
    # Validate required parameters
    if (is.null(start_date) || is.null(end_date)) {
        stop("start_date and end_date are required")
    }

    # Create base parameter list
    params <- list(
        # [Input]
        CaseFile = case_file,
        PopulationFile = pop_file,
        CoordinatesFile = geo_file,
        PrecisionCaseTimes = 3, # Day precision
        StartDate = start_date,
        EndDate = end_date,
        CoordinatesType = 1, # Lat/Long

        # [Analysis]
        AnalysisType = analysis_type,
        ModelType = model_type,
        ScanAreas = scan_areas,
        TimeAggregationUnits = 3, # Day
        TimeAggregationLength = 1,

        # [Spatial/Temporal]
        MaxSpatialSizeInPopulationAtRisk = max_spatial_size,
        MaxTemporalSize = max_temporal_size,

        # [Output]
        ReportGiniClusters = "n",
        LogRunToHistoryFile = "n"
    )

    # Add any additional parameters from ...
    extra_params <- list(...)
    if (length(extra_params) > 0) {
        params <- c(params, extra_params)
    }

    return(params)
}

#' Validate SatScan Parameters
#'
#' Checks that parameter list contains required fields and valid values.
#'
#' @param params List. Parameter list created by create_satscan_params()
#'
#' @return TRUE if valid, stops with error message if invalid
#' @export
validate_satscan_params <- function(params) {
    required_fields <- c(
        "CaseFile", "PopulationFile", "CoordinatesFile",
        "StartDate", "EndDate", "AnalysisType", "ModelType"
    )

    missing_fields <- setdiff(required_fields, names(params))
    if (length(missing_fields) > 0) {
        stop("Missing required parameters: ", paste(missing_fields, collapse = ", "))
    }

    # Validate date format (basic check)
    date_pattern <- "^\\d{4}/\\d{2}/\\d{2}$"
    if (!grepl(date_pattern, params$StartDate)) {
        stop("StartDate must be in YYYY/MM/DD format")
    }
    if (!grepl(date_pattern, params$EndDate)) {
        stop("EndDate must be in YYYY/MM/DD format")
    }

    # Validate date order
    if (as.Date(params$StartDate, "%Y/%m/%d") >= as.Date(params$EndDate, "%Y/%m/%d")) {
        stop("StartDate must be before EndDate")
    }

    return(TRUE)
}

#' Write SatScan Parameters to File
#'
#' Writes parameter list to .prm file using rsatscan.
#'
#' @param params List. Parameter list created by create_satscan_params()
#' @param output_dir Character. Directory where .prm file will be written
#' @param project_name Character. Project name (without extension)
#'
#' @return Path to created .prm file
#'
#' @examples
#' \dontrun{
#' params <- create_satscan_params(start_date = "2025/01/01", end_date = "2025/11/30")
#' write_satscan_params(params, tempdir(), "lepto")
#' }
#' @export
write_satscan_params <- function(params, output_dir, project_name) {
    # Validate parameters first
    validate_satscan_params(params)

    # Load rsatscan if not already loaded
    if (!requireNamespace("rsatscan", quietly = TRUE)) {
        stop("rsatscan package is required. Please install it.")
    }

    # Reset ss.options and set new parameters
    rsatscan::ss.options(reset = TRUE)
    rsatscan::ss.options(params)

    # Write parameter file
    rsatscan::write.ss.prm(output_dir, project_name)

    prm_path <- file.path(output_dir, paste0(project_name, ".prm"))
    message("SatScan parameters written to: ", prm_path)

    return(prm_path)
}

#' Print SatScan Parameter Summary
#'
#' Displays a formatted summary of SatScan parameters for review.
#'
#' @param params List. Parameter list
#'
#' @return Invisible NULL (prints to console)
#' @export
print_satscan_summary <- function(params) {
    cat("=== SatScan Analysis Configuration ===\n\n")

    cat("Data Files:\n")
    cat("  Case File:       ", params$CaseFile, "\n")
    cat("  Population File: ", params$PopulationFile, "\n")
    cat("  Coordinates File:", params$CoordinatesFile, "\n\n")

    cat("Time Period:\n")
    cat("  Start Date:", params$StartDate, "\n")
    cat("  End Date:  ", params$EndDate, "\n\n")

    cat("Analysis Settings:\n")
    analysis_types <- c(
        "1" = "Purely Spatial", "2" = "Purely Temporal",
        "3" = "Retrospective Space-Time", "4" = "Prospective Space-Time"
    )
    model_types <- c(
        "0" = "Discrete Poisson", "1" = "Bernoulli",
        "2" = "Space-Time Permutation"
    )

    cat("  Analysis Type:", analysis_types[as.character(params$AnalysisType)], "\n")
    cat("  Model Type:   ", model_types[as.character(params$ModelType)], "\n")
    cat("  Scan Areas:   ", ifelse(params$ScanAreas == 1, "High Rates",
        ifelse(params$ScanAreas == 2, "Low Rates", "Both")
    ), "\n\n")

    cat("Cluster Parameters:\n")
    cat("  Max Spatial Size: ", params$MaxSpatialSizeInPopulationAtRisk, "% of population\n")
    cat("  Max Temporal Size:", params$MaxTemporalSize, "days\n\n")

    cat("=====================================\n")

    invisible(NULL)
}

#' Read SatScan Parameter File
#'
#' Reads a .prm file and returns a named list of parameters.
#'
#' @param prm_path Character. Path to the .prm file
#'
#' @return Named list of parameters
#' @export
read_satscan_prm <- function(prm_path) {
    if (!file.exists(prm_path)) stop("PRM file not found: ", prm_path)

    lines <- readLines(prm_path)
    params <- list()

    for (line in lines) {
        # Skip comments and section headers
        if (grepl("^;", line) || grepl("^\\[.*\\]", line) || nchar(trimws(line)) == 0) {
            next
        }

        # Parse Key=Value
        if (grepl("=", line)) {
            parts <- strsplit(line, "=", fixed = TRUE)[[1]]
            key <- trimws(parts[1])
            value <- trimws(paste(parts[-1], collapse = "="))

            # Remove inline comments
            if (grepl(";", value)) {
                value <- trimws(strsplit(value, ";")[[1]][1])
            }

            params[[key]] <- value
        }
    }

    return(params)
}
