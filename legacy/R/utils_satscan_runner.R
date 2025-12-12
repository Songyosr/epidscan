# SatScan Runner Functions
#
# This module provides wrapper functions for preparing and executing SatScan analyses.

#' Prepare SatScan Input Files
#'
#' Reads SatScan input files and writes them to a working directory using rsatscan.
#'
#' @param case_file Character. Path to case file (.cas)
#' @param pop_file Character. Path to population file (.pop)
#' @param geo_file Character. Path to coordinates file (.geo)
#' @param output_dir Character. Working directory for SatScan
#' @param project_name Character. Project name (files will be named project_name.cas, etc.)
#' @param start_date Character. Start date for filtering (optional, "YYYY/MM/DD")
#' @param end_date Character. End date for filtering (optional, "YYYY/MM/DD")
#'
#' @return Named list with paths to created files
#'
#' @examples
#' prepare_satscan_files(
#'     case_file = "data/satscan_case.cas",
#'     pop_file = "data/satscan_pop.pop",
#'     geo_file = "data/satscan_geo.geo",
#'     output_dir = tempdir(),
#'     project_name = "lepto"
#' )
prepare_satscan_files <- function(
    case_file,
    pop_file,
    geo_file,
    output_dir,
    project_name,
    start_date = NULL,
    end_date = NULL) {
    # Load rsatscan
    if (!requireNamespace("rsatscan", quietly = TRUE)) {
        stop("rsatscan package is required")
    }

    # Validate input files exist
    if (!file.exists(case_file)) stop("Case file not found: ", case_file)
    if (!file.exists(pop_file)) stop("Population file not found: ", pop_file)
    if (!file.exists(geo_file)) stop("Coordinates file not found: ", geo_file)

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Read input files
    case_data <- read.table(case_file, col.names = c("location_id", "cases", "date"))
    pop_data <- read.table(pop_file, col.names = c("location_id", "year", "pop"))
    geo_data <- read.table(geo_file, col.names = c("location_id", "lat", "long"))

    # Filter case data by date if specified
    if (!is.null(start_date) && !is.null(end_date)) {
        case_data <- case_data[case_data$date >= start_date & case_data$date <= end_date, ]
        message("Filtered cases to period: ", start_date, " to ", end_date)
        message("  Cases retained: ", nrow(case_data))
    }

    # Prepare data in rsatscan format
    case_formatted <- case_data[, c("location_id", "cases", "date")]
    pop_formatted <- pop_data[, c("location_id", "year", "pop")]
    geo_formatted <- geo_data[, c("location_id", "lat", "long")]

    # Write files using rsatscan
    rsatscan::write.cas(case_formatted, output_dir, project_name)
    rsatscan::write.pop(pop_formatted, output_dir, project_name)
    rsatscan::write.geo(geo_formatted, output_dir, project_name)

    # Return paths to created files
    files <- list(
        case = file.path(output_dir, paste0(project_name, ".cas")),
        pop = file.path(output_dir, paste0(project_name, ".pop")),
        geo = file.path(output_dir, paste0(project_name, ".geo"))
    )

    message("SatScan input files prepared in: ", output_dir)
    message("  - ", basename(files$case))
    message("  - ", basename(files$pop))
    message("  - ", basename(files$geo))

    return(files)
}

#' Run SatScan Analysis
#'
#' Executes SatScan analysis with error handling and validation.
#'
#' @param working_dir Character. Directory containing input files and .prm file
#' @param project_name Character. Project name (without extension)
#' @param satscan_path Character. Path to SatScan executable
#' @param cleanup Logical. Whether to clean up temporary files after execution
#' @param verbose Logical. Whether to print detailed output
#'
#' @return SatScan results object from rsatscan::satscan()
#'
#' @examples
#' results <- run_satscan_analysis(
#'     working_dir = tempdir(),
#'     project_name = "lepto",
#'     satscan_path = "/Applications/SaTScan.app/Contents/app"
#' )
run_satscan_analysis <- function(
    working_dir,
    project_name,
    satscan_path = "/Applications/SaTScan.app/Contents/app",
    cleanup = FALSE,
    verbose = FALSE) {
    # Load rsatscan
    if (!requireNamespace("rsatscan", quietly = TRUE)) {
        stop("rsatscan package is required")
    }

    # Validate working directory exists
    if (!dir.exists(working_dir)) {
        stop("Working directory does not exist: ", working_dir)
    }

    # Check that required files exist
    required_files <- c(
        paste0(project_name, ".cas"),
        paste0(project_name, ".pop"),
        paste0(project_name, ".geo"),
        paste0(project_name, ".prm")
    )

    for (f in required_files) {
        filepath <- file.path(working_dir, f)
        if (!file.exists(filepath)) {
            stop("Required file not found: ", filepath)
        }
    }

    # Run SatScan
    message("\n=== Running SatScan Analysis ===")
    message("Working directory: ", working_dir)
    message("Project: ", project_name)
    message("SatScan path: ", satscan_path)

    results <- tryCatch(
        {
            rsatscan::satscan(
                working_dir,
                project_name,
                sslocation = satscan_path,
                ssbatchfilename = "satscan",
                cleanup = cleanup,
                verbose = verbose
            )
        },
        error = function(e) {
            stop("SatScan execution failed: ", e$message)
        }
    )

    message("=== SatScan Analysis Complete ===\n")

    # Validate results
    if (is.null(results)) {
        warning("SatScan returned NULL results")
    } else {
        validate_satscan_results(results)
    }

    return(results)
}

#' Validate SatScan Results
#'
#' Checks that SatScan results object contains expected components.
#'
#' @param results SatScan results object
#'
#' @return TRUE if valid, prints warnings for missing components
validate_satscan_results <- function(results) {
    if (is.null(results)) {
        warning("Results object is NULL")
        return(FALSE)
    }

    expected_components <- c("col", "gis", "rr")
    missing <- setdiff(expected_components, names(results))

    if (length(missing) > 0) {
        warning("Missing expected result components: ", paste(missing, collapse = ", "))
    }

    # Check cluster results
    if (!is.null(results$col)) {
        if (nrow(results$col) == 0) {
            message("Note: No clusters detected")
        } else {
            message("Clusters detected: ", nrow(results$col))
            if ("P_VALUE" %in% names(results$col)) {
                sig_clusters <- sum(results$col$P_VALUE < 0.05, na.rm = TRUE)
                message("  Significant (p < 0.05): ", sig_clusters)
            }
        }
    }

    return(TRUE)
}

#' Save SatScan Results
#'
#' Saves SatScan results object and extracts cluster information to files.
#'
#' @param results SatScan results object
#' @param output_dir Character. Directory to save results
#' @param project_name Character. Project name for output files
#'
#' @return List of paths to saved files
save_satscan_results <- function(results, output_dir, project_name) {
    # Create output directory if needed
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # Save raw results object
    rds_path <- file.path(output_dir, paste0(project_name, "_results.rds"))
    saveRDS(results, rds_path)
    message("Saved results object to: ", rds_path)

    output_files <- list(rds = rds_path)

    # Extract and save cluster data
    if (!is.null(results$col) && nrow(results$col) > 0) {
        clusters <- results$col

        # Add significance flag
        if ("P_VALUE" %in% names(clusters)) {
            clusters$significant <- clusters$P_VALUE < 0.05
        }

        csv_path <- file.path(output_dir, paste0(project_name, "_clusters.csv"))
        write.csv(clusters, csv_path, row.names = FALSE)
        message("Saved cluster data to: ", csv_path)

        output_files$clusters <- csv_path
    } else {
        message("No clusters to save")
    }

    return(output_files)
}
