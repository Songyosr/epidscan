#' Run SatScan Module
#'
#' Executes SatScan analysis using rsatscan package with tempdir() approach.
#'
#' @param input_dir Directory containing prepared input files (.cas, .pop, .geo)
#' @param output_dir Directory to save SatScan results
#' @param start_date Analysis start date (YYYY/MM/DD)
#' @param end_date Analysis end date (YYYY/MM/DD)
#' @param satscan_config Optional configuration (list or path to .prm file)
#' @param project_name Base name for SatScan files (default: "satscan")
#'
#' @importFrom rsatscan writes.cas writes.pop writes.geo ss.options write.ss.prm satscan
#' @importFrom utils read.table write.csv modifyList
#' @importFrom stats sd
#' @export
run_satscan_module <- function(input_dir = "data/derived",
                               output_dir = "output/satscan_results",
                               start_date = "2025/01/01",
                               end_date = "2025/12/31",
                               satscan_config = NULL,
                               project_name = "satscan") {
    message("=== Running SatScan Analysis ===")

    # Convert dates from YYYY-MM-DD to YYYY/MM/DD if needed
    start_date <- gsub("-", "/", start_date)
    end_date <- gsub("-", "/", end_date)

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # Define input file paths
    cas_file <- file.path(input_dir, "satscan.cas")
    pop_file <- file.path(input_dir, "satscan.pop")
    geo_file <- file.path(input_dir, "satscan.geo")

    # Check files exist
    if (!file.exists(cas_file)) stop("Case file missing: ", cas_file)
    if (!file.exists(pop_file)) stop("Pop file missing: ", pop_file)
    if (!file.exists(geo_file)) stop("Geo file missing: ", geo_file)

    message("Input files validated")

    # Use unique temporary directory for this run
    working_dir <- tempfile(pattern = "satscan_run_")
    dir.create(working_dir)
    message("Working directory: ", working_dir)

    # Read and prepare files using rsatscan functions
    message("Preparing input files...")

    # Read case file
    case_data <- utils::read.table(cas_file, fill = TRUE)
    # Determine number of columns
    n_cols <- ncol(case_data)
    if (n_cols == 3) {
        # Simple mode: location_id, cases, date
        names(case_data) <- c("location_id", "cases", "date")
    } else if (n_cols == 5) {
        # Stratified mode: location_id, cases, date, age_group, sex
        names(case_data) <- c("location_id", "cases", "date", "strata1", "strata2")
    }

    # Read population file
    pop_data <- utils::read.table(pop_file)
    if (ncol(pop_data) == 3) {
        names(pop_data) <- c("location_id", "year", "pop")
    } else {
        names(pop_data) <- c("location_id", "year", "pop", "strata1", "strata2")
    }

    # Read geo file
    geo_data <- utils::read.table(geo_file)
    names(geo_data) <- c("location_id", "lat", "long")

    # Write files to working directory using rsatscan functions
    rsatscan::write.cas(case_data, working_dir, project_name)
    rsatscan::write.pop(pop_data, working_dir, project_name)
    rsatscan::write.geo(geo_data, working_dir, project_name)

    message("Files prepared in working directory")

    # Set SatScan parameters
    rsatscan::ss.options(reset = TRUE)

    # Default parameters
    params <- list(
        CaseFile = paste0(project_name, ".cas"),
        PopulationFile = paste0(project_name, ".pop"),
        CoordinatesFile = paste0(project_name, ".geo"),
        PrecisionCaseTimes = 3, # Day
        StartDate = start_date,
        EndDate = end_date,
        CoordinatesType = 1, # Lat/Long
        AnalysisType = 4, # Prospective Space-Time
        ModelType = 0, # Discrete Poisson
        ScanAreas = 1, # High rates
        TimeAggregationUnits = 3, # Day
        TimeAggregationLength = 1,
        MaxSpatialSizeInPopulationAtRisk = 50,
        MaxTemporalSize = 30,
        ReportGiniClusters = "n",
        LogRunToHistoryFile = "n"
    )

    # Apply user configuration
    if (!is.null(satscan_config)) {
        if (is.list(satscan_config)) {
            # Merge list: user config overrides defaults
            message("Applying user-provided configuration list...")
            params <- utils::modifyList(params, satscan_config)
        } else if (is.character(satscan_config) && file.exists(satscan_config)) {
            # Read external PRM file
            message("Reading configuration from PRM file: ", satscan_config)
            external_params <- read_satscan_prm(satscan_config)

            # Merge defaults with external params (external overrides defaults)
            params <- utils::modifyList(params, external_params)

            # We MUST override file paths to match what the pipeline generated
            # But we keep all other statistical settings from the external file
            params <- utils::modifyList(params, list(
                CaseFile = paste0(project_name, ".cas"),
                PopulationFile = paste0(project_name, ".pop"),
                CoordinatesFile = paste0(project_name, ".geo"),
                StartDate = start_date, # Ensure dates match pipeline scope
                EndDate = end_date
            ))
        } else {
            warning("Invalid satscan_config provided. Using defaults.")
        }
    }

    # Set SatScan parameters
    rsatscan::ss.options(reset = TRUE)
    rsatscan::ss.options(params)

    # Write parameter file to working directory
    message("Writing parameter file...")
    rsatscan::write.ss.prm(working_dir, project_name)

    # Determine SatScan executable path
    ss_path <- get_satscan_path()

    if (is.null(ss_path)) {
        # Fallback to checking default locations if not configured
        ss_paths <- c(
            "/Applications/SaTScan.app/Contents/app",
            "/Applications/SaTScan.app/Contents/MacOS/SaTScan"
        )
        for (path in ss_paths) {
            if (file.exists(path) || dir.exists(path)) {
                ss_path <- path
                break
            }
        }
    }

    if (is.null(ss_path)) {
        warning("SatScan executable not found. Please use set_satscan_path() to configure it.")
        warning("Skipping SatScan execution. Files prepared in: ", output_dir)
        return(output_dir)
    }

    # Run SatScan
    message("Executing SatScan...")
    message("SatScan location: ", ss_path)

    results <- tryCatch(
        {
            rsatscan::satscan(
                working_dir,
                project_name,
                sslocation = ss_path,
                ssbatchfilename = "satscan",
                cleanup = FALSE, # Keep files for debugging
                verbose = FALSE
            )
        },
        error = function(e) {
            warning("SatScan execution failed: ", e$message)
            return(NULL)
        }
    )

    # Process and save results
    if (!is.null(results)) {
        message("✓ SatScan completed successfully")

        # Save results object
        saveRDS(results, file.path(output_dir, "satscan_results.rds"))

        # Check for clusters
        if (is.list(results) && !is.null(results$col) && is.data.frame(results$col) && nrow(results$col) > 0) {
            message("Clusters detected: ", nrow(results$col))
            if ("P_VALUE" %in% names(results$col)) {
                sig_clusters <- sum(results$col$P_VALUE < 0.05, na.rm = TRUE)
                message("  Significant (p < 0.05): ", sig_clusters)
            }

            # Save cluster data
            utils::write.csv(results$col, file.path(output_dir, "clusters.csv"), row.names = FALSE)
            message("Cluster data saved to: ", file.path(output_dir, "clusters.csv"))
        } else {
            message("No clusters detected")
        }

        # Copy parameter file to output directory for reference
        file.copy(
            file.path(working_dir, paste0(project_name, ".prm")),
            file.path(output_dir, paste0(project_name, ".prm")),
            overwrite = TRUE
        )

        message("Results saved to: ", output_dir)
    } else {
        message("SatScan did not return results")
    }

    return(output_dir)
}
