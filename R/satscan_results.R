# Internal helpers for SatScan execution and result parsing
# These functions are NOT exported - used by epid_satscan()

#' Run SatScan Analysis
#'
#' Executes SatScan via rsatscan::satscan()
#'
#' @param work_dir Working directory with input files
#' @param project_name Project name (default: "epid")
#' @param ss_location SatScan install location
#' @param ss_batch SatScan batch file name
#' @param final_opts Options list for ss.options()
#' @param verbose Print output?
#' @return satscan result object
#' @keywords internal
run_satscan <- function(work_dir, project_name = "epid",
                        ss_location, ss_batch, final_opts, verbose = FALSE) {
    if (verbose) message("Running SatScan via rsatscan...")

    # Set options in rsatscan environment
    rsatscan::ss.options(reset = TRUE)
    rsatscan::ss.options(final_opts)

    # Write PRM file
    rsatscan::write.ss.prm(work_dir, project_name)

    # Execute
    tryCatch(
        {
            rsatscan::satscan(
                work_dir,
                project_name,
                sslocation = ss_location,
                ssbatchfilename = ss_batch,
                verbose = verbose,
                cleanup = FALSE
            )
        },
        error = function(e) {
            warning("SatScan failed: ", e$message)
            NULL
        }
    )
}

#' Get macOS SatScan Path
#'
#' Handles macOS App Bundle path detection
#'
#' @param ss_full_path Full path to SatScan executable
#' @return List with ss_location and ss_batch
#' @keywords internal
get_macos_satscan_path <- function(ss_full_path) {
    ss_location <- dirname(ss_full_path)
    ss_batch <- basename(ss_full_path)

    # macOS App Bundle handling - use CLI binary, not GUI launcher
    if (Sys.info()["sysname"] == "Darwin" && ss_batch == "SaTScan") {
        if (basename(ss_location) == "MacOS") {
            alt_location <- file.path(dirname(ss_location), "app")
            alt_batch <- "satscan"
            if (file.exists(file.path(alt_location, alt_batch))) {
                ss_location <- alt_location
                ss_batch <- alt_batch
            }
        }
    }

    list(ss_location = ss_location, ss_batch = ss_batch)
}

#' Parse SatScan Results
#'
#' Extracts cluster info from rsatscan results and joins to original data
#'
#' @param ss_results Result from run_satscan()
#' @param data Original input data
#' @param id_quo Quosure for ID column
#' @param verbose Print debug info?
#' @return data.frame with CLUSTER, P_VALUE, REL_RISK columns added
#' @keywords internal
parse_results <- function(ss_results, data, id_quo, verbose = FALSE) {
    if (verbose) {
        message("SatScan Result Debug:")
        message("  ss_results$col class: ", paste(class(ss_results$col), collapse = ", "))
        message("  ss_results$gis class: ", paste(class(ss_results$gis), collapse = ", "))
    }

    # Check for GIS output
    if (is.null(ss_results) || is.null(ss_results$gis) || !is.data.frame(ss_results$gis)) {
        if (verbose) message("No valid cluster data found")
        return(data)
    }

    if (verbose) message("GIS data.frame found - extracting clusters")

    # Extract location-cluster mapping
    loc_cluster_map <- ss_results$gis |>
        dplyr::select(LOC_ID, CLUSTER) |>
        dplyr::mutate(LOC_ID = as.character(LOC_ID))

    # Get cluster stats from $gis (use CLU_RR for relative risk)
    cluster_info <- ss_results$gis |>
        dplyr::select(CLUSTER, P_VALUE, REL_RISK = CLU_RR) |>
        dplyr::distinct(CLUSTER, .keep_all = TRUE)

    # Join cluster info to location map
    full_map <- loc_cluster_map |>
        dplyr::left_join(cluster_info, by = "CLUSTER")

    # Add link ID to data
    if (!rlang::quo_is_null(id_quo)) {
        data$epid_link_id <- as.character(dplyr::pull(data, !!id_quo))
    } else {
        data$epid_link_id <- as.character(seq_len(nrow(data)))
    }

    # Join and clean up
    data |>
        dplyr::left_join(full_map, by = c("epid_link_id" = "LOC_ID")) |>
        dplyr::select(-epid_link_id)
}
