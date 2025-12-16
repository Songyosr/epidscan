# Internal helpers for SatScan execution and result parsing
# These functions are NOT exported - used by epid_satscan()

# Suppress R CMD check notes for NSE columns
utils::globalVariables(c("LOC_ID", "CLUSTER", "P_VALUE", "REL_RISK", "CLU_RR", "epid_link_id", "CLU_ODE", "id_char", "RR"))

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

#' Parse SatScan Output into S3 Object
#'
#' Creates a satscan_result S3 object independent of the original data frame's structure (tidy/long),
#' allowing for efficient storage and flexible access to location-summaries.
#'
#' @param ss_results Result from run_satscan()
#' @param data Original input data (tibble/df/sf)
#' @param geo_df Geometry data frame (id, lat, long) matching data rows
#' @param id_quo Quosure for ID column
#' @param output_dir Output directory (optional)
#' @param verbose Print debug info?
#' @param merge_time_series Logical. If TRUE, creates a huge joined data frame in user memory?
#'   Defaults to FALSE to save memory.
#' @return object of class "satscan_result"
#' @keywords internal
parse_satscan_output <- function(ss_results, data, geo_df, id_quo, output_dir = NULL,
                                 verbose = FALSE, merge_time_series = FALSE) {
    # 1. Handle NULL results (no clusters or error)
    if (is.null(ss_results) || is.null(ss_results$gis)) {
        if (verbose) message("No SatScan results found.")
        res <- list(
            main_results = NULL,
            location_summary = NULL,
            cluster_summary = NULL,
            raw_output = ss_results
        )
        class(res) <- "satscan_result"
        return(res)
    }

    if (verbose) message("Parsing SatScan results...")

    # 2. Create Cluster Summary (metadata for each cluster)
    # ss_results$col contains: CLUSTER, LOCATION_IDs, START_DATE, END_DATE, P_VALUE, etc.
    # It is usually the authoritative source for cluster-level stats.
    if (!is.null(ss_results$col)) {
        cluster_summary <- ss_results$col

        # Ensure standardized column names for REL_RISK (handling variation in SatScan versions)
        # Some output RR, some REL_RISK.
        if ("RR" %in% names(cluster_summary) && !"REL_RISK" %in% names(cluster_summary)) {
            cluster_summary <- dplyr::rename(cluster_summary, REL_RISK = RR)
        }
    } else {
        # Fallback: Extract from GIS if COL file is missing
        if (verbose) message("No .col file found - extracting cluster summary from GIS data")
        cluster_summary <- ss_results$gis |>
            dplyr::select(CLUSTER, P_VALUE, REL_RISK = CLU_RR, ODE = CLU_ODE) |>
            dplyr::distinct(CLUSTER, .keep_all = TRUE) |>
            dplyr::arrange(CLUSTER)
    }

    # 3. Create Location Summary (One row per location)
    # Join unique geometry with GIS results
    # geo_df has duplicated rows (time series), so we distinct it
    loc_geo <- geo_df |>
        dplyr::distinct(id, .keep_all = TRUE)

    # Check if original data was sf, if so, we might want to recover sf geometry?
    # But epid_satscan extracted geometry to a separate df.
    # If the user passed sf, `data` is sf.
    # We should probably return an sf object for location_summary if possible.
    is_sf_input <- inherits(data, "sf")

    # Join GIS results (LOC_ID) to our Location IDs (id)
    # ss_results$gis contains: LOC_ID, CLUSTER, OBSERVED, EXPECTED, ODE, etc.
    # LOC_ID in GIS matches our internal numeric ID if we generated one,
    # or the user provided ID if we passed that (currently epid_satscan passes numeric sequential ID usually or user ID?)
    # logic in epid_satscan:
    # id_vec <- as.character(dplyr::pull(data, !!id_quo))
    # geo_df$id <- id_vec
    # And write_satscan_files writes these IDs to .geo and .cas
    # So LOC_ID in SatScan output SHOULD be the user's ID (as char).

    gis_df <- ss_results$gis |>
        dplyr::mutate(LOC_ID = as.character(LOC_ID))

    location_summary <- loc_geo |>
        dplyr::left_join(gis_df, by = c("id" = "LOC_ID"))

    # Restore sf if input was sf
    if (is_sf_input) {
        # We need to re-attach geometry from the unique set of original data
        # This is expensive if data is huge.
        # Alternative: reconstruct st_point from lat/long in loc_geo
        # This is safer/faster for point data.
        # If input was polygons, extract_geometry used centroids.
        # So returning centroids as points is the honest representation of what SatScan used.
        location_summary <- sf::st_as_sf(location_summary, coords = c("long", "lat"), crs = 4326)
    }

    # 4. Create Main Results (Optional Time Series Join)
    main_results <- NULL
    if (merge_time_series) {
        if (verbose) message("Merging results back to full time-series data...")
        # Map ID -> Cluster
        id_cluster_map <- gis_df |>
            dplyr::select(id = LOC_ID, CLUSTER, P_VALUE)

        # We drop geometry from data if it's sf, to keep main_results lightweight
        data_no_geo <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data

        # Perform the join
        # Note: This assigns the Cluster ID to ALL time points for that location
        # If it's a Space-Time cluster, it only exists for a specific time range.
        # But standard GIS output in rsatscan gives the cluster for the location.
        # To correctly filter time, we'd need to checking [START_DATE, END_DATE] from cluster_summary.
        # For now, we just join the Cluster ID.
        main_results <- data_no_geo |>
            dplyr::mutate(id_char = as.character(dplyr::pull(data_no_geo, !!id_quo))) |>
            dplyr::left_join(id_cluster_map, by = c("id_char" = "id")) |>
            dplyr::select(-id_char)
    }

    # 5. Construct Object
    res <- list(
        main_results = main_results,
        location_summary = location_summary,
        cluster_summary = cluster_summary,
        raw_output = ss_results
    )
    class(res) <- "satscan_result"

    res
}

#' @export
print.satscan_result <- function(x, ...) {
    n_locs <- if (is.null(x$location_summary)) 0 else nrow(x$location_summary)
    n_clusters <- if (is.null(x$cluster_summary)) 0 else nrow(x$cluster_summary)
    sig_clusters <- if (n_clusters > 0) sum(x$cluster_summary$P_VALUE < 0.05, na.rm = TRUE) else 0

    cat("SatScan Analysis Result\n")
    cat("=======================\n")
    cat(sprintf("Locations: %d\n", n_locs))
    cat(sprintf("Clusters Found: %d (%d significant at p < 0.05)\n", n_clusters, sig_clusters))
    cat("\nTop Significant Clusters:\n")
    if (n_clusters > 0) {
        print(head(x$cluster_summary |>
            dplyr::filter(P_VALUE < 0.05) |>
            dplyr::select(dplyr::any_of(c("CLUSTER", "P_VALUE", "rel_risk", "REL_RISK", "START_DATE", "END_DATE"))), 5))
    } else {
        cat("None.\n")
    }
    cat("\nAccess components via $location_summary, $cluster_summary, $main_results\n")
}

#' @export
summary.satscan_result <- function(object, ...) {
    object$cluster_summary
}

#' @export
as.data.frame.satscan_result <- function(x, row.names = NULL, optional = FALSE, ...) {
    # If main_results (time series) is populated, user probably wants that.
    # If not, they probably want the location summary (map data).
    if (!is.null(x$main_results)) {
        return(x$main_results)
    } else {
        # If location_summary is SF, drop geometry to return a plain data.frame
        if (inherits(x$location_summary, "sf")) {
            return(sf::st_drop_geometry(x$location_summary))
        }
        return(x$location_summary)
    }
}
