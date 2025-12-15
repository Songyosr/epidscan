# Internal helpers for SatScan data preparation
# These functions are NOT exported - used by epid_satscan()

#' Extract Geometry from Data
#'
#' Uses sf::st_geometry() for sf objects, lat/long columns for data.frames.
#'
#' @param data Input data (sf or data.frame)
#' @param lat_quo Quosure for latitude column (if data.frame)
#' @param long_quo Quosure for longitude column (if data.frame)
#' @return data.frame with lat, long columns
#' @keywords internal
extract_geometry <- function(data, lat_quo = NULL, long_quo = NULL) {
    is_sf <- inherits(data, "sf")

    if (is_sf) {
        # Use sf centroids
        centroids <- sf::st_centroid(sf::st_geometry(data))
        coords <- sf::st_coordinates(centroids)

        data.frame(
            lat = coords[, 2], # Y
            long = coords[, 1] # X
        )
    } else {
        # Extract from columns
        if (rlang::quo_is_null(lat_quo) || rlang::quo_is_null(long_quo)) {
            stop("For non-sf data, lat_col and long_col must be provided.")
        }

        data.frame(
            lat = rlang::eval_tidy(lat_quo, data),
            long = rlang::eval_tidy(long_quo, data)
        )
    }
}

#' Write SatScan Input Files
#'
#' Writes .cas, .geo, .pop files for SatScan.
#'
#' @param geo_df Geometry data.frame with id, lat, long
#' @param export_df Export data.frame with id, cases, date, pop (optional)
#' @param work_dir Working directory
#' @param project_name Project name for files (default: "epid")
#' @return List with file paths (cas_file, geo_file, pop_file)
#' @keywords internal
write_satscan_files <- function(geo_df, export_df, work_dir, project_name = "epid") {
    cas_file <- file.path(work_dir, paste0(project_name, ".cas"))
    geo_file <- file.path(work_dir, paste0(project_name, ".geo"))
    pop_file <- file.path(work_dir, paste0(project_name, ".pop"))

    # Write Geo (id, lat, long) - deduplicated
    geo_unique <- geo_df |>
        dplyr::distinct(id, .keep_all = TRUE) |>
        dplyr::select(id, lat, long)
    utils::write.table(geo_unique, geo_file,
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    # Write Case (id, cases, [date])
    cas_df <- data.frame(id = export_df$id, cases = export_df$cases)
    if ("date" %in% names(export_df)) {
        # Format date for SatScan
        cas_df$date <- format(export_df$date, "%Y/%m/%d")
    }
    utils::write.table(cas_df, cas_file,
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    # Write Pop (id, [date], pop) - includes date for space-time analysis
    pop_written <- FALSE
    if ("pop" %in% names(export_df)) {
        pop_df <- data.frame(id = export_df$id)
        if ("date" %in% names(export_df)) {
            pop_df$date <- format(export_df$date, "%Y/%m/%d")
        }
        pop_df$pop <- export_df$pop

        # Deduplicate by id (and date if present)
        if ("date" %in% names(pop_df)) {
            pop_df <- pop_df |> dplyr::distinct(id, date, .keep_all = TRUE)
        } else {
            pop_df <- pop_df |> dplyr::distinct(id, .keep_all = TRUE)
        }

        utils::write.table(pop_df, pop_file,
            row.names = FALSE, col.names = FALSE, quote = FALSE
        )
        pop_written <- TRUE
    }

    list(
        cas_file = cas_file,
        geo_file = geo_file,
        pop_file = if (pop_written) pop_file else NULL
    )
}
