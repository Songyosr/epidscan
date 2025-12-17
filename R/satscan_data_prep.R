# Internal helpers for SatScan data preparation
# These functions are NOT exported - used by epid_satscan()

# Suppress R CMD check notes for NSE columns
utils::globalVariables(c("id", "lat", "long", "date"))

#' Extract Geometry from Data
#'
#' Uses sf::st_geometry() for sf objects, lat/long columns for data.frames.
#'
#' @param data Input data (sf or data.frame)
#' @param lat_quo Quosure for latitude column (if data.frame)
#' @param long_quo Quosure for longitude column (if data.frame)
#' @return data.frame with lat, long columns
#' @keywords internal
extract_geometry <- function(data, lat_quo = NULL, long_quo = NULL, geo_type = "latlong") {
    is_sf <- inherits(data, "sf")

    if (is_sf) {
        # If latlong, enforce WGS84
        if (geo_type == "latlong") {
            if (sf::st_crs(data) != sf::st_crs(4326)) {
                data <- sf::st_transform(data, 4326)
            }
        }
        # If cartesian, use raw coordinates (no transform)

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
#' @param time_precision Integer (0=Generic, 1=Year, 2=Month, 3=Day)
#' @return List with file paths (cas_file, geo_file, pop_file)
#' @keywords internal
write_satscan_files <- function(geo_df, export_df, work_dir, project_name = "epid", time_precision = 3L, covariates = NULL) {
    cas_file <- file.path(work_dir, paste0(project_name, ".cas"))
    geo_file <- file.path(work_dir, paste0(project_name, ".geo"))
    pop_file <- file.path(work_dir, paste0(project_name, ".pop"))

    # Helper for date formatting
    format_date_by_precision <- function(d, prec) {
        # 1. If Date/POSIXt, standard formatting
        if (inherits(d, "Date") || inherits(d, "POSIXt")) {
            if (prec == 1L) {
                return(format(d, "%Y"))
            } # Year
            if (prec == 2L) {
                return(format(d, "%Y/%m"))
            } # Month
            if (prec == 3L) {
                return(format(d, "%Y/%m/%d"))
            } # Day
            return(as.character(d)) # Fallback
        }

        # 2. If Generic (0), just stringify
        if (prec == 0L) {
            return(as.character(d))
        }

        # 3. Handling Non-Date inputs for specific precision
        # If Year precision and numeric -> Assume valid 4-digit year
        if (prec == 1L) {
            if (is.numeric(d)) {
                return(as.character(d))
            }
            if (is.character(d)) {
                return(d)
            }
        }

        # 4. Otherwise, error because we cannot safely infer YYYY or YYYY/MM from raw numbers/strings
        # (e.g. does "202401" mean Jan 2024? Or is it just a number?)
        stop(sprintf(
            "Time precision %s requires a Date/POSIXt column, or (for Year) a numeric/character column. Found: %s",
            switch(as.character(prec),
                "1" = "Year",
                "2" = "Month",
                "3" = "Day",
                "Unknown"
            ),
            class(d)[1]
        ))
    }

    # Write Geo (id, lat, long) - deduplicated
    geo_unique <- geo_df |>
        dplyr::distinct(id, .keep_all = TRUE) |>
        dplyr::select(id, lat, long)
    utils::write.table(geo_unique, geo_file,
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    # Write Case (id, cases, [date], [covariates])
    # Case File Structure: LocationID, NoCases, [Date], [Covariate1, Covariate2...]
    cas_df <- data.frame(id = export_df$id, cases = export_df$cases)

    # 1. Add Date if present
    if ("date" %in% names(export_df)) {
        cas_df$date <- format_date_by_precision(export_df$date, time_precision)
    }

    # 2. Add Covariates if present
    if (!is.null(covariates)) {
        for (cov in covariates) {
            if (cov %in% names(export_df)) {
                cas_df[[cov]] <- export_df[[cov]]
            } else {
                warning(sprintf("Covariate '%s' not found in data - skipping.", cov))
            }
        }
    }

    utils::write.table(cas_df, cas_file,
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    # Write Pop (id, [date], pop, [covariates])
    # Population File Structure: LocationID, [Date/Year], Population, [Covariate1, Covariate2...]
    # Note: If covariates are used, population data usually needs to be stratified by them too (for Poisson).
    pop_written <- FALSE
    if ("pop" %in% names(export_df)) {
        pop_df <- data.frame(id = export_df$id)

        # 1. Add Date if present
        if ("date" %in% names(export_df)) {
            pop_df$date <- format_date_by_precision(export_df$date, time_precision)
        }

        # 2. Add Population
        pop_df$pop <- export_df$pop

        # 3. Add Covariates
        if (!is.null(covariates)) {
            for (cov in covariates) {
                if (cov %in% names(export_df)) {
                    pop_df[[cov]] <- export_df[[cov]]
                }
                # No warning here as we already warned above if missing
            }
        }

        # Deduplicate
        # We must deduplicate based on ALL stratification variables (ID, Date, Covariates)
        distinct_cols <- c("id")
        if ("date" %in% names(pop_df)) distinct_cols <- c(distinct_cols, "date")
        if (!is.null(covariates)) {
            # Only include covariates that actually made it into the DF
            valid_covs <- intersect(covariates, names(pop_df))
            distinct_cols <- c(distinct_cols, valid_covs)
        }

        pop_df <- pop_df |> dplyr::distinct(dplyr::pick(dplyr::all_of(distinct_cols)), .keep_all = TRUE)

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
