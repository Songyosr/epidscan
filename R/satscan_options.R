# Internal helpers for SatScan options configuration
# These functions are NOT exported - used by epid_satscan()

#' Detect Time Precision
#'
#' Auto-detect or use user-specified time precision.
#'
#' @param date_values Values from date column (or NULL)
#' @param user_precision User-specified precision (character or numeric, or NULL)
#' @return Integer: 0=Generic, 1=Year, 2=Month, 3=Day
#' @keywords internal
detect_time_precision <- function(date_values, user_precision = NULL) {
    # User explicitly specified
    if (!is.null(user_precision)) {
        if (is.character(user_precision)) {
            return(switch(tolower(user_precision),
                "year" = 1L,
                "month" = 2L,
                "day" = 3L,
                "generic" = 0L,
                0L
            ))
        }
        return(as.integer(user_precision))
    }

    # No date column
    if (is.null(date_values)) {
        return(0L) # Generic
    }

    # Auto-detect from first value
    sample_val <- date_values[1]

    if (inherits(sample_val, "Date") || inherits(sample_val, "POSIXt")) {
        return(3L) # Day precision for Date objects
    }

    # Numeric - default to generic
    0L
}

#' Build SatScan Options List
#'
#' Creates the options list for rsatscan::ss.options()
#'
#' @param files List with cas_file, geo_file, pop_file paths
#' @param export_df Export dataframe for date range
#' @param time_precision Integer time precision
#' @param type Analysis type string
#' @param model Model type string
#' @return Named list of SatScan options
#' @keywords internal
build_satscan_options <- function(files, export_df, time_precision, type, model, geo_type = "latlong", start_date = NULL, end_date = NULL) {
    # 0=Cartesian, 1=Lat/Long
    coords_type <- if (geo_type == "cartesian") 0 else 1

    opts <- list(
        CaseFile = basename(files$cas_file),
        CoordinatesFile = basename(files$geo_file),
        CoordinatesType = coords_type,
        PrecisionCaseTimes = time_precision,
        TimeAggregationUnits = time_precision,
        AnalysisType = switch(type,
            "space-time" = 3,
            "purely-spatial" = 1,
            "space-time-permutation" = 4,
            3 # default
        ),
        ModelType = switch(model,
            "poisson" = 0,
            "bernoulli" = 1,
            "space-time-permutation" = 2,
            0 # default
        ),
        ResultsFile = "epid.txt"
    )

    # Add population file if exists
    if (!is.null(files$pop_file)) {
        opts$PopulationFile <- basename(files$pop_file)
    }

    # Helper to format any date/string using precision logic
    fmt_date <- function(d, prec) {
        # If it's already character, assume user knows best?
        # But we could try to reformat if it looks like a Date object
        if (inherits(d, "Date") || inherits(d, "POSIXt")) {
            fmt <- switch(as.character(prec),
                "1" = "%Y",
                "2" = "%Y/%m",
                "3" = "%Y/%m/%d",
                "%Y/%m/%d"
            )
            return(format(d, fmt))
        }
        # If character, just return as is (assuming valid)
        return(as.character(d))
    }

    # Add date range from data OR explicit arguments
    # If explicit dates provided, they take precedence

    # Defaults from data
    min_d <- NULL
    max_d <- NULL
    if ("date" %in% names(export_df)) {
        min_d <- min(export_df$date, na.rm = TRUE)
        max_d <- max(export_df$date, na.rm = TRUE)
    }

    # Resolve StartDate
    if (!is.null(start_date)) {
        opts$StartDate <- fmt_date(start_date, time_precision)
    } else if (!is.null(min_d)) {
        opts$StartDate <- fmt_date(min_d, time_precision)
    }

    # Resolve EndDate
    if (!is.null(end_date)) {
        opts$EndDate <- fmt_date(end_date, time_precision)
    } else if (!is.null(max_d)) {
        opts$EndDate <- fmt_date(max_d, time_precision)
    }

    opts
}

#' Apply User Overrides
#'
#' Merge user options with current options (user takes precedence)
#'
#' @param opts Current options list
#' @param user_opts Named list of user overrides
#' @return Merged options list
#' @keywords internal
apply_user_overrides <- function(opts, user_opts) {
    if (length(user_opts) == 0) {
        return(opts)
    }
    utils::modifyList(opts, user_opts)
}
