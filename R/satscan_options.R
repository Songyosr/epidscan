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
build_satscan_options <- function(files, export_df, time_precision, type, model) {
    opts <- list(
        CaseFile = basename(files$cas_file),
        CoordinatesFile = basename(files$geo_file),
        CoordinatesType = 1, # Lat/Long (CRITICAL!)
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

    # Add date range from data
    if ("date" %in% names(export_df)) {
        dates <- export_df$date

        fmt <- switch(as.character(time_precision),
            "1" = "%Y",
            "2" = "%Y/%m",
            "3" = "%Y/%m/%d",
            "%Y/%m/%d" # Default for Generic if not handled or fallthrough
        )

        # Generic (0) often doesn't need start/end date in same format, but usually SatScan infers from data file
        # If generic, we might skip StartDate/EndDate or trust as.character?
        # rsatscan usually expects Y/M/D for parameters if they are dates.
        # BUT if precision is Year, it MUST be Year.

        opts$StartDate <- format(min(dates), fmt)
        opts$EndDate <- format(max(dates), fmt)
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
