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
build_satscan_options <- function(files, export_df, time_precision, type, model,
                                  geo_type = "latlong", start_date = NULL, end_date = NULL,
                                  monitor_mode = "retrospective", prospective_start_date = NULL) {
    .Deprecated("satscanr", msg = "build_satscan_options is deprecated. Use ss.options directly or via set_satscan_opts().")
    # helper for checking if a value is effectively an integer
    as_int_or_val <- function(x) {
        if (is.null(x) || length(x) == 0) {
            return(x)
        }
        if (is.na(x)) {
            return(x)
        }
        if (is.numeric(x) && x %% 1 == 0) {
            return(as.integer(x))
        }
        x_str <- as.character(x)
        if (grepl("^[0-9]+$", x_str)) {
            return(as.integer(x_str))
        }
        x
    }

    # 1. Map AnalysisType
    # Clean string input
    type_key <- if (is.character(type)) tolower(type) else type

    analysis_type <- switch(as.character(type_key),
        "purely-spatial" = 1L,
        "purely-temporal" = 2L,
        "space-time" = 3L,
        "space-time-permutation" = 4L,
        "spatial-variation-in-temporal-trends" = 5L,
        "space-time-magnitude" = 6L,
        # "bernoulli" is NOT a valid AnalysisType. It is a ModelType.
        as_int_or_val(type) # Fallback to user input
    )

    # Validation for AnalysisType
    if (is.numeric(analysis_type) && !is.na(analysis_type)) {
        if (analysis_type == 7L) {
            stop("AnalysisType 7 (Bernoulli) is not a valid SaTScan analysis type. Did you mean to set model='bernoulli'?")
        }
        if (!analysis_type %in% 1:6) {
            warning(sprintf("Unknown AnalysisType: %s. Valid types are 1-6.", analysis_type))
        }
    } else if (!is.null(analysis_type) && !is.na(analysis_type)) {
        # Character validation
        if (tolower(as.character(analysis_type)) == "bernoulli") {
            stop("AnalysisType 'bernoulli' is not a valid SaTScan analysis type. Did you mean to set model='bernoulli'?")
        }
    }

    # 2. Map ModelType
    model_key <- if (is.character(model)) tolower(model) else model

    model_type <- switch(as.character(model_key),
        "poisson" = 0L,
        "bernoulli" = 1L,
        "space-time-permutation" = 2L,
        "ordinal" = 3L,
        "exponential" = 4L,
        "normal" = 5L,
        "continuous-poisson" = 6L,
        "multinomial" = 7L,
        "rank" = 8L,
        as_int_or_val(model) # Fallback
    )

    # 3. Geo Type
    coords_type <- if (geo_type == "cartesian") 0L else 1L

    opts <- list(
        CaseFile = basename(files$cas_file),
        CoordinatesFile = basename(files$geo_file),
        CoordinatesType = coords_type,
        PrecisionCaseTimes = time_precision,
        TimeAggregationUnits = time_precision,
        AnalysisType = analysis_type,
        ModelType = model_type,
        ResultsFile = "epid.txt"
    )

    # Add population file if exists
    if (!is.null(files$pop_file)) {
        opts$PopulationFile <- basename(files$pop_file)
    }

    # Helper to format any date/string using precision logic
    fmt_date <- function(d, prec, is_end = FALSE) {
        # 1. If Date/POSIXt, standard formatting
        if (inherits(d, "Date") || inherits(d, "POSIXt")) {
            d_date <- as.Date(d)
            if (is_end) {
                # If Year precision (1), ensure 12/31
                if (prec == 1L) {
                    # Create date for Dec 31 of that year
                    d_date <- as.Date(paste0(format(d_date, "%Y"), "-12-31"))
                } else if (prec == 2L) {
                    # If Month precision (2), ensure last day of month
                    # ceiling_date logic without lubridate:
                    # Next month 1st minus 1 day
                    d_next <- seq(d_date, by = "month", length.out = 2)[2]
                    d_date <- d_next - 1
                }
            } else {
                # Start Date logic
                if (prec == 1L) {
                    # Year: Force 01/01
                    d_date <- as.Date(paste0(format(d_date, "%Y"), "-01-01"))
                } else if (prec == 2L) {
                    # Month: Force 01
                    d_date <- as.Date(paste0(format(d_date, "%Y-%m"), "-01"))
                }
            }
            return(format(d_date, "%Y/%m/%d"))
        }

        # 2. If Character/Numeric Year (e.g. "2024" or 2024)
        d_str <- as.character(d)
        if (grepl("^\\d{4}$", d_str)) {
            if (is_end) {
                return(paste0(d_str, "/12/31"))
            } else {
                return(paste0(d_str, "/01/01"))
            }
        }

        # 3. If "YYYY/MM" string (Month precision)
        if (grepl("^\\d{4}/\\d{2}$", d_str)) {
            if (is_end) {
                # Parse and find EOM
                d_date <- as.Date(paste0(d_str, "/01"))
                d_next <- seq(d_date, by = "month", length.out = 2)[2]
                return(format(d_next - 1, "%Y/%m/%d"))
            } else {
                return(paste0(d_str, "/01"))
            }
        }

        # Fallback
        return(d_str)
    }

    # Add date range from data OR explicit arguments
    min_d <- NULL
    max_d <- NULL
    if ("date" %in% names(export_df)) {
        min_d <- min(export_df$date, na.rm = TRUE)
        max_d <- max(export_df$date, na.rm = TRUE)
    }

    # Resolve StartDate
    if (!is.null(start_date)) {
        opts$StartDate <- fmt_date(start_date, time_precision, is_end = FALSE)
    } else if (!is.null(min_d)) {
        opts$StartDate <- fmt_date(min_d, time_precision, is_end = FALSE)
    }

    # Resolve EndDate
    if (!is.null(end_date)) {
        opts$EndDate <- fmt_date(end_date, time_precision, is_end = TRUE)
    } else if (!is.null(max_d)) {
        opts$EndDate <- fmt_date(max_d, time_precision, is_end = TRUE)
    }

    # Resolve Prospective Scanning
    if (monitor_mode == "prospective" && !is.null(prospective_start_date)) {
        opts$ProspectiveStartDate <- fmt_date(prospective_start_date, time_precision, is_end = FALSE)
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
    .Deprecated("set_satscan_opts")
    if (length(user_opts) == 0) {
        return(opts)
    }
    utils::modifyList(opts, user_opts)
}
