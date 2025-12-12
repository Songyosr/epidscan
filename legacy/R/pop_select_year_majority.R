#' Select Year Using Majority Vote
#'
#' Selects year with most cases, or uses fallback year.
#'
#' @param data Multi-year population data
#' @param case_data Optional case data for majority vote
#' @param year_select_fn Optional user function for year selection
#' @param fallback_year Year to use if no cases (default: most recent in data)
#'
#' @return Single-year population data
#'
#' @details
#' Year selection strategy:
#' 1. If year_select_fn provided: use custom function
#' 2. If case_data provided: select year with most cases
#' 3. Else: use fallback_year (or most recent year in data)
#'
#' @examples
#' # Majority vote from cases
#' select_year_majority(pop_data, case_data = cases_df)
#'
#' # Use specific year
#' select_year_majority(pop_data, fallback_year = 2024)
#'
#' @export
select_year_majority <- function(data,
                                 case_data = NULL,
                                 year_select_fn = NULL,
                                 fallback_year = NULL) {
    available_years <- unique(data$year)

    # Strategy 1: Custom function
    if (!is.null(year_select_fn)) {
        selected_year <- year_select_fn(data, case_data)
        message("Selected year using custom function: ", selected_year)
    }
    # Strategy 2: Majority vote from cases
    else if (!is.null(case_data)) {
        # Extract year from date column
        if ("date" %in% names(case_data)) {
            case_years <- as.numeric(format(case_data$date, "%Y"))
        } else if ("onset_date" %in% names(case_data)) {
            case_years <- as.numeric(format(as.Date(case_data$onset_date), "%Y"))
        } else {
            warning("No date column found in case_data, using fallback")
            case_years <- NULL
        }

        if (!is.null(case_years)) {
            # Count cases per year
            year_counts <- table(case_years)
            selected_year <- as.numeric(names(year_counts)[which.max(year_counts)])

            message("Year selection (majority vote from cases):")
            for (yr in names(year_counts)) {
                message(
                    "  ", yr, ": ", year_counts[yr], " cases",
                    if (yr == selected_year) " <- selected" else ""
                )
            }
        } else {
            selected_year <- fallback_year %||% max(available_years)
        }
    }
    # Strategy 3: Fallback
    else {
        selected_year <- fallback_year %||% max(available_years)
        message("Using fallback year: ", selected_year)
    }

    # Validate selected year exists
    if (!selected_year %in% available_years) {
        warning("Selected year ", selected_year, " not in data. Using ", max(available_years))
        selected_year <- max(available_years)
    }

    # Filter to selected year
    data_filtered <- data[data$year == selected_year, ]

    message("Filtered to year ", selected_year, ": ", nrow(data_filtered), " rows")

    return(data_filtered)
}

# Helper for NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
