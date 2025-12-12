#' Aggregate Population by Location
#'
#' Sums population across all age/sex groups to get total per location.
#' Used for simple (univariate) mode.
#'
#' @param data Population data with age/sex columns from MOPH API
#' @param location_col Column containing location codes (default: "areacode")
#'
#' @return Data frame with columns: location_id, year, population
#'
#' @details
#' The MOPH API returns data with columns like:
#' - male_0, male_1-4, male_5-9, ..., male_85_plus
#' - female_0, female_1-4, female_5-9, ..., female_85_plus
#'
#' This function sums all those columns to get total population per location.
#'
#' @examples
#' aggregate_by_location(moph_data)
#'
#' @export
aggregate_by_location <- function(data, location_col = "areacode") {
    if (!location_col %in% names(data)) {
        stop("Column '", location_col, "' not found")
    }

    # Extract tambon code (first 6 digits of areacode)
    data$location_id <- substr(as.character(data[[location_col]]), 1, 6)

    # Find all male/female columns
    age_sex_cols <- names(data)[grepl("^(male_|female_)", names(data))]

    if (length(age_sex_cols) == 0) {
        stop("No age/sex columns found (expected male_*, female_*)")
    }

    # Sum across all age/sex groups per row
    data$total_pop <- rowSums(data[, age_sex_cols], na.rm = TRUE)

    # Aggregate by location and year
    result <- data %>%
        group_by(location_id, year) %>%
        summarise(population = sum(total_pop, na.rm = TRUE), .groups = "drop")

    message("Aggregated to ", nrow(result), " location-year combinations")
    message("  Total population: ", format(sum(result$population), big.mark = ","))

    return(result)
}
