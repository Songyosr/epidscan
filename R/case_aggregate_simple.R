#' Aggregate Cases for Simple (Univariate) Mode
#'
#' Aggregates case data by location and date for simple SatScan analysis.
#'
#' @param data Data frame with location_id and date columns
#' @param location_col Character. Location ID column name (default: "location_id")
#' @param date_col Character. Date column name (default: "date")
#'
#' @return Data frame with columns: location_id, date, cases
#'
#' @examples
#' data <- data.frame(
#'     location_id = c("900101", "900101", "900102"),
#'     date = as.Date(c("2025-01-01", "2025-01-01", "2025-01-02"))
#' )
#' aggregate_simple(data)
#' # Returns 2 rows (2 cases at 900101 on 2025-01-01, 1 case at 900102)
#'
#' @export
aggregate_simple <- function(data,
                             location_col = "location_id",
                             date_col = "date") {
    # Validate columns
    if (!location_col %in% names(data)) stop("Column '", location_col, "' not found")
    if (!date_col %in% names(data)) stop("Column '", date_col, "' not found")

    # Aggregate
    result <- data %>%
        group_by(!!sym(location_col), !!sym(date_col)) %>%
        summarise(cases = n(), .groups = "drop") %>%
        rename(location_id = !!sym(location_col), date = !!sym(date_col))

    message("Aggregated to ", nrow(result), " location-date combinations")
    message("  Total cases: ", sum(result$cases))
    message("  Date range: ", min(result$date), " to ", max(result$date))

    return(result)
}
