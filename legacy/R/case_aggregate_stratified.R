#' Aggregate Cases for Stratified Mode
#'
#' Aggregates case data by location, date, and strata (age group, sex) for
#' stratified SatScan analysis.
#'
#' @param data Data frame with location_id, date, age_group, sex columns
#' @param location_col Character. Location ID column (default: "location_id")
#' @param date_col Character. Date column (default: "date")
#' @param strata_cols Character vector. Strata columns (default: c("age_group", "sex"))
#'
#' @return Data frame with columns: location_id, date, age_group, sex, cases
#'
#' @examples
#' data <- data.frame(
#'     location_id = rep("900101", 4),
#'     date = as.Date("2025-01-01"),
#'     age_group = c("0-17", "0-17", "18-59", "18-59"),
#'     sex = c("Male", "Male", "Female", "Male")
#' )
#' aggregate_stratified(data)
#' # Returns 3 rows (2 Male 0-17, 1 Female 18-59, 1 Male 18-59)
#'
#' @export
aggregate_stratified <- function(data,
                                 location_col = "location_id",
                                 date_col = "date",
                                 strata_cols = c("age_group", "sex")) {
    # Validate columns
    required_cols <- c(location_col, date_col, strata_cols)
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }

    # Aggregate
    result <- data %>%
        group_by(across(all_of(c(location_col, date_col, strata_cols)))) %>%
        summarise(cases = n(), .groups = "drop") %>%
        rename(
            location_id = !!sym(location_col),
            date = !!sym(date_col)
        )

    message("Aggregated to ", nrow(result), " location-date-strata combinations")
    message("  Total cases: ", sum(result$cases))
    message("  Strata: ", paste(strata_cols, collapse = " × "))

    return(result)
}
