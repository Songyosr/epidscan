#' Validate Population Data
#'
#' Validates population data structure and values before writing.
#'
#' @param data Population data to validate
#' @param mode "simple" or "stratified"
#'
#' @return Validated data (throws error if invalid)
#'
#' @examples
#' validate_population_data(pop_data, mode = "simple")
#'
#' @export
validate_population_data <- function(data, mode = "simple") {
    # Check required columns
    if (mode == "simple") {
        required_cols <- c("location_id", "year", "population")
    } else {
        required_cols <- c("location_id", "year", "population", "age_group", "sex")
    }

    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Check for negative populations
    if (any(data$population < 0, na.rm = TRUE)) {
        stop("Negative population values detected")
    }

    # Check for NA in critical columns
    for (col in required_cols) {
        n_na <- sum(is.na(data[[col]]))
        if (n_na > 0) {
            stop("Column '", col, "' has ", n_na, " NA values")
        }
    }

    # Check year format
    if (!all(data$year >= 1900 & data$year <= 2100)) {
        stop("Invalid year values (must be between 1900-2100)")
    }

    # Check population is numeric
    if (!is.numeric(data$population)) {
        stop("Population column must be numeric")
    }

    # Stratified mode specific checks
    if (mode == "stratified") {
        # Check sex values
        valid_sex <- c("Male", "Female")
        invalid_sex <- setdiff(unique(data$sex), valid_sex)
        if (length(invalid_sex) > 0) {
            stop(
                "Invalid sex values: ", paste(invalid_sex, collapse = ", "),
                "\nMust be 'Male' or 'Female'"
            )
        }

        # Check age_group is factor or character
        if (!is.factor(data$age_group) && !is.character(data$age_group)) {
            stop("age_group must be factor or character")
        }
    }

    message("✓ Population data validated")
    message("  Mode: ", mode)
    message("  Rows: ", nrow(data))
    message("  Locations: ", length(unique(data$location_id)))
    message("  Years: ", paste(unique(data$year), collapse = ", "))

    return(data)
}
