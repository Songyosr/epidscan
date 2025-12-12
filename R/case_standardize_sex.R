#' Standardize Sex Values
#'
#' Converts various sex/gender representations to standardized English values.
#' Handles Thai (ชาย/หญิง), English (Male/Female/M/F), and mixed formats.
#'
#' @param data Data frame containing sex column
#' @param sex_col Character. Name of column containing sex values (default: "sex")
#' @param male_values Character vector. Values to map to "Male"
#'   (default: c("ชาย", "male", "m", "Male", "M"))
#' @param female_values Character vector. Values to map to "Female"
#'   (default: c("หญิง", "female", "f", "Female", "F"))
#'
#' @return Data frame with standardized sex column
#'
#' @examples
#' data <- data.frame(sex = c("ชาย", "หญิง", "Male", "F"))
#' standardize_sex(data)
#' # Returns: sex = c("Male", "Female", "Male", "Female")
#'
#' @export
standardize_sex <- function(data,
                            sex_col = "sex",
                            male_values = c("ชาย", "male", "m", "Male", "M"),
                            female_values = c("หญิง", "female", "f", "Female", "F")) {
    # Check if sex column exists
    if (!sex_col %in% names(data)) {
        warning("Column '", sex_col, "' not found. Skipping sex standardization.")
        return(data)
    }

    # Count original values
    original_vals <- table(data[[sex_col]], useNA = "ifany")

    # Standardize using case-insensitive matching
    data[[sex_col]] <- sapply(data[[sex_col]], function(x) {
        if (is.na(x)) {
            return(NA_character_)
        }
        x_lower <- tolower(as.character(x))
        male_lower <- tolower(male_values)
        female_lower <- tolower(female_values)

        if (x_lower %in% male_lower) {
            return("Male")
        } else if (x_lower %in% female_lower) {
            return("Female")
        } else {
            return(as.character(x)) # Keep original if not matched
        }
    })

    # Report changes
    new_vals <- table(data[[sex_col]], useNA = "ifany")
    message("Standardized sex values:")
    message("  Male: ", new_vals["Male"])
    message("  Female: ", new_vals["Female"])
    if (any(!names(new_vals) %in% c("Male", "Female"))) {
        message("  Unmapped: ", paste(names(new_vals)[!names(new_vals) %in% c("Male", "Female")],
            collapse = ", "
        ))
    }

    return(data)
}
