#' Create Age Groups from Continuous Age
#'
#' Bins continuous age values into discrete age groups using specified breaks.
#'
#' @param data Data frame containing age column
#' @param age_col Character. Name of age column (default: "age")
#' @param breaks Numeric vector. Age breaks (default: c(0, 15, 60, Inf))
#' @param labels Character vector. Age group labels (optional, auto-generated if NULL)
#' @param output_col Character. Name of output column (default: "age_group")
#'
#' @return Data frame with new age_group column
#'
#' @examples
#' data <- data.frame(age = c(5, 25, 70, 15))
#' create_age_groups(data)
#' # Returns age_group: c("0-14", "15-59", "60+", "0-14")
#'
#' @export
create_age_groups <- function(data,
                              age_col = "age",
                              breaks = c(0, 15, 60, Inf),
                              labels = NULL,
                              output_col = "age_group") {
    # Check if age column exists
    if (!age_col %in% names(data)) {
        stop("Column '", age_col, "' not found in data")
    }

    # Auto-generate labels if not provided
    if (is.null(labels)) {
        labels <- paste0(
            breaks[-length(breaks)], "-",
            c(breaks[-c(1, length(breaks))] - 1, "+")
        )
    }

    # Create age groups
    data[[output_col]] <- cut(
        data[[age_col]],
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        right = FALSE
    )

    # Report distribution
    group_counts <- table(data[[output_col]], useNA = "ifany")
    message("Created age groups:")
    for (grp in names(group_counts)) {
        message("  ", grp, ": ", group_counts[grp])
    }

    return(data)
}
