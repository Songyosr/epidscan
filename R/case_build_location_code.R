#' Build Location Code from Administrative Components
#'
#' Constructs a standardized location identifier by concatenating administrative
#' unit codes (e.g., province, district, subdistrict).
#'
#' @param data Data frame containing administrative code columns
#' @param prefix_col Character. Name of column containing prefix/province code
#' @param middle_col Character. Name of column containing middle/district code
#' @param suffix_col Character. Name of column containing suffix/subdistrict code
#' @param output_col Character. Name of output column (default: "location_id")
#' @param pad_width Integer vector. Number of digits for each component (default: c(2,2,2))
#'
#' @return Data frame with new location_id column
#'
#' @examples
#' data <- data.frame(chw_code = 90, amp_code = 1, tmb_code = 5)
#' build_location_code(data, "chw_code", "amp_code", "tmb_code")
#' # Returns data with location_id = "900105"
#'
#' @export
build_location_code <- function(data,
                                prefix_col,
                                middle_col,
                                suffix_col,
                                output_col = "location_id",
                                pad_width = c(2, 2, 2)) {
    # Validate inputs
    if (!is.data.frame(data)) stop("data must be a data frame")

    required_cols <- c(prefix_col, middle_col, suffix_col)
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }

    # Build location code
    data[[output_col]] <- paste0(
        sprintf(paste0("%0", pad_width[1], "d"), as.numeric(data[[prefix_col]])),
        sprintf(paste0("%0", pad_width[2], "d"), as.numeric(data[[middle_col]])),
        sprintf(paste0("%0", pad_width[3], "d"), as.numeric(data[[suffix_col]]))
    )

    message("Built ", output_col, " from ", prefix_col, "+", middle_col, "+", suffix_col)
    message("  Example: ", data[[output_col]][1])

    return(data)
}
