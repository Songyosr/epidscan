#' Filter Population by Spatial Extent
#'
#' Filters population data to specific province codes.
#'
#' @param data Population data frame with areacode column
#' @param province_codes Character vector of province codes (e.g., c("90", "91"))
#' @param location_col Column containing location codes (default: "areacode")
#'
#' @return Filtered data frame
#'
#' @examples
#' filter_spatial_extent(pop_data, c("90")) # Songkhla only
#' filter_spatial_extent(pop_data, c("90", "91")) # Songkhla + Satun
#'
#' @export
filter_spatial_extent <- function(data,
                                  province_codes,
                                  location_col = "areacode") {
    if (!location_col %in% names(data)) {
        stop("Column '", location_col, "' not found in data")
    }

    # Extract province from location code (first 2 digits)
    data$province_extracted <- substr(as.character(data[[location_col]]), 1, 2)

    # Filter
    initial_rows <- nrow(data)
    data_filtered <- data[data$province_extracted %in% province_codes, ]

    # Remove temp column
    data_filtered$province_extracted <- NULL

    message("Filtered to province(s): ", paste(province_codes, collapse = ", "))
    message("  Rows: ", initial_rows, " -> ", nrow(data_filtered))

    if (nrow(data_filtered) == 0) {
        warning("No data found for specified provinces")
    }

    return(data_filtered)
}
