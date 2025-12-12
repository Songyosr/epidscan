#' Detect Spatial Extent from Case Data
#'
#' Automatically detects province codes from case data location IDs.
#'
#' @param case_data Case data frame or path to RDS file
#' @param location_col Name of location ID column (default: "location_id")
#'
#' @return Character vector of unique province codes
#'
#' @details
#' Extracts first 2 digits from location IDs to determine provinces.
#' Example: "900105" -> "90" (Songkhla province)
#'
#' @examples
#' # From data frame
#' detect_spatial_extent(cases_df)
#'
#' # From file
#' detect_spatial_extent("data/cases_prepared.rds")
#'
#' @export
detect_spatial_extent <- function(case_data, location_col = "location_id") {
    # Load if path provided
    if (is.character(case_data) && length(case_data) == 1) {
        if (file.exists(case_data)) {
            case_data <- readRDS(case_data)
        } else {
            stop("Case data file not found: ", case_data)
        }
    }

    # Check column exists
    if (!location_col %in% names(case_data)) {
        stop("Column '", location_col, "' not found in case data")
    }

    # Extract province codes (first 2 digits)
    location_ids <- as.character(case_data[[location_col]])
    province_codes <- unique(substr(location_ids, 1, 2))
    province_codes <- province_codes[!is.na(province_codes) & province_codes != ""]

    # Sort for consistency
    province_codes <- sort(province_codes)

    message(
        "Detected ", length(province_codes), " province(s): ",
        paste(province_codes, collapse = ", ")
    )

    return(province_codes)
}
