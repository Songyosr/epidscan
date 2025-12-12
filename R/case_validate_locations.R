#' Validate Location IDs Against Geographic Reference
#'
#' Filters data to only include location IDs present in a geographic reference file.
#' Removes rows with invalid or missing location IDs and reports statistics.
#'
#' @param data Data frame with location_id column
#' @param geo_lookup_path Character. Path to geo_lookup.rds file
#' @param location_col Character. Name of location ID column (default: "location_id")
#' @param geo_id_col Character. Name of ID column in geo file (default: "ADM3_PCODE")
#' @param strip_prefix Character. Prefix to remove from geo IDs (default: "TH")
#'
#' @return Data frame with only valid location IDs
#'
#' @examples
#' data <- data.frame(location_id = c("900101", "900102", "999999"))
#' validate_locations(data, "data/derived/geo_lookup.rds")
#' # Removes row with 999999 if not in geo file
#'
#' @export
validate_locations <- function(data,
                               geo_lookup_path,
                               location_col = "location_id",
                               geo_id_col = "ADM3_PCODE",
                               strip_prefix = "TH") {
    # Check if geo file exists
    if (!file.exists(geo_lookup_path)) {
        warning("Geo lookup file not found: ", geo_lookup_path)
        warning("Skipping location validation")
        return(data)
    }

    # Check if location column exists
    if (!location_col %in% names(data)) {
        stop("Column '", location_col, "' not found in data")
    }

    # Load geo reference
    geo_lookup <- readRDS(geo_lookup_path)

    # Extract valid IDs and strip prefix if specified
    if (!is.null(strip_prefix) && strip_prefix != "") {
        valid_ids <- unique(sub(paste0("^", strip_prefix), "", geo_lookup[[geo_id_col]]))
    } else {
        valid_ids <- unique(geo_lookup[[geo_id_col]])
    }

    # Identify invalid locations
    invalid_mask <- !data[[location_col]] %in% valid_ids

    if (sum(invalid_mask) > 0) {
        invalid_ids <- unique(data[[location_col]][invalid_mask])
        n_invalid_rows <- sum(invalid_mask)

        message("Removing ", n_invalid_rows, " rows with invalid location IDs")
        message(
            "  Invalid IDs (showing up to 10): ",
            paste(head(invalid_ids, 10), collapse = ", ")
        )
        if (length(invalid_ids) > 10) {
            message("  ... and ", length(invalid_ids) - 10, " more")
        }

        # Filter to valid locations
        data <- data[!invalid_mask, ]
        message("Remaining rows: ", nrow(data))
    } else {
        message("All location IDs are valid")
    }

    return(data)
}
