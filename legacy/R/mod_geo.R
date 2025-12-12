#' Process Geographic Data Module
#'
#' This script processes administrative boundary shapefiles to create:
#' 1. A lookup RDS file with sf objects for mapping
#' 2. A SatScan-compatible .geo file with centroids
#'
#' @param shp_path Path to the shapefile (.shp)
#' @param admin_level Administrative level code (default: "ADM3")
#' @param province_filter Optional province code to filter by (e.g., "90")
#' @param output_dir Directory to save output files
#'
#' @return List containing paths to the generated files:
#' \itemize{
#'   \item geo_lookup: Path to the RDS lookup file
#'   \item satscan_geo: Path to the SatScan .geo file
#' }
#'
#' @importFrom sf st_read st_centroid st_coordinates
#' @importFrom dplyr filter mutate select matches sym %>%
#' @importFrom stringr str_remove
#' @importFrom utils write.table
#' @export

process_geo <- function(shp_path,
                        admin_level = "ADM3",
                        province_filter = NULL,
                        output_dir = "data/derived") {
    message("=== Processing Geographic Data ===")

    # Ensure output directory exists
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # 1. Load Shapefile
    if (!file.exists(shp_path)) stop("Shapefile not found: ", shp_path)
    shp <- st_read(shp_path, quiet = TRUE)

    # 2. Filter by province (optional)
    if (!is.null(province_filter)) {
        # Assume province_filter is the ADM1 code (e.g., "90" for Songkhla)
        # Filter based on ADM codes that start with the province code
        pcode_col <- paste0(admin_level, "_PCODE")

        if (!pcode_col %in% names(shp)) {
            stop("Column ", pcode_col, " not found in shapefile")
        }

        # Construct filter pattern (e.g., "^TH90" for Songkhla at ADM3 level)
        filter_pattern <- paste0("^TH", province_filter)
        shp <- shp %>% filter(grepl(filter_pattern, !!sym(pcode_col)))

        message("Filtered to province ", province_filter, ": ", nrow(shp), " locations")
    }

    # 3. Create lookup table
    pcode_col <- paste0(admin_level, "_PCODE")
    name_th_col <- paste0(admin_level, "_TH")
    name_en_col <- paste0(admin_level, "_EN")

    shp_processed <- shp %>%
        mutate(
            location_id = str_remove(!!sym(pcode_col), "^TH"),
            location_name = !!sym(name_en_col)
        ) %>%
        select(
            location_id,
            location_name,
            matches("^ADM\\d+_(PCODE|TH|EN)$"),
            geometry
        )

    message("Processed ", nrow(shp_processed), " locations")

    # 4. Save SF Object (for mapping/lookup)
    saveRDS(shp_processed, file.path(output_dir, "geo_lookup.rds"))
    message("Saved geo lookup to: ", file.path(output_dir, "geo_lookup.rds"))

    # 5. Extract Centroids for SatScan
    centroids <- st_centroid(shp_processed)
    coords <- st_coordinates(centroids)

    geo_data <- data.frame(
        location_id = shp_processed$location_id,
        lat = coords[, 2],
        long = coords[, 1]
    )

    # 6. Save .geo File
    # Format: <LocationID> <Lat> <Long>
    write.table(geo_data,
        file.path(output_dir, "satscan.geo"),
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    message("Saved SatScan geo file to: ", file.path(output_dir, "satscan.geo"))

    return(list(
        geo_lookup = file.path(output_dir, "geo_lookup.rds"),
        satscan_geo = file.path(output_dir, "satscan.geo")
    ))
}

# Execute if run as script
if (sys.nframe() == 0) {
    # Example: process_geo("data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp", province_filter = "90")
    process_geo(
        shp_path = "data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp",
        admin_level = "ADM3",
        province_filter = "90" # Songkhla
    )
}
