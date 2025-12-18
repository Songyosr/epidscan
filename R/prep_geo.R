#' Prepare SaTScan Geometry File
#'
#' `r lifecycle::badge("superseded")`
#'
#' Prepares a coordinate file for SaTScan.
#' Handles \code{sf} objects (extracting centroids) or plain data frames.
#'
#' @seealso [ss_geo()] for the new `ss_tbl` interface.
#'
#' @section SaTScan File Specification:
#' The Coordinates File has the following structure:
#' \code{<LocationID> <Latitude/Y> <Longitude/X>}
#'
#' \itemize{
#'   \item \strong{LocationID}: Unique identifier matching Case/Population files.
#'   \item \strong{Coordinate Order}:
#'     \itemize{
#'       \item \strong{Lat/Long}: SaTScan expects \code{Latitude} first, then \code{Longitude}.
#'       \item \strong{Cartesian}: SaTScan expects \code{X} first, then \code{Y}.
#'     }
#' }
#'
#' \strong{Note on Coordinate Ordering}:
#' This function automatically handles the swapping for you:
#' \itemize{
#'   \item If \code{sf} object (Lat/Long crs) -> Swaps to \code{Lat, Long}.
#'   \item If \code{data.frame} (Lat/Long detected) -> Swaps to \code{Lat, Long}.
#'   \item If \code{data.frame} (Cartesian detected) -> Keeps as \code{X, Y}.
#' }
#'
#' @param x Input object. Can be an \code{sf} object or a \code{data.frame}.
#' @param loc_id Column name for location ID (unquoted).
#' @param coords Character vector of length 2 specifying coordinate columns if x is a data.frame.
#' @param coord_type Coordinate system: "auto" (default), "latlong", or "cartesian".
#'   \itemize{
#'     \item "auto": Attempts to detect from data range or CRS.
#'     \item "latlong": Forces interpretation as Latitude/Longitude.
#'     \item "cartesian": Forces interpretation as Cartesian (X/Y).
#'   }
#' @param geometry Column name for geometry if x is sf and not active geometry (optional).
#' @return A \code{satscan_table} object of kind "geo".
#'
#' @examples
#' \dontrun{
#' # From sf object
#' library(sf)
#' my_shapes <- st_read("shapes.shp")
#' geo_obj <- prep_geo(my_shapes, loc_id = GEOID)
#'
#' # From data frame (Lat/Long)
#' df <- data.frame(id = "A", lon = 100, lat = 13)
#' geo_obj2 <- prep_geo(df, loc_id = id, coords = c("lon", "lat"), coord_type = "latlong")
#'
#' # From data frame (Cartesian)
#' df_cart <- data.frame(id = "A", x = 500000, y = 100000)
#' geo_obj3 <- prep_geo(df_cart, loc_id = id, coords = c("x", "y"), coord_type = "cartesian")
#' }
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @importFrom sf st_centroid st_coordinates st_geometry st_is_longlat st_crs
#' @export
prep_geo <- function(x, loc_id, coords = NULL, geometry = NULL,
                     coord_type = c("auto", "latlong", "cartesian")) {
    coord_type_arg <- match.arg(coord_type)

    id_quo <- rlang::enquo(loc_id)
    if (rlang::quo_is_null(id_quo)) stop("loc_id is required")

    ids <- as.character(rlang::eval_tidy(id_quo, x))

    final_type <- "latlong"

    # -------------------------------------------------------------------------
    # Case 1: SF Object
    # -------------------------------------------------------------------------
    if (inherits(x, "sf")) {
        # Check CRS
        is_ll <- sf::st_is_longlat(x)
        crs <- sf::st_crs(x)

        # Detect type if auto
        if (coord_type_arg == "auto") {
            if (isTRUE(is_ll)) {
                final_type <- "latlong"
                message("Detected Lat/Long coordinates from SF object (EPSG:", crs$epsg, ")")
            } else {
                final_type <- "cartesian"
                # If NA (no CRS), assume cartesian or warn? Default to cartesian for projected.
                if (is.na(is_ll)) {
                    warning("SF object has no CRS. Assuming Cartesian/Projected coordinates.")
                } else {
                    message("Detected Projected/Cartesian coordinates from SF object (EPSG:", crs$epsg, ")")
                }
            }
        } else {
            # User forced type
            final_type <- coord_type_arg
            # Warn on mismatch?
            if (final_type == "latlong" && isFALSE(is_ll)) {
                warning("User forced 'latlong' but SF object is projected. Ensure this is intended.")
            }
            if (final_type == "cartesian" && isTRUE(is_ll)) {
                warning("User forced 'cartesian' but SF object is Lat/Long. Ensure this is intended.")
            }
        }

        # Extract centroids
        suppressWarnings({
            cents <- sf::st_centroid(sf::st_geometry(x))
            coords_mat <- sf::st_coordinates(cents)
        })

        col1_raw <- coords_mat[, 1] # X / Long
        col2_raw <- coords_mat[, 2] # Y / Lat
    } else {
        # -------------------------------------------------------------------------
        # Case 2: Data Frame
        # -------------------------------------------------------------------------
        if (is.null(coords) || length(coords) != 2) {
            stop("For data.frames, 'coords' must be a vector of two column names (e.g. c('long', 'lat') or c('x', 'y'))")
        }

        col1_raw <- x[[coords[1]]] # First coord (X or Long)
        col2_raw <- x[[coords[2]]] # Second coord (Y or Lat)

        if (coord_type_arg == "auto") {
            # Heuristic: Check if fit within Lat/Long bounds
            # X/Long: -180 to 180
            # Y/Lat: -90 to 90

            # Simple check: are they reasonably small numbers?
            is_valid_ll <- all(col1_raw >= -180 & col1_raw <= 180, na.rm = TRUE) &&
                all(col2_raw >= -90 & col2_raw <= 90, na.rm = TRUE)

            if (is_valid_ll) {
                final_type <- "latlong"
                # message("Auto-detected Lat/Long coordinates from data range.")
            } else {
                final_type <- "cartesian"
                # message("Auto-detected Cartesian coordinates from data range.")
            }
        } else {
            final_type <- coord_type_arg
        }
    }

    # -------------------------------------------------------------------------
    # Format Outputs
    # -------------------------------------------------------------------------
    # SaTScan File Specs:
    # 1. Lat/Long:  Lat (Y) first, then Long (X)
    # 2. Cartesian: X first, then Y

    out_df <- data.frame(
        loc_id = ids,
        stringsAsFactors = FALSE
    )

    if (final_type == "latlong") {
        # Input assumed: col1=Long(X), col2=Lat(Y)
        # Output required: Lat, Long
        out_df$coord1 <- col2_raw # Lat
        out_df$coord2 <- col1_raw # Long
    } else {
        # Input assumed: col1=X, col2=Y
        # Output required: X, Y
        out_df$coord1 <- col1_raw
        out_df$coord2 <- col2_raw
    }

    # Select only required cols
    out_df <- out_df[, c("loc_id", "coord1", "coord2")]

    satscan_table(
        data = out_df,
        kind = "geo",
        spec = list(coord_type = final_type)
    )
}
