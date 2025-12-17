#' Prepare SaTScan Geometry File
#'
#' Prepares a coordinate file for SaTScan.
#' Handles \code{sf} objects (extracting centroids) or plain data frames.
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
#'   Order should be \code{c("long", "lat")} or \code{c("x", "y")}.
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
#' geo_obj2 <- prep_geo(df, loc_id = id, coords = c("lon", "lat"))
#'
#' # From data frame (Cartesian)
#' df_cart <- data.frame(id = "A", x = 500000, y = 100000)
#' geo_obj3 <- prep_geo(df_cart, loc_id = id, coords = c("x", "y"))
#' }
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @importFrom sf st_centroid st_coordinates st_geometry st_is_longlat
#' @export
prep_geo <- function(x, loc_id, coords = NULL, geometry = NULL) {
    id_quo <- rlang::enquo(loc_id)
    if (rlang::quo_is_null(id_quo)) stop("loc_id is required")

    ids <- as.character(rlang::eval_tidy(id_quo, x))

    coord_type <- "latlong" # Default assumption unless cartesian is obvious or user specifies?
    # The spec says: "Store coordinate type in spec$coord_type"
    # We should detect it.

    if (inherits(x, "sf")) {
        # It is sf
        # Check crs
        if (sf::st_is_longlat(x)) {
            coord_type <- "latlong"
        } else {
            # Check if projected but we want latlong?
            # Usually SaTScan works best with LatLong for global, or Cartesian for local.
            # We will extract whatever is there.
            # Use simple heuristic: if CRS is NA or projected, assume Cartesian unless we transform?
            # User guide says "If "latlong", sf data is transformed to WGS84" in old api.
            # But here we are just preparing data.
            # Let's extract coordinates.
            if (is.na(sf::st_is_longlat(x))) {
                coord_type <- "cartesian"
            } else {
                coord_type <- "cartesian" # projected
            }
        }

        # Extract centroids
        suppressWarnings({
            cents <- sf::st_centroid(sf::st_geometry(x))
            coords_mat <- sf::st_coordinates(cents)
        })

        col1 <- coords_mat[, 1]
        col2 <- coords_mat[, 2]
    } else {
        # Data.frame
        if (is.null(coords) || length(coords) != 2) {
            stop("For data.frames, 'coords' must be a vector of two column names (e.g. c('long', 'lat'))")
        }

        col1 <- x[[coords[1]]]
        col2 <- x[[coords[2]]]

        # Simple heuristic for type if not provided?
        # Spec says: "cartesian or lat/lon coordinates... Store coordinate type"
        # User instructions: "If additional scan centers are needed, use .grd, not fake cases"
        # Nothing strictly about enforcing cartesian vs latlong here, just defaulting.
        # We'll default to 'latlong' if values are within -180/180 and -90/90?
        # Or just assume latlong unless specified?
        # Actually, we can't switch types here easily without user input.
        # We will assume "latlong" by default for the spec, but maybe add an argument `geo_type` to `prep_geo`?
        # The signature in plan was `prep_geo(x, loc_id, coords, ...)`. I'll assume users handle projection.
        # But I DO need to put something in `spec$coord_type`.
        # I'll default to 'latlong' but allow override via `...` if needed, OR just leave it null and let `satscanr` decide/default?
        # Better: check if values look like lat/long.
        if (all(col1 >= -180 & col1 <= 180, na.rm = T) && all(col2 >= -90 & col2 <= 90, na.rm = T)) {
            coord_type <- "latlong"
        } else {
            coord_type <- "cartesian"
        }
    }

    out_df <- data.frame(
        loc_id = ids,
        x = col1, # SaTScan: Lat/Long or X/Y. Order matters.
        # SaTScan Geo File: LocationID, Lat, Long (or X, Y)
        # WAIT: SaTScan expects Lat, then Long?
        # rsatscan docs say: "Location ID, Latitude/Y, Longitude/X"
        # So if I have (Long, Lat) from sf, I need to swap them for file writing?
        # Standard ss.options: CoordinatesFile.
        # If I extract st_coordinates(centroids), it returns (X, Y) which is (Long, Lat).
        # So col1 is Long, col2 is Lat.
        # If I write them as Long, Lat -> SaTScan reads "Lat, Long" -> Errors.
        # So I MUST write Lat, Long (Y, X).
        # Let's clean this up.
        stringsAsFactors = FALSE
    )

    # Fix order for SaTScan:
    # If Lat/Long: SaTScan expects <Lat> <Long> (Y, X).
    # If Cartesian: SaTScan expects <X> <Y>.

    if (coord_type == "latlong") {
        # Swap: col1 is Long(X), col2 is Lat(Y) -> Lat, Long
        out_df$coord1 <- col2
        out_df$coord2 <- col1
    } else {
        # Cartesian: col1 is X, col2 is Y -> X, Y
        out_df$coord1 <- col1
        out_df$coord2 <- col2
    }

    # Select only required cols
    out_df <- out_df[, c("loc_id", "coord1", "coord2")]

    satscan_table(
        data = out_df,
        kind = "geo",
        spec = list(coord_type = coord_type)
    )
}
