#' Prepare SaTScan Grid File
#'
#' `r lifecycle::badge("superseded")`
#'
#' Prepares a grid file for SaTScan, which defines the centers of the scanning windows.
#' Leverages \code{prep_geo} logic for coordinate extraction and formatting.
#'
#' @seealso [as_satscan_grid()] for the new `ss_tbl` interface.
#'
#' @section SaTScan File Specification:
#' The Grid File has the same structure as the Coordinates File:
#' \code{<LocationID> <Latitude/Y> <Longitude/X>}
#'
#' \itemize{
#'   \item \strong{LocationID}: Unique identifier for the grid point.
#'   \item \strong{Coordinates}: Same rules as \code{prep_geo} (Lat/Long order vs X/Y order).
#' }
#'
#' @param x Input object (sf or data.frame).
#' @param loc_id Column name for location ID (unquoted).
#' @param coords Character vector of length 2 (required if x is data.frame).
#' @return A \code{satscan_table} object of kind "grd".
#'
#' @examples
#' \dontrun{
#' # From sf grid
#' grid_sf <- sf::st_make_grid(study_area, n = c(10, 10)) %>% sf::st_sf()
#' grid_sf$id <- 1:nrow(grid_sf)
#'
#' grd_obj <- prep_grd(grid_sf, loc_id = id)
#' }
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @export
prep_grd <- function(x, loc_id = NULL, coords = NULL) {
    # Reuse prep_geo logic but change kind
    # Grid file format is identical to Geo file: ID, X, Y

    # We can just call prep_geo and modify the kind?
    # `prep_geo` returns a validated object.
    # We can't modify the kind easily without hacking the object structure or adding a 'kind' arg to prep_geo.
    # I'll just call prep_geo essentially.

    # Copying some logic for safety and clarity since prep_geo might evolve.

    # ...Actually, `prep_geo` does exactly what we need for extracting coordinates.
    # Let's modify `prep_geo` to accept a `kind` argument? No, that breaks the API purity.
    # I will just invoke prep_geo logic.

    # If loc_id is missing?
    # prep_geo requires loc_id.
    # If user didn't provide one, maybe we generate?
    # But usually grid points need IDs to be referenced in results?
    # Actually SaTScan output references Cluster Center ID.

    # Let's just wrap prep_geo's logic.

    geo_obj <- prep_geo(x, loc_id = {{ loc_id }}, coords = coords)

    # Change kind
    geo_obj$kind <- "grd"

    # Re-validate?
    # validate_satscan_table(geo_obj)

    geo_obj
}
