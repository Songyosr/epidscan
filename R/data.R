#' New Mexico Lung Cancer Data (Case File)
#'
#' Lung cancer cases in New Mexico counties, 1973-1991.
#'
#' @format A data frame with 7,793 rows and 5 variables:
#' \describe{
#'   \item{county}{County name (Location ID).}
#'   \item{cases}{Number of cases (1 for individual records).}
#'   \item{date}{Date of diagnosis (Month/Year precision, day set to 1).}
#'   \item{age_group}{Age group (18 levels: <5, 5-9, ..., 85+).}
#'   \item{sex}{Sex (Male, Female).}
#' }
#' @source \url{https://www.satscan.org/datasets/nmlung/index.html}
"NMlung_cas"

#' New Mexico Lung Cancer Data (Population File)
#'
#' Yearly population counts for New Mexico counties, 1973-1991.
#'
#' @format A data frame with 66,115 rows and 5 variables:
#' \describe{
#'   \item{county}{County name (Location ID).}
#'   \item{year}{Census year (Integer).}
#'   \item{population}{Population count.}
#'   \item{age_group}{Age group (18 levels: <5, 5-9, ..., 85+).}
#'   \item{sex}{Sex (Male, Female).}
#' }
#' @source \url{https://www.satscan.org/datasets/nmlung/index.html}
"NMlung_pop"

#' New Mexico Lung Cancer Data (Coordinates)
#'
#' Centroid coordinates for New Mexico counties (Cartesian).
#'
#' @format A data frame with 32 rows and 3 variables:
#' \describe{
#'   \item{county}{County name (Location ID).}
#'   \item{x_km}{X coordinate in km.}
#'   \item{y_km}{Y coordinate in km.}
#' }
#' @source \url{https://www.satscan.org/datasets/nmlung/index.html}
"NMlung_geo"

#' @examples
#' \dontrun{
#' # Load data
#' data(NMlung_cas)
#' data(NMlung_pop)
#' data(NMlung_geo)
#'
#' # Create ss_tbl objects
#' ss_cas <- ss_cas(
#'     NMlung_cas,
#'     loc_id = "county",
#'     cases = "cases",
#'     time = "date",
#'     covars = c("age_group", "sex"),
#'     time_precision = "month"
#' )
#'
#' ss_pop <- ss_pop(
#'     NMlung_pop,
#'     loc_id = "county",
#'     time = "year",
#'     population = "population",
#'     covars = c("age_group", "sex"),
#'     time_precision = "year"
#' )
#'
#' ss_geo <- ss_geo(
#'     NMlung_geo,
#'     loc_id = "county",
#'     coord1 = "x_km",
#'     coord2 = "y_km",
#'     coord_type = "cartesian"
#' )
#'
#' # Run Analysis
#' # (Assuming satscanr() is available and configured)
#' # result <- satscanr(
#' #   cas = ss_cas,
#' #   pop = ss_pop,
#' #   geo = ss_geo,
#' #   analysis_type = "space_time"
#' # )
#' }
