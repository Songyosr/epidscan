#' SaTScan Schema Registry
#'
#' Defines the supported SaTScan file types, their required/optional column roles,
#' file extensions, and default specifications.
#'
#' @return A named list of schema definitions.
#' @keywords internal
ss_schema <- function() {
    list(
        cas = list(
            ext = ".cas",
            required_roles = c("loc_id", "cases"),
            optional_roles = c("time", "attribute", "censored", "weight", "covars"),
            defaults = list(time_precision = NULL)
        ),
        ctl = list(
            ext = ".ctl",
            required_roles = c("loc_id", "controls"),
            optional_roles = c("time", "covars"),
            defaults = list(time_precision = NULL)
        ),
        pop = list(
            ext = ".pop",
            required_roles = c("loc_id", "time", "population"),
            optional_roles = c("covars"),
            defaults = list(time_precision = NULL)
        ),
        geo = list(
            ext = ".geo",
            required_roles = c("loc_id", "coord1", "coord2"),
            optional_roles = c("z"),
            defaults = list(coord_type = c("latlong", "cartesian", "auto"))
        ),
        grd = list(
            ext = ".grd",
            required_roles = c("coord1", "coord2"),
            optional_roles = c("z", "earliest_start", "latest_start", "earliest_end", "latest_end"),
            defaults = list(grid_variant = "basic")
        ),
        nwk = list(
            ext = ".nwk",
            required_roles = c("loc_id", "neighbor_id"),
            optional_roles = c("distance"),
            defaults = list(distance_units = NULL)
        ),
        nbr = list(
            ext = ".nbr",
            required_roles = c("loc_id", "neighbors"), # 'neighbors' is a placeholder for 1..N columns
            optional_roles = character(0),
            defaults = list()
        ),
        met = list(
            ext = ".met",
            required_roles = c("meta_loc_id", "member_ids"), # 'member_ids' is a placeholder for 1..N columns
            optional_roles = character(0),
            defaults = list()
        ),
        max = list(
            ext = ".max",
            required_roles = c("loc_id", "population"),
            optional_roles = character(0),
            defaults = list()
        ),
        adj = list(
            ext = ".adj",
            required_roles = c("loc_id", "relative_risk"),
            optional_roles = c("start_time", "end_time"),
            defaults = list(time_precision = NULL)
        ),
        ha = list(
            ext = ".ha",
            required_roles = c("loc_id", "relative_risk"),
            optional_roles = c("start_time", "end_time"),
            defaults = list(time_precision = NULL, multiple_hypotheses = TRUE)
        )
    )
}

#' Create a SaTScan Table
#'
#'
#' @description
#' Creates a new `ss_tbl` object, which is a `vctrs` subclass of `data.frame` tailored for SaTScan data.
#'
#' @param data A data.frame.
#' @param type type string (e.g. "cas", "geo").
#' @param roles Named character vector mapping schema roles to data columns.
#' @param spec List of additional specifications.
#' @export
new_ss_tbl <- function(data, type, roles, spec = list()) {
    stopifnot(is.data.frame(data))
    schema <- ss_schema()
    type <- match.arg(type, choices = names(schema))

    if (is.null(names(roles)) || any(names(roles) == "")) {
        stop("`roles` must be a named character vector: role -> column_name.")
    }

    # Check all mapped columns exist in data
    if (!all(unname(roles) %in% names(data))) {
        missing_cols <- setdiff(unname(roles), names(data))
        stop("Missing columns in `data`: ", paste(missing_cols, collapse = ", "))
    }

    # Validate required roles exist
    req <- schema[[type]]$required_roles
    # For dynamic types like nbr/met, the second required role is a placeholder for multiple columns
    # so we only rigorously check the first ID role, + the existence of the specific dynamic roles
    if (type %in% c("nbr", "met")) {
        # Check primary ID role
        id_role <- req[1]
        if (!id_role %in% names(roles)) {
            stop("Missing required role '", id_role, "' for type '", type, "'.")
        }
        # For neighbors/members, we expect them to be in strict order in roles or spec?
        # Actually, as_satscan_neighbors passes them as mapped roles for "neighbor_1", "neighbor_2", etc.
        # But new_ss_tbl doesn't strictly validate the count of N neighbors here, just that passed roles cover the basics?
        # We'll trust the constructor helpers for the dynamic part.
    } else {
        if (!all(req %in% names(roles))) {
            stop(
                "Missing required roles for type='", type, "': ",
                paste(setdiff(req, names(roles)), collapse = ", ")
            )
        }
    }

    # Compute output order:
    # 1. Standard required roles
    # 2. Dynamic wide columns (neighbors/members)
    ss_order <- character(0)

    if (type == "nbr") {
        # Order: loc_id, then all neighbor cols in user-provided order
        id_col <- unname(roles[req[1]])
        # Extract dynamic neighbor columns from spec or roles?
        # User-facing 'as_satscan_neighbors' puts them in 'spec$neighbor_cols'
        if (is.null(spec$neighbor_cols)) stop("Internal error: 'neighbor_cols' missing from spec for 'nbr'.")
        ss_order <- c(id_col, spec$neighbor_cols)
    } else if (type == "met") {
        id_col <- unname(roles[req[1]])
        if (is.null(spec$member_cols)) stop("Internal error: 'member_cols' missing from spec for 'met'.")
        ss_order <- c(id_col, spec$member_cols)
    } else {
        # Standard types
        ss_order <- unname(roles[req])
    }

    # Spec merge
    defaults <- schema[[type]]$defaults
    merged_spec <- utils::modifyList(defaults, spec)

    vctrs::new_data_frame(
        data,
        class = c("ss_tbl", "data.frame"),
        ss_type = type,
        ss_roles = roles,
        ss_order = ss_order,
        ss_spec = merged_spec
    )
}

#' Check if Object is an ss_tbl
#'
#' @param x Object to check.
#' @return Logical.
#' @export
is_ss_tbl <- function(x) inherits(x, "ss_tbl")

#' Get ss_tbl Type
#'
#' @param x An `ss_tbl` object.
#' @return Character string (e.g., "cas", "geo").
#' @export
ss_type <- function(x) attr(x, "ss_type", exact = TRUE)

#' Get ss_tbl Roles
#'
#' @param x An `ss_tbl` object.
#' @return Named character vector of role mappings.
#' @export
ss_roles <- function(x) attr(x, "ss_roles", exact = TRUE)

#' Get ss_tbl Column Order
#'
#' @param x An `ss_tbl` object.
#' @return Character vector of column names in SaTScan output order.
#' @export
ss_order <- function(x) attr(x, "ss_order", exact = TRUE)

#' Get ss_tbl Specification
#'
#' @param x An `ss_tbl` object.
#' @return List of specifications.
#' @export
ss_spec <- function(x) attr(x, "ss_spec", exact = TRUE)

#' @export
print.ss_tbl <- function(x, ...) {
    cat("<ss_tbl:", ss_type(x), ">\n")
    cat("Roles:\n")
    r <- ss_roles(x)
    # Print first few roles to avoid spam
    print_r <- if (length(r) > 10) c(r[1:10], "..." = paste(length(r) - 10, "more")) else r
    for (nm in names(print_r)) cat("  - ", nm, " -> ", print_r[[nm]], "\n", sep = "")

    sp <- ss_spec(x)
    if (length(sp)) {
        cat("Spec:\n")
        for (nm in names(sp)) {
            val <- sp[[nm]]
            if (length(val) > 5) val <- c(val[1:5], "...")
            cat("  - ", nm, ": ", paste(val, collapse = ", "), "\n", sep = "")
        }
    }
    cat("Data:", nrow(x), "rows x", ncol(x), "cols\n")
    print(utils::head(as.data.frame(x)))
    invisible(x)
}

# ---- User-Facing Coercers ----

#' Coerce Data to SaTScan Case Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"cas"` (SaTScan Case File).
#' A case file provides the observed case counts by location and, optionally,
#' by time and covariate strata. SaTScan uses these counts as the primary
#' numerator when evaluating candidate spatial/temporal clusters against the
#' null model.
#'
#' This constructor does not alter the input data. It records which columns
#' represent the required SaTScan fields (location ID, case count, and optional
#' time/covariates). Any time formatting is deferred to write-time via
#' `write_satscan()` (and `format_satscan_time()`), based on `time_precision`.
#'
#' @section SaTScan File Specification:
#' The Case File has the following structure:
#' \code{<LocationID> <NoCases> <Date> <Covariate1> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Character or numeric identifier. Matching ID must exist in Geo file.
#'   \item \strong{NoCases}: Number of cases. For "casewise" style without a count column, this is set to 1.
#'     Zero-case rows are removed (implicit zeros).
#'   \item \strong{Date}: Formatted according to \code{time_precision}:
#'     \itemize{
#'       \item "day": \code{YYYY/MM/DD}
#'       \item "month": \code{YYYY/MM}
#'       \item "year": \code{YYYY}
#'       \item "generic": Time string/number
#'     }
#'   \item \strong{Covariates}: Optional categorical variables.
#' }
#'
#' @param data A data frame containing case data.
#' @param loc_id Column name for location ID (character or numeric).
#' @param cases Column name for case counts (numeric).
#' @param time Optional column name for time (Date, numeric, or character).
#' @param covars Optional vector of column names for covariates.
#' @param time_precision User-specified time precision ("day", "month", "year", "generic").
#'
#' @return An `ss_tbl` object of type "cas".
#'
#' @examples
#' df <- data.frame(id = c("A", "B"), count = c(10, 5), year = 2020)
#' ss_cas_obj <- ss_cas(df, loc_id = "id", cases = "count", time = "year", time_precision = "year")
#'
#' @export
ss_cas <- function(data, loc_id, cases, time = NULL, covars = NULL,
                   time_precision = NULL) {
    roles <- c(loc_id = loc_id, cases = cases)
    if (!is.null(time)) roles <- c(roles, time = time)

    spec <- list()
    if (!is.null(time_precision)) spec$time_precision <- time_precision
    if (!is.null(covars)) spec$covars <- covars

    new_ss_tbl(data, "cas", roles, spec)
}

#' Coerce Data to SaTScan Control Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"ctl"` (SaTScan Control File).
#' A control file provides the number of controls by location and, optionally,
#' by time and covariate strata. When used with a case file (Bernoulli models),
#' SaTScan compares the spatial/temporal concentration of cases against the
#' background distribution implied by controls.
#'
#' This constructor preserves the original data and stores column role mappings.
#' Time precision is not enforced here; if `time` is provided, it will be formatted
#' at write-time (via `write_satscan()`), using `time_precision` if supplied.
#'
#' @section SaTScan File Specification:
#' The Control File has the following structure:
#' \code{<LocationID> <NoControls> <Date> <Covariate1> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Identifier matching Case/Geo files.
#'   \item \strong{NoControls}: Number of controls.
#'   \item \strong{Date}: Required for Space-Time Bernoulli. Same format rules as Case file.
#'   \item \strong{Covariates}: Optional.
#' }
#'
#' @param data A data frame containing control data.
#' @param loc_id Column name for location ID.
#' @param controls Column name for control counts.
#' @param time Optional column name for time.
#' @param covars Optional vector of column names for covariates.
#' @param time_precision User-specified time precision.
#'
#' @return An `ss_tbl` object of type "ctl".
#'
#' @export
ss_ctl <- function(data, loc_id, controls, time = NULL, covars = NULL,
                   time_precision = NULL) {
    roles <- c(loc_id = loc_id, controls = controls)
    if (!is.null(time)) roles <- c(roles, time = time)

    spec <- list()
    if (!is.null(time_precision)) spec$time_precision <- time_precision
    if (!is.null(covars)) spec$covars <- covars

    new_ss_tbl(data, "ctl", roles, spec)
}

#' Coerce Data to SaTScan Population Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"pop"` (SaTScan Population File).
#' A population file provides the population-at-risk by location and time and
#' is used as the denominator for Poisson-based scan statistics. If multiple
#' time points are supplied, SaTScan may interpolate population size between
#' time anchors depending on the analysis setup.
#'
#' This constructor records the mappings for location ID, time, population, and
#' optional covariate strata without changing the input data. Any time formatting
#' is deferred to `write_satscan()`, using `time_precision` if provided.
#'
#' @section SaTScan File Specification:
#' The Population File has the following structure:
#' \code{<LocationID> <Year/Time> <Population> <Covariate1> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Character or numeric identifier. Match cases and geo.
#'   \item \strong{Year/Time}: The "census year" or time point. For daily analysis, you can still use
#'     yearly population anchors (e.g. 2023, 2024). SaTScan interpolates linearly.
#'   \item \strong{Population}: The count of people at risk.
#'   \item \strong{Covariates}: Optional. If used, population must be stratified by these covariates.
#' }
#'
#' @param data A data frame containing population data.
#' @param loc_id Column name for location ID.
#' @param time Column name for time (census year/time).
#' @param population Column name for population count.
#' @param covars Optional vector of column names for covariates.
#' @param time_precision User-specified time precision (e.g. "year" to format Dates as YYYY).
#'
#' @return An `ss_tbl` object of type "pop".
#'
#' @export
ss_pop <- function(data, loc_id, time, population, covars = NULL,
                   time_precision = NULL) {
    roles <- c(loc_id = loc_id, time = time, population = population)

    spec <- list()
    if (!is.null(time_precision)) spec$time_precision <- time_precision
    if (!is.null(covars)) spec$covars <- covars

    new_ss_tbl(data, "pop", roles, spec)
}

#' Coerce Data to SaTScan Coordinates Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"geo"` (SaTScan Coordinates File).
#' A coordinates file defines the spatial location of each analysis unit,
#' and SaTScan uses these locations to compute distances and to construct
#' circular/elliptic scanning windows. Coordinates may be provided in
#' latitude/longitude or Cartesian units; the required output column order
#' differs between these coordinate types.
#'
#' If `data` is an `sf` object, this function extracts feature centroids and
#' maps them to SaTScan’s expected coordinate order (Lat, Long for lat/long
#' CRS; X, Y for projected CRS). If `data` is a plain data frame, it records
#' the user-supplied coordinate columns and (when `coord_type = "auto"`)
#' applies a simple heuristic to infer lat/long vs Cartesian. No projection
#' or coordinate transformation is performed.
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
#' @param data A data frame or `sf` object containing coordinate data.
#' @param loc_id Column name for location ID.
#' @param coord1 Column name for the first coordinate (X/Longitude), required if `data` is not `sf`.
#' @param coord2 Column name for the second coordinate (Y/Latitude), required if `data` is not `sf`.
#' @param z Optional column name for altitude/Z-coordinate.
#' @param coord_type Coordinate system: "auto" (default), "latlong", or "cartesian".
#'
#' @return An `ss_tbl` object of type "geo".
#'
#' @export
ss_geo <- function(data, loc_id, coord1 = NULL, coord2 = NULL, z = NULL,
                   coord_type = c("auto", "latlong", "cartesian")) {
    coord_type <- match.arg(coord_type)

    # SF Handling
    if (inherits(data, "sf")) {
        # Check CRS
        is_ll <- sf::st_is_longlat(data)
        crs <- sf::st_crs(data)

        # Auto-detect type
        final_type <- if (coord_type == "auto") {
            if (isTRUE(is_ll)) "latlong" else "cartesian"
        } else {
            coord_type
        }

        # Validate forced types
        if (coord_type == "latlong" && isFALSE(is_ll)) warning("Forcing 'latlong' on projected SF object.")
        if (coord_type == "cartesian" && isTRUE(is_ll)) warning("Forcing 'cartesian' on lat/long SF object.")

        # Extract centroids
        suppressWarnings({
            cents <- sf::st_centroid(sf::st_geometry(data))
            coords_mat <- sf::st_coordinates(cents)
        })

        # Create plain dataframe
        # ensure loc_id is preserved
        if (!loc_id %in% names(data)) stop("loc_id '", loc_id, "' not found in sf object.")

        df_out <- as.data.frame(data)
        df_out <- df_out[, c(loc_id, setdiff(names(df_out), c(loc_id, attr(data, "sf_column")))), drop = FALSE]

        # Add coords with standard internal names
        # SF returns X (Long) then Y (Lat)
        # We'll assign them to temporary names and map them later
        df_out[["__ss_c1"]] <- coords_mat[, 1] # X / Long
        df_out[["__ss_c2"]] <- coords_mat[, 2] # Y / Lat

        # If latlong:  SaTScan wants Lat(Y), Long(X)
        # If cartesian: SaTScan wants X, Y
        # new_ss_tbl expects us to map roles 'coord1', 'coord2'.
        # For 'geo' schema:
        #  - coord1 is the first column in the file.
        #  - coord2 is the second column in the file.

        if (final_type == "latlong") {
            # File: Map Lat(Y) -> coord1, Long(X) -> coord2
            roles <- c(loc_id = loc_id, coord1 = "__ss_c2", coord2 = "__ss_c1")
        } else {
            # File: Map X -> coord1, Y -> coord2
            roles <- c(loc_id = loc_id, coord1 = "__ss_c1", coord2 = "__ss_c2")
        }

        spec <- list(coord_type = final_type)
        if (!is.null(z)) {
            roles <- c(roles, z = z)
            spec$z <- z
        }

        return(new_ss_tbl(df_out, "geo", roles, spec))
    } else {
        # Data Frame Handling
        if (is.null(coord1) || is.null(coord2)) {
            stop("Must provide 'coord1' and 'coord2' for non-sf data frames.")
        }

        c1_vals <- data[[coord1]]
        c2_vals <- data[[coord2]]

        final_type <- coord_type
        if (coord_type == "auto") {
            # Heuristic: -180...180, -90...90
            is_valid_ll <- all(c1_vals >= -180 & c1_vals <= 180, na.rm = TRUE) &&
                all(c2_vals >= -90 & c2_vals <= 90, na.rm = TRUE)
            final_type <- if (is_valid_ll) "latlong" else "cartesian"
        }

        # Roles Mapping
        # Assume user provided X/Long as coord1, Y/Lat as coord2 (Standard R/GIS convention)
        # BUT SaTScan lat/long wants Y then X.

        if (final_type == "latlong") {
            # Swap: Map user's coord2 (Lat) -> file's coord1
            #       Map user's coord1 (Long) -> file's coord2
            roles <- c(loc_id = loc_id, coord1 = coord2, coord2 = coord1)
        } else {
            # Keep: Map user's coord1 (X) -> file's coord1
            #       Map user's coord2 (Y) -> file's coord2
            roles <- c(loc_id = loc_id, coord1 = coord1, coord2 = coord2)
        }

        spec <- list(coord_type = final_type)
        if (!is.null(z)) {
            roles <- c(roles, z = z) # Map Z role to column name z
            spec$z <- z
        }

        return(new_ss_tbl(data, "geo", roles, spec))
    }
}

#' Coerce Data to SaTScan Grid Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"grd"` (SaTScan Grid File).
#' A grid file specifies the set of candidate centroids at which SaTScan
#' evaluates potential clusters. When a grid is supplied, SaTScan scans only
#' at these grid points (rather than at every observed location), which can
#' reduce computation and enforce a user-defined scanning lattice.
#'
#' This constructor records the coordinate columns (and optional constraints)
#' without modifying the input data. Coordinate ordering rules match `ss_geo`
#' and are applied by the writer when producing the SaTScan ASCII file.
#'
#' @section SaTScan File Specification:
#' The Grid File has the same structure as the Coordinates File:
#' \code{<LocationID> <Latitude/Y> <Longitude/X>}
#'
#' \itemize{
#'   \item \strong{LocationID}: Unique identifier for the grid point.
#'   \item \strong{Coordinates}: Same rules as \code{ss_geo} (Lat/Long order vs X/Y order).
#' }
#'
#' @param data A data frame containing grid locations.
#' @param coord1 Column name for the first coordinate.
#' @param coord2 Column name for the second coordinate.
#' @param z Optional column name for Z-coordinate.
#' @param earliest_start Optional column for earliest start time constraint.
#' @param latest_start Optional column for latest start time constraint.
#' @param earliest_end Optional column for earliest end time constraint.
#' @param latest_end Optional column for latest end time constraint.
#' @param grid_variant Variant of grid file ("basic", etc.).
#'
#' @return An `ss_tbl` object of type "grd".
#'
#' @export
ss_grd <- function(data, coord1, coord2, z = NULL,
                   earliest_start = NULL, latest_start = NULL,
                   earliest_end = NULL, latest_end = NULL,
                   grid_variant = "basic") {
    roles <- c(coord1 = coord1, coord2 = coord2)
    spec <- list(grid_variant = grid_variant)

    if (!is.null(z)) spec$z <- z
    if (!is.null(z)) roles <- c(roles, z = z)

    if (!is.null(earliest_start)) roles <- c(roles, earliest_start = earliest_start)
    if (!is.null(latest_start)) roles <- c(roles, latest_start = latest_start)
    if (!is.null(earliest_end)) roles <- c(roles, earliest_end = earliest_end)
    if (!is.null(latest_end)) roles <- c(roles, latest_end = latest_end)
    new_ss_tbl(data, "grd", roles, spec)
}

#' Coerce Data to SaTScan Network Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"nwk"` (SaTScan Network File).
#' A network file defines adjacency between locations as an edge list,
#' optionally with a distance or weight for each connection. SaTScan can use
#' this structure to define cluster expansion on a network (rather than by
#' Euclidean distance), making it suitable for road networks or other
#' connectivity-based proximity.
#'
#' This constructor records which columns define the source location, the
#' neighbor location, and the optional distance/weight. It does not compute
#' distances or alter the network; those decisions are deferred to SaTScan
#' and the analysis configuration.
#'
#' @section SaTScan File Specification:
#' The Network File has the following structure:
#' \code{<LocationID> <NeighborID> <Distance>}
#'
#' \itemize{
#'   \item \strong{LocationID}: Identifier for the source location.
#'   \item \strong{NeighborID}: Identifier for the connected neighbor location.
#'   \item \strong{Distance}: Optional. Numeric distance or weight between the two locations.
#' }
#'
#' @param data A data frame containing network edges.
#' @param loc_id Column name for the source location ID.
#' @param neighbor_id Column name for the connected neighbor ID.
#' @param distance Optional column name for distance/weight for the connection.
#' @param distance_units Units for distance (optional string).
#'
#' @return An `ss_tbl` object of type "nwk".
#'
#' @export
ss_nwk <- function(data, loc_id, neighbor_id, distance = NULL,
                   distance_units = NULL) {
    roles <- c(loc_id = loc_id, neighbor_id = neighbor_id)
    if (!is.null(distance)) roles <- c(roles, distance = distance)

    spec <- list()
    if (!is.null(distance_units)) spec$distance_units <- distance_units

    new_ss_tbl(data, "nwk", roles, spec)
}

#' Coerce Data to SaTScan Neighbors Table
#'
#' @description
#' Creates an `ss_tbl` object of type `"nbr"` (SaTScan Non-Euclidean Neighbors File).
#' A neighbors file defines, for each centroid location, an ordered list of
#' neighboring locations ranked by closeness. SaTScan constructs candidate
#' clusters by progressively expanding each row (centroid, then centroid + first
#' neighbor, then + second neighbor, etc.) until the row ends.
#'
#' This constructor expects neighbors in a wide format (one row per centroid,
#' multiple neighbor columns in rank order). It preserves the input data and
#' records `neighbor_cols` so the writer can emit the correct SaTScan ASCII
#' structure without reshaping user data.
#'
#' @section SaTScan File Specification:
#' The Neighbors File has the following structure:
#' \code{<LocationID> <Neighbor1ID> <Neighbor2ID> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Identifier for the location.
#'   \item \strong{NeighborIDs}: One or more columns, each representing a neighbor of the \code{LocationID}.
#'     The order of these columns matters for SaTScan's interpretation of neighbor hierarchy.
#' }
#'
#' @param data A data frame containing neighbor lists.
#' @param loc_id Column name for the location ID.
#' @param neighbor_cols Character vector of column names representing neighbors.
#'
#' @return An `ss_tbl` object of type "nbr".
#'
#' @export
ss_nbr <- function(data, loc_id, neighbor_cols) {
    # neighbor_cols: character vector of column names providing neighbors in order
    roles <- c(loc_id = loc_id)
    spec <- list(neighbor_cols = neighbor_cols)

    # We don't map individual neighbors to separate roles in 'roles' vector to avoid clutter,
    # but we MUST verify they exist.
    missing <- setdiff(neighbor_cols, names(data))
    if (length(missing) > 0) stop("Missing neighbor columns: ", paste(missing, collapse = ", "))

    new_ss_tbl(data, "nbr", roles, spec)
}

#' Coerce Data to SaTScan Meta Locations
#'
#' @description
#' Creates an `ss_tbl` object of type `"met"` (SaTScan Meta Locations File).
#' A meta locations file defines groupings of multiple locations into a single
#' meta-location (e.g., aggregating tracts into counties). SaTScan uses these
#' groupings to treat members as belonging to the same higher-level unit when
#' a meta-location analysis is requested.
#'
#' This constructor expects a wide representation (one row per meta-location,
#' with one or more member columns). It preserves the input data and records
#' `member_cols` so the writer can output the correct SaTScan ASCII format.
#'
#' @section SaTScan File Specification:
#' The Meta Locations File has the following structure:
#' \code{<MetaLocationID> <Member1ID> <Member2ID> ...}
#'
#' \itemize{
#'   \item \strong{MetaLocationID}: Identifier for the meta-location (e.g., a county).
#'   \item \strong{MemberIDs}: One or more columns, each representing a member location (e.g., a census tract)
#'     that belongs to the \code{MetaLocationID}.
#' }
#'
#' @param data A data frame containing meta-location definitions.
#' @param meta_loc_id Column name for the meta-location ID.
#' @param member_cols Character vector of column names representing member locations.
#'
#' @return An `ss_tbl` object of type "met".
#'
#' @export
ss_met <- function(data, meta_loc_id, member_cols) {
    roles <- c(meta_loc_id = meta_loc_id)
    spec <- list(member_cols = member_cols)
    missing <- setdiff(member_cols, names(data))
    if (length(missing) > 0) stop("Missing member columns: ", paste(missing, collapse = ", "))

    new_ss_tbl(data, "met", roles, spec)
}


# ---- Write-Time Formatting ----

#' Format Time for SaTScan
#'
#' @param x Vector of times (Date, POSIXt, numeric, character)
#' @param time_precision "day", "month", "year", or "generic"
#' @return Character vector formatted for SaTScan
#' @export
format_satscan_time <- function(x, time_precision) {
    if (is.null(time_precision)) {
        return(as.character(x))
    }

    time_precision <- match.arg(time_precision, c("day", "month", "year", "generic"))
    if (time_precision == "generic") {
        return(as.character(x))
    }

    if (time_precision == "day") {
        # If already Date or POSIXt, format safely
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
            return(format(x, "%Y/%m/%d"))
        }
        # Try coercion
        d <- suppressWarnings(as.Date(x))
        if (all(is.na(d) & !is.na(x))) stop("Cannot parse time to Date for 'day' precision.")
        return(format(d, "%Y/%m/%d"))
    }

    if (time_precision == "month") {
        # If Date/POSIXt, format to YYYY/MM
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
            return(format(x, "%Y/%m"))
        }
        # If character "YYYY-MM", replace - with /
        # Or try appending -01 to parse
        d <- suppressWarnings(as.Date(paste0(x, "-01")))
        if (all(!is.na(d))) {
            return(format(d, "%Y/%m"))
        }
        # Fallback: maybe it's already YYYY/MM
        return(as.character(x))
    }

    if (time_precision == "year") {
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
            return(format(x, "%Y"))
        }
        return(as.character(x))
    }

    as.character(x)
}


#' Write SaTScan File
#'
#' Writes an ss_tbl to disk in SaTScan ASCII format.
#' Handles column ordering, time formatting, and whitespace.
#'
#' @param x An ss_tbl object.
#' @param file Output file path.
#' @param sep Separator (default space).
#' @param na NA string (default "").
#' @param quote Logical (default FALSE).
#' @export
write_satscan <- function(x, file, sep = " ", na = "", quote = FALSE) {
    stopifnot(is_ss_tbl(x))

    # Prepare output data frame
    # 1. Select columns in correct order
    out_cols <- ss_order(x)

    # For cas/ctl/pop, we might want to include covars and other optional cols *after* the required ones?
    # The schema says "optional_roles". If user mapped them, they should be in ss_order?
    # Wait, new_ss_tbl only calculated ss_order for REQUIRED roles + neighbor expansions.
    # We need to append optional mapped roles (time, covars, etc) to out_cols if they aren't there.

    roles <- ss_roles(x)
    all_mapped_cols <- unname(roles)

    # For neighbor/met types, out_cols covers everything (ID + neighbors)
    # For cas/ctl/pop/geo, out_cols currently ONLY has required roles.
    # We should append the rest of the mapped roles to out_cols.

    extra_cols <- setdiff(all_mapped_cols, out_cols)

    # HOWEVER, SaTScan expects specific order for optional columns too?
    # e.g. .geo: ID, X, Y, [Z]. Z must come after.
    # e.g. .cas: ID, Cases, [Time, Age, Sex...]
    # We should look at the 'optional_roles' list in schema to order them correctly?
    # Yes.

    type <- ss_type(x)
    schema <- ss_schema()[[type]]

    opt_roles <- schema$optional_roles
    ordered_extras <- character(0)

    # 1. Add known optional roles in schema order
    for (r in opt_roles) {
        if (r == "covars") {
            # covars is a bit special, usually user maps it to multiple cols?
            # If roles has 'covars', append it?
            # Actually our coercion helpers map 'covars' into spec$covars (vector of names), OR explicitly in roles?
            # as_satscan_* puts it in spec$covars.
            covars_vec <- ss_spec(x)$covars
            if (!is.null(covars_vec)) ordered_extras <- c(ordered_extras, covars_vec)
        } else {
            if (r %in% names(roles)) {
                ordered_extras <- c(ordered_extras, roles[[r]])
            }
        }
    }

    # 2. What about roles NOT in required or optional? (e.g. user just passed extra stuff?)
    # SaTScan (cas) allows extra columns.
    # We should probably respect that? Or only write what is explicitly mapped?
    # Let's write explicitly mapped + spec-defined columns.

    final_cols <- c(out_cols, ordered_extras)

    # Subset data
    # Note: ensure we don't duplicate columns if overlap
    final_cols <- unique(final_cols)
    out_df <- as.data.frame(x)[, final_cols, drop = FALSE]

    # Formatting: Time
    # Apply formatting to any column mapped to 'time', 'start_time', 'end_time'
    # Vectorized: check which columns in out_df correspond to these roles
    time_roles <- c("time", "start_time", "end_time")
    active_time_roles <- intersect(names(roles), time_roles)

    if (length(active_time_roles) > 0) {
        tp <- ss_spec(x)$time_precision
        if (!is.null(tp)) {
            # Apply formatting
            for (tr in active_time_roles) {
                col_name <- roles[[tr]]
                if (col_name %in% names(out_df)) {
                    out_df[[col_name]] <- format_satscan_time(out_df[[col_name]], tp)
                }
            }
        }
    }

    # Write
    utils::write.table(out_df,
        file = file, sep = sep, row.names = FALSE,
        col.names = FALSE, quote = quote, na = na
    )
    invisible(file)
}

# Helper `%||%` is now in utils_ss.R
