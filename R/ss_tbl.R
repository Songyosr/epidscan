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
#' `r lifecycle::badge("stable")`
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
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "cas" (Case File).
#'
#' @param data A data frame containing case data.
#' @param loc_id Column name for location ID (character or numeric).
#' @param cases Column name for case counts (numeric).
#' @param time Optional column name for time (Date, numeric, or character).
#' @param covars Optional vector of column names for covariates.
#' @param spec Additional specifications (e.g., time_precision).
#'
#' @return An `ss_tbl` object of type "cas".
#'
#' @examples
#' df <- data.frame(id = c("A", "B"), count = c(10, 5), year = 2020)
#' ss_cas <- as_satscan_case(df, loc_id = "id", cases = "count", time = "year")
#'
#' @export
as_satscan_case <- function(data, loc_id, cases, time = NULL, covars = NULL, spec = list()) {
    roles <- c(loc_id = loc_id, cases = cases)
    if (!is.null(time)) roles <- c(roles, time = time)
    if (!is.null(covars)) spec$covars <- covars
    new_ss_tbl(data, "cas", roles, spec)
}

#' Coerce Data to SaTScan Control Table
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "ctl" (Control File) for Bernoulli models.
#'
#' @param data A data frame containing control data.
#' @param loc_id Column name for location ID.
#' @param controls Column name for control counts.
#' @param time Optional column name for time.
#' @param covars Optional vector of column names for covariates.
#' @param spec Additional specifications.
#'
#' @return An `ss_tbl` object of type "ctl".
#'
#' @export
as_satscan_control <- function(data, loc_id, controls, time = NULL, covars = NULL, spec = list()) {
    roles <- c(loc_id = loc_id, controls = controls)
    if (!is.null(time)) roles <- c(roles, time = time)
    if (!is.null(covars)) spec$covars <- covars
    new_ss_tbl(data, "ctl", roles, spec)
}

#' Coerce Data to SaTScan Population Table
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "pop" (Population File).
#'
#' @param data A data frame containing population data.
#' @param loc_id Column name for location ID.
#' @param time Column name for time (census year/time).
#' @param population Column name for population count.
#' @param covars Optional vector of column names for covariates.
#' @param spec Additional specifications.
#'
#' @return An `ss_tbl` object of type "pop".
#'
#' @export
as_satscan_population <- function(data, loc_id, time, population, covars = NULL, spec = list()) {
    roles <- c(loc_id = loc_id, time = time, population = population)
    if (!is.null(covars)) spec$covars <- covars
    new_ss_tbl(data, "pop", roles, spec)
}

#' Coerce Data to SaTScan Coordinates Table
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "geo" (Coordinates File).
#'
#' @param data A data frame containing coordinate data.
#' @param loc_id Column name for location ID.
#' @param coord1 Column name for the first coordinate (Latitude or Cartesian X).
#' @param coord2 Column name for the second coordinate (Longitude or Cartesian Y).
#' @param z Optional column name for altitude/Z-coordinate.
#' @param spec Additional specifications (e.g., `coord_type = "latlong"` or `"cartesian"`).
#' Defaults to "auto".
#'
#' @return An `ss_tbl` object of type "geo".
#'
#' @export
as_satscan_coordinates <- function(data, loc_id, coord1, coord2, z = NULL, spec = list(coord_type = "auto")) {
    roles <- c(loc_id = loc_id, coord1 = coord1, coord2 = coord2)
    if (!is.null(z)) spec$z <- z
    new_ss_tbl(data, "geo", roles, spec)
}

#' Coerce Data to SaTScan Grid Table
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "grd" (Grid File).
#'
#' @param data A data frame containing grid locations.
#' @param coord1 Column name for the first coordinate.
#' @param coord2 Column name for the second coordinate.
#' @param z Optional column name for Z-coordinate.
#' @param earliest_start Optional column for earliest start time constraint.
#' @param latest_start Optional column for latest start time constraint.
#' @param earliest_end Optional column for earliest end time constraint.
#' @param latest_end Optional column for latest end time constraint.
#' @param spec Additional specifications (e.g. `grid_variant`).
#'
#' @return An `ss_tbl` object of type "grd".
#'
#' @export
as_satscan_grid <- function(data, coord1, coord2, z = NULL,
                            earliest_start = NULL, latest_start = NULL,
                            earliest_end = NULL, latest_end = NULL,
                            spec = list(grid_variant = "basic")) {
    roles <- c(coord1 = coord1, coord2 = coord2)
    if (!is.null(z)) spec$z <- z
    if (!is.null(earliest_start)) roles <- c(roles, earliest_start = earliest_start)
    if (!is.null(latest_start)) roles <- c(roles, latest_start = latest_start)
    if (!is.null(earliest_end)) roles <- c(roles, earliest_end = earliest_end)
    if (!is.null(latest_end)) roles <- c(roles, latest_end = latest_end)
    new_ss_tbl(data, "grd", roles, spec)
}

#' Coerce Data to SaTScan Network Table
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "nwk" (Network File).
#'
#' @param data A data frame containing network edges.
#' @param loc_id Column name for the source location ID.
#' @param neighbor_id Column name for the connected neighbor ID.
#' @param distance Optional column name for distance/weight for the connection.
#' @param spec Additional specifications (e.g. `distance_units`).
#'
#' @return An `ss_tbl` object of type "nwk".
#'
#' @export
as_satscan_network <- function(data, loc_id, neighbor_id, distance = NULL, spec = list(distance_units = NULL)) {
    roles <- c(loc_id = loc_id, neighbor_id = neighbor_id)
    if (!is.null(distance)) roles <- c(roles, distance = distance)
    new_ss_tbl(data, "nwk", roles, spec)
}

#' Coerce Data to SaTScan Neighbors Table
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "nbr" (Neighbors File).
#'
#' @param data A data frame containing neighbor lists.
#' @param loc_id Column name for the location ID.
#' @param neighbor_cols Character vector of column names representing neighbors.
#' @param spec Additional specifications.
#'
#' @return An `ss_tbl` object of type "nbr".
#'
#' @export
as_satscan_neighbors <- function(data, loc_id, neighbor_cols, spec = list()) {
    # neighbor_cols: character vector of column names providing neighbors in order
    roles <- c(loc_id = loc_id)
    spec$neighbor_cols <- neighbor_cols
    # We don't map individual neighbors to separate roles in 'roles' vector to avoid clutter,
    # but we MUST verify they exist.
    missing <- setdiff(neighbor_cols, names(data))
    if (length(missing) > 0) stop("Missing neighbor columns: ", paste(missing, collapse = ", "))

    new_ss_tbl(data, "nbr", roles, spec)
}

#' Coerce Data to SaTScan Meta Locations
#'
#' `r lifecycle::badge("stable")`
#'
#' Creates an `ss_tbl` object of type "met" (Meta Population File).
#'
#' @param data A data frame containing meta-location definitions.
#' @param meta_loc_id Column name for the meta-location ID.
#' @param member_cols Character vector of column names representing member locations.
#' @param spec Additional specifications.
#'
#' @return An `ss_tbl` object of type "met".
#'
#' @export
as_satscan_meta_locations <- function(data, meta_loc_id, member_cols, spec = list()) {
    roles <- c(meta_loc_id = meta_loc_id)
    spec$member_cols <- member_cols
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
