#' specific constructor for internal use
#' @keywords internal
new_satscan_table <- function(data, kind, spec = list()) {
    structure(
        list(
            data = data,
            kind = kind,
            spec = spec
        ),
        class = "satscan_table"
    )
}

#' Validator for satscan_table
#' @keywords internal
validate_satscan_table <- function(x) {
    if (!inherits(x, "satscan_table")) {
        stop("Object is not of class 'satscan_table'")
    }

    if (!is.data.frame(x$data)) {
        stop("satscan_table 'data' must be a data.frame")
    }

    valid_kinds <- c("cas", "pop", "geo", "ctl", "grd", "adj", "nbr", "mlc", "maxcircle")
    if (!x$kind %in% valid_kinds) {
        stop(sprintf("Invalid kind '%s'. Must be one of: %s", x$kind, paste(valid_kinds, collapse = ", ")))
    }

    # Basic checking of required columns by kind could go here
    # But that is mostly handled by prep_* functions

    x
}

#' Create a SaTScan Table Object
#'
#' Constructs a validated `satscan_table` object from a data frame.
#' These objects are used as inputs to \code{\link{satscanr}}.
#'
#' @details
#' In most cases, you should use the helper functions \code{\link{prep_cas}},
#' \code{\link{prep_pop}}, \code{\link{prep_geo}}, \code{\link{prep_ctl}}, or
#' \code{\link{prep_grd}} to create satscan_table objects, as they handle
#' data formatting and validation.
#'
#' This function is primarily for advanced users who need to create
#' custom table types or bypass the prep_* helpers.
#'
#' @param data A data.frame containing the table data.
#' @param kind Character. Type of SaTScan file:
#'   \itemize{
#'     \item \code{"cas"}: Case file
#'     \item \code{"pop"}: Population file
#'     \item \code{"geo"}: Coordinates/geometry file
#'     \item \code{"ctl"}: Control file (Bernoulli model)
#'     \item \code{"grd"}: Grid file (custom scan centers)
#'   }
#' @param spec List of metadata about the table (e.g., time_precision, coord_type).
#'
#' @return A validated `satscan_table` object with components:
#'   \itemize{
#'     \item \code{data}: The input data.frame
#'     \item \code{kind}: The table type
#'     \item \code{spec}: Metadata list
#'   }
#'
#' @examples
#' \dontrun{
#' # Create a simple case table manually
#' df <- data.frame(
#'     loc_id = c("A", "B"),
#'     cases = c(5, 10)
#' )
#' cas_table <- satscan_table(df, kind = "cas", spec = list(time_precision = "generic"))
#' print(cas_table)
#'
#' # Preferred: use prep_cas() instead
#' cas_table2 <- prep_cas(df, loc_id = loc_id, cases = cases, style = "aggregated")
#' }
#'
#' @seealso \code{\link{prep_cas}}, \code{\link{prep_geo}}, \code{\link{satscanr}}
#' @export
satscan_table <- function(data, kind, spec = list()) {
    validate_satscan_table(new_satscan_table(data, kind, spec))
}

#' Print Method for satscan_table
#'
#' Displays a summary of a satscan_table object including its kind,
#' metadata, and a preview of the data.
#'
#' @param x A \code{satscan_table} object.
#' @param ... Additional arguments passed to \code{\link{print}} for the data preview.
#'
#' @return Invisibly returns the input object.
#' @export
print.satscan_table <- function(x, ...) {
    cat(sprintf("<SaTScan Table: %s>\n", x$kind))

    # Print spec details
    if (length(x$spec) > 0) {
        cat("Metadata:\n")
        for (n in names(x$spec)) {
            val <- x$spec[[n]]
            # Format value nicely
            val_str <- if (length(val) > 5) paste0(paste(head(val, 5), collapse = ", "), "...") else paste(val, collapse = ", ")
            cat(sprintf("  %s: %s\n", n, val_str))
        }
    }

    cat(sprintf("Data: %d rows, %d columns\n", nrow(x$data), ncol(x$data)))
    print(head(x$data, ...))
    invisible(x)
}
