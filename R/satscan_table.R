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

#' Helper to create satscan_table safely
#' @export
satscan_table <- function(data, kind, spec = list()) {
    validate_satscan_table(new_satscan_table(data, kind, spec))
}

#' Print method for satscan_table
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
