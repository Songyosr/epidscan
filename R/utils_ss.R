#' Internal Helper: Validating Input Type
#' @keywords internal
check_ss_input <- function(obj, kind) {
    if (inherits(obj, "ss_tbl")) {
        return(ss_type(obj) == kind)
    }
    if (inherits(obj, "satscan_table")) {
        return(obj$kind == kind)
    }
    FALSE
}

#' Internal Helper: Get Spec Value
#' @keywords internal
get_ss_spec <- function(obj, key) {
    if (inherits(obj, "ss_tbl")) ss_spec(obj)[[key]] else obj$spec[[key]]
}

#' Internal Helper: Get Data Frame
#' @keywords internal
get_ss_data <- function(obj) {
    if (inherits(obj, "ss_tbl")) as.data.frame(obj) else obj$data
}

#' Internal Helper: Write SS File Polymorphically
#' @keywords internal
write_ss_file_wrapper <- function(obj, path) {
    if (inherits(obj, "ss_tbl")) {
        # Use the new writer which handles ordering and time formatting
        write_satscan(obj, path, quote = FALSE)
    } else {
        # Legacy writer (matches previous internal logic)
        utils::write.table(obj$data, path, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    }
}

#' Null Coalesce
#' @name null_coalesce
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b
