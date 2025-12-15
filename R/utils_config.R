#' Set SatScan Executable Path
#'
#' Sets the path to the SatScan executable for the current session.
#'
#' @param path Character. Absolute path to SatScan executable.
#'
#' @export
#' @examples
#' \dontrun{
#' set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")
#' }
set_satscan_path <- function(path) {
    if (!file.exists(path)) {
        warning("Path does not exist: ", path)
    }
    options(epidscan.satscan_path = path)
    message("SatScan path set to: ", path)
}

#' Get SatScan Executable Path
#'
#' Retrieves the configured SatScan path.
#'
#' @return Character path or NULL if not set.
#' @export
get_satscan_path <- function() {
    getOption("epidscan.satscan_path")
}
