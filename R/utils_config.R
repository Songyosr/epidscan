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

#' Get SaTScan Version
#'
#' Attempts to determine the version of the configured SaTScan executable.
#'
#' @return Character string containing the version (e.g. "10.2.5") or NULL if unable to determine.
#' @export
get_satscan_version <- function() {
    ss_path <- get_satscan_path()
    if (is.null(ss_path) || !file.exists(ss_path)) {
        return(NULL)
    }

    # Run "satscan -v"
    # Note: On macOS/Linux this works. On Windows, check if -v is supported or check help.
    tryCatch(
        {
            # Capture output
            output <- suppressWarnings(system2(ss_path, args = "-v", stdout = TRUE, stderr = TRUE))
            # Output format: "SaTScan 10.2.5  (64-bit)"

            # Look for line starting with SaTScan
            version_line <- output[grep("^SaTScan", output)[1]]

            if (!is.na(version_line)) {
                # Extract X.Y (ignore patch version to match template keys)
                # Regex: Match numbers separated by dots
                m <- regexpr("\\d+\\.\\d+", version_line)
                if (m > 0) {
                    return(regmatches(version_line, m))
                }
            }
            return(NULL)
        },
        error = function(e) {
            warning("Failed to run SaTScan to determine version: ", e$message)
            return(NULL)
        }
    )
}
