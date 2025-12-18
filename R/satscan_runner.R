# SaTScan Binary Runner
# Handles low-level execution of the SaTScan binary

#' Run SaTScan Binary
#'
#' Executes the SaTScan binary directly using system commands.
#' Replaces the reliance on `rsatscan::satscan()`.
#'
#' @param prm_file Path to the input PRM file.
#' @param ss_path Path to the SaTScan executable.
#' @param verbose Logical. If TRUE, prints output to console.
#' @return Invisible TRUE on success. Stops on failure.
#' @keywords internal
run_satscan_binary <- function(prm_file, ss_path, verbose = FALSE) {
    if (!file.exists(ss_path)) {
        stop("SaTScan binary not found at: ", ss_path)
    }
    if (!file.exists(prm_file)) {
        stop("PRM file not found at: ", prm_file)
    }

    # Normalize paths for shell execution
    ss_path_safe <- shQuote(normalizePath(ss_path))
    prm_file_safe <- shQuote(normalizePath(prm_file))

    # Construct command
    # SaTScan usage: SaTScanBatch <prm_file>
    # Note: On macOS, we might be calling the binary inside the app bundle or a standalone binary.

    cmd <- paste(ss_path_safe, prm_file_safe)

    if (verbose) {
        message("Executing: ", cmd)
    }

    # Execute
    # stdout = "" sends output to R console if verbose=TRUE, otherwise capture it?
    # system2 is generally safer.

    if (verbose) {
        exit_code <- system2(ss_path, args = c(prm_file_safe), stdout = "", stderr = "")
    } else {
        exit_code <- system2(ss_path, args = c(prm_file_safe), stdout = FALSE, stderr = FALSE)
    }

    if (exit_code != 0) {
        stop("SaTScan execution failed with exit code: ", exit_code)
    }

    return(invisible(TRUE))
}
