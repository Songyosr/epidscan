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
        # Stream output to console
        exit_code <- system2(ss_path, args = c(prm_file_safe), stdout = "", stderr = "")
    } else {
        # Capture output silently
        # system2 with stdout=TRUE returns character vector of lines
        # Warnings generated if status != 0
        output_lines <- tryCatch(
            system2(ss_path, args = c(prm_file_safe), stdout = TRUE, stderr = TRUE),
            warning = function(w) {
                # Could be content-less warning about status, or something else.
                # If system2 failed, it usually returns the output anyway, but warns.
                # We can try to get the result from the warning call if possible, or just allow it.
                # simpler: suppress warnings here, we check status attribute.
                suppressWarnings(system2(ss_path, args = c(prm_file_safe), stdout = TRUE, stderr = TRUE))
            }
        )

        # Check exit status
        status_attr <- attr(output_lines, "status")
        if (!is.null(status_attr)) {
            exit_code <- status_attr
        } else {
            exit_code <- 0
        }
    }

    if (exit_code != 0) {
        msg <- paste("SaTScan execution failed with exit code:", exit_code)

        if (!verbose && exists("output_lines") && length(output_lines) > 0) {
            # Add tail of output to message for debugging context
            tail_n <- 20
            tail_lines <- if (length(output_lines) > tail_n) tail(output_lines, tail_n) else output_lines
            formatted_output <- paste(tail_lines, collapse = "\n")
            msg <- paste0(msg, "\n\n--- SaTScan Output (Tail) ---\n", formatted_output, "\n------------------------------")
        }

        stop(msg)
    }

    return(invisible(TRUE))
}
