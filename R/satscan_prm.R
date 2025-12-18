# PRM File Utilities
# Functions for reading and managing SaTScan parameter files

#' Read a SaTScan PRM File
#'
#' Parses a SaTScan `.prm` parameter file into a named R list.
#' This allows you to inspect, modify, or use existing SaTScan configurations.
#'
#' @param path Path to a `.prm` file.
#' @return A named list of SaTScan parameters (e.g., `list(AnalysisType = "3", ...)`).
#'   Values are returned as character strings; numeric conversion is left to the caller.
#' @examples
#' \dontrun{
#' prm <- read_prm("path/to/analysis.prm")
#' prm$AnalysisType
#' # [1] "3"
#' }
#' @export
read_prm <- function(path) {
    if (!file.exists(path)) {
        stop("PRM file not found: ", path)
    }

    lines <- readLines(path, warn = FALSE)

    # Keep only true "key=value" lines:
    # - ignore empty/whitespace-only lines
    # - ignore section headers like [Input]
    # - ignore comment lines that start with ';' (allow leading whitespace)
    # - require at least one '=' somewhere after a non-empty key
    lines_trim <- trimws(lines)
    is_param <- nzchar(lines_trim) &
        !grepl("^\\s*\\[", lines) &
        !grepl("^\\s*;", lines) &
        grepl("=", lines, fixed = TRUE)

    param_lines <- lines[is_param]

    # Vectorized parse on the FIRST '=' only
    x <- trimws(param_lines)
    pos <- regexpr("=", x, fixed = TRUE)
    has_eq <- pos > 0
    x <- x[has_eq]
    pos <- pos[has_eq]

    key <- trimws(substr(x, 1, pos - 1))
    val <- trimws(substr(x, pos + 1, nchar(x)))

    # drop empty keys (defensive)
    ok <- nzchar(key)
    key <- key[ok]
    val <- val[ok]

    # Named list; duplicates overwrite earlier ones ("last wins"), matching your loop behavior
    as.list(setNames(val, key))
}


#' Set SaTScan Options with Hierarchical Override
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' A convenience wrapper around \code{rsatscan::ss.options()} that applies
#' parameters in a controlled hierarchy: base template first, then user overrides.
#'
#' @details
#' This function is deprecated. Use the new \code{prm_*} system instead:
#' \itemize{
#'   \item \code{\link{prm_defaults}()} to load default parameters
#'   \item \code{\link{prm_set}()} to modify parameters
#'   \item \code{\link{prm_write}()} to write PRM files
#' }
#'
#' @param ... Named SaTScan parameters to set (e.g., \code{AnalysisType = 3}).
#' @param .base Optional named list of base parameters (e.g., from \code{read_prm()}).
#'   Applied before \code{...}.
#' @param .reset If TRUE (default), resets \code{ss.options()} to defaults before applying.
#' @return Invisibly returns the final parameter vector from \code{ss.options()}.
#' @examples
#' \dontrun{
#' # DEPRECATED - use prm_* functions instead:
#' prm <- prm_defaults()
#' prm <- prm_set(prm, AnalysisType = 1, MonteCarloReps = 999)
#' }
#' @export
set_satscan_opts <- function(..., .base = NULL, .reset = TRUE) {
    .Deprecated("prm_set", msg = "set_satscan_opts() is deprecated. Use prm_defaults() + prm_set() instead.")
    if (.reset) {
        rsatscan::ss.options(reset = TRUE)
    }

    # Apply base template first

    if (!is.null(.base)) {
        if (!is.list(.base)) stop(".base must be a named list")
        rsatscan::ss.options(.base)
    }

    # Apply user overrides
    user_opts <- list(...)
    if (length(user_opts) > 0) {
        rsatscan::ss.options(user_opts)
    }

    invisible(rsatscan::ss.options())
}

#' Infer Dates from Data
#'
#' Helper to infer StartDate and EndDate from the case data if missing from options.
#'
#' @param current_opts List of current SaTScan options
#' @param cas_data Case data frame (must have a 'time' column)
#' @param time_precision_char Character string: "day", "month", or "year"
#' @param verbose Logical, print messages
#' @return Named list of inferred dates (StartDate, EndDate) or NULL if nothing inferred.
#' @keywords internal
infer_dates_from_data <- function(current_opts, cas_data, time_precision_char, verbose = FALSE) {
    # Check if dates are already present
    # Use [ ] to be safe if named vector
    val_start <- current_opts["StartDate"]
    val_end <- current_opts["EndDate"]

    need_start <- is.na(val_start) || is.null(val_start) || val_start == ""
    need_end <- is.na(val_end) || is.null(val_end) || val_end == ""

    if (!need_start && !need_end) {
        return(NULL)
    }

    if (is.null(cas_data) || is.null(cas_data$time)) {
        return(NULL)
    }

    # Parse based on precision
    d_vals <- NULL
    if (time_precision_char == "day") {
        d_vals <- as.Date(cas_data$time, format = "%Y/%m/%d")
    } else if (time_precision_char == "month") {
        d_vals <- as.Date(paste0(cas_data$time, "/01"), format = "%Y/%m/%d")
    } else if (time_precision_char == "year") {
        d_vals <- as.Date(paste0(cas_data$time, "/01/01"), format = "%Y/%m/%d")
    }

    if (is.null(d_vals) || all(is.na(d_vals))) {
        return(NULL)
    }

    min_d <- min(d_vals, na.rm = TRUE)
    max_d <- max(d_vals, na.rm = TRUE)

    inferred <- list()
    if (need_start) {
        if (time_precision_char == "year") {
            inferred$StartDate <- format(min_d, "%Y/01/01")
        } else if (time_precision_char == "month") {
            inferred$StartDate <- format(min_d, "%Y/%m/01")
        } else {
            inferred$StartDate <- format(min_d, "%Y/%m/%d")
        }
    }
    if (need_end) {
        if (time_precision_char == "year") {
            inferred$EndDate <- format(max_d, "%Y/12/31")
        } else if (time_precision_char == "month") {
            # End of month
            d_next <- seq(max_d, by = "month", length.out = 2)[2]
            inferred$EndDate <- format(d_next - 1, "%Y/%m/%d")
        } else {
            inferred$EndDate <- format(max_d, "%Y/%m/%d")
        }
    }

    if (length(inferred) > 0) {
        if (verbose) message("Inferring missing dates from data: ", paste(names(inferred), collapse = ", "))
    }

    inferred
}

#' Write a Safe SaTScan PRM File
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Writes SaTScan parameters to a file, ensuring that required section headers
#' (e.g. `[Input]`, `[Analysis]`) are present. This guards against issues where
#' internal defaults might be missing headers.
#'
#' @details
#' This function is deprecated. Use \code{\link{prm_write}()} instead, which
#' uses skeleton injection for more robust PRM file generation.
#'
#' @param params Character vector of parameters (from ss.options())
#' @param path File path to write to
#' @export
write_prm_safe <- function(params, path) {
    .Deprecated("prm_write", msg = "write_prm_safe() is deprecated. Use prm_write() instead.")
    if (length(params) == 0) {
        writeLines(character(0), path)
        return(invisible())
    }

    out <- params

    # 1. Ensure headers exist (simple injection if missing)
    ensure_header <- function(lines, header, trigger_pattern) {
        if (any(grepl(paste0("^\\Q", header, "\\E"), lines))) {
            return(lines)
        }
        idx <- grep(trigger_pattern, lines)
        if (length(idx) > 0) append(lines, header, after = idx[1] - 1) else lines
    }

    out <- ensure_header(out, "[Input]", "^CaseFile=")
    out <- ensure_header(out, "[Analysis]", "^AnalysisType=")
    out <- ensure_header(out, "[Output]", "^ResultsFile=")
    out <- ensure_header(out, "[Power Simulations]", "^SimulatedDataMethodType=")
    out <- ensure_header(out, "[Run Options]", "^NumberParallelProcesses=")
    out <- ensure_header(out, "[System]", "^Version=")

    # 2. Critical: Ensure [Input] is the VERY FIRST line.
    # We must MOVE it to the top, not just splice from it (which deletes earlier params like AnalysisType!)

    input_idx <- grep("^\\[Input\\]", out)

    if (length(input_idx) > 0) {
        # Remove existing [Input] lines
        out <- out[-input_idx]
        # Prepend one [Input]
        out <- c("[Input]", out)
    } else {
        # If missing, just prepend
        out <- c("[Input]", out)
    }

    # 3. Clean up potential double headers if injection was sloppy?
    # No, the removal above handles it.

    writeLines(out, path)
}
