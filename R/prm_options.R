# =============================================================================
# PRM OBJECT SYSTEM
# =============================================================================
# Provides an S3 class 'satscan_prm' for handling SaTScan parameters explicitly.

#' Create SaTScan Parameter Object
#'
#' Constructor for a `satscan_prm` object. Defaults are loaded based on the
#' configured SaTScan version (if available), and user overrides are applied.
#'
#' @param ... Named parameters to override (e.g., `AnalysisType = 3`).
#'   These must match standard SaTScan parameter names (case-sensitive).
#'   Common parameters include:
#'   \itemize{
#'     \item \code{AnalysisType}:
#'       \itemize{
#'         \item 1 = Purely Spatial
#'         \item 2 = Purely Temporal
#'         \item 3 = Retrospective Space-Time
#'         \item 4 = Prospective Space-Time
#'         \item 5 = Spatial Variation in Temporal Trends
#'         \item 6 = Prospective Purely Temporal
#'         \item 7 = Seasonal Temporal
#'       }
#'     \item \code{ModelType}:
#'       \itemize{
#'         \item 0 = Discrete Poisson
#'         \item 1 = Bernoulli
#'         \item 2 = Space-Time Permutation
#'         \item 3 = Ordinal
#'         \item 4 = Exponential
#'         \item 5 = Normal
#'         \item 6 = Continuous Poisson
#'         \item 7 = Multinomial
#'         \item 8 = Rank
#'         \item 9 = Uniform Time
#'         \item 10 = Batched
#'       }
#'     \item \code{ScanAreas}:
#'       \itemize{
#'         \item 1 = High Rates
#'         \item 2 = Low Rates
#'         \item 3 = Both High and Low Rates
#'       }
#'     \item \code{TimeAggregationUnits} / \code{PrecisionCaseTimes}:
#'       \itemize{
#'         \item 1 = Year
#'         \item 2 = Month
#'         \item 3 = Day
#'         \item 4 = Generic
#'       }
#'     \item \code{MonteCarloReps}: Number of replications (e.g., 999)
#'   }
#' @param version SaTScan version string (e.g., "10.2"). If NULL (default),
#'   attempts to auto-detect from the configured SaTScan executable.
#' @param base_prm Optional existing `satscan_prm` object to extend.
#' @return An object of class `satscan_prm` (which is a subclass of `prm_list`).
#'
#' @export
prm_options <- function(..., version = NULL, base_prm = NULL) {
    # 1. Determine Baseline
    if (!is.null(base_prm)) {
        if (!inherits(base_prm, "prm_list")) {
            stop("base_prm must be a prm_list/satscan_prm object", call. = FALSE)
        }
        prm <- base_prm
    } else {
        # Auto-detect version if needed
        if (is.null(version)) {
            version <- get_satscan_version()
        }

        # Load defaults
        # Note: prm_defaults handles fallback if version is NULL or invalid
        prm <- prm_defaults(version)
    }

    # 2. Apply Overrides
    user_opts <- list(...)
    if (length(user_opts) > 0) {
        # Split into existing vs new parameters
        existing_keys <- intersect(names(user_opts), names(prm))
        new_keys <- setdiff(names(user_opts), names(prm))

        # Update existing
        if (length(existing_keys) > 0) {
            # Fix: subset user_opts by name to ensure named list
            updates <- user_opts[existing_keys]
            prm <- do.call(prm_set, c(list(prm, .strict = FALSE), updates))
        }

        # Add new
        if (length(new_keys) > 0) {
            for (k in new_keys) {
                # Add to [Input] section by default? Or [UserDefined]?
                # Let's use [Input] as a safe default or try to guess.
                # Actually, prm_add puts it at the end if section not found.
                # Let's put in [UserOverrides]
                prm <- prm_add(prm, k, user_opts[[k]], section = "UserOverrides")
            }
        }
    }

    # 3. Upgrade Class
    if (!inherits(prm, "satscan_prm")) {
        class(prm) <- c("satscan_prm", class(prm))
    }

    prm
}

#' Print SaTScan Parameters
#'
#' @param x A `satscan_prm` object.
#' @param ... Unused.
#' @export
print.satscan_prm <- function(x, ...) {
    # Use the summary helper to show a "Pre-flight" view
    s <- prm_summarize(x)
    print_prm_summary(s)

    cat(sprintf("\n(plus %d other parameters. Use as.list() to view all)\n", length(x) - 7))
    invisible(x)
}

#' Summary of SaTScan Parameters
#'
#' @param object A `satscan_prm` object.
#' @param ... Unused.
#' @export
summary.satscan_prm <- function(object, ...) {
    prm_summarize(object)
}
