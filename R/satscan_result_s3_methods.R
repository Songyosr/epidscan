# =============================================================================
# ENHANCED S3 METHODS FOR satscan_result
# =============================================================================
# This file provides a comprehensive set of S3 methods for satscan_result objects,
# following modern R conventions (especially broom-style tidiers).

#' @importFrom generics tidy glance augment
#' @importFrom utils head
#' @importFrom dplyr left_join
NULL

# =============================================================================
# 0. BASE METHODS
# =============================================================================

#' Print SaTScan Results
#'
#' @description
#' By default, prints the raw SaTScan text output if available.
#' Use `summary(x)` for a structured analytical summary.
#'
#' @param x A `satscan_result` object.
#' @param ... Additional arguments (currently unused).
#' @return Invisibly returns `x`.
#' @export
#' @method print satscan_result
print.satscan_result <- function(x, ...) {
    if (!is.null(x$main)) {
        cat(x$main)
        cat("\n")
    } else {
        # Fallback to summary if raw text is not available
        print(summary(x, ...))
    }
    invisible(x)
}


# =============================================================================
# 1. ENHANCED summary() METHOD
# =============================================================================

#' Enhanced Summary for SaTScan Results
#'
#' @description
#' Creates a detailed summary object containing cluster statistics, location
#' summaries, and analysis metadata. Returns a structured object of class
#' `summary.satscan_result` with a custom print method.
#'
#' @param object A `satscan_result` object from `satscanr()`.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `summary.satscan_result` containing:
#' \describe{
#'   \item{n_clusters}{Total number of detected clusters}
#'   \item{n_significant}{Number of clusters with p < 0.05}
#'   \item{n_locations}{Total number of locations analyzed}
#'   \item{n_in_clusters}{Number of locations belonging to clusters}
#'   \item{cluster_stats}{Summary statistics for clusters (if available)}
#'   \item{location_stats}{Summary statistics for locations}
#'   \item{most_likely}{Details of the most likely cluster (if exists)}
#'   \item{rr_range}{Range of relative risks (if available)}
#'   \item{analysis_params}{Analysis parameters (if stored)}
#' }
#'
#' @examples
#' \dontrun{
#' result <- satscanr(cas, geo, pop, ...)
#' summary(result)
#' }
#'
#' @export
#' @method summary satscan_result
summary.satscan_result <- function(object, ...) {
    # =========================================================================
    # BASIC COUNTS
    # =========================================================================
    n_clusters <- if (!is.null(object$clusters)) nrow(object$clusters) else 0
    n_locs <- if (!is.null(object$locations)) nrow(object$locations) else 0

    n_sig <- 0
    if (n_clusters > 0 && "P_VALUE" %in% names(object$clusters)) {
        n_sig <- sum(object$clusters$P_VALUE < 0.05, na.rm = TRUE)
    }

    n_in_cluster <- 0
    if (n_locs > 0 && "CLUSTER" %in% names(object$locations)) {
        n_in_cluster <- sum(!is.na(object$locations$CLUSTER))
    }

    # =========================================================================
    # CLUSTER STATISTICS
    # =========================================================================
    cluster_stats <- NULL
    if (n_clusters > 0) {
        cluster_stats <- compute_cluster_summary(object$clusters)
    }

    # =========================================================================
    # LOCATION STATISTICS
    # =========================================================================
    location_stats <- NULL
    if (n_locs > 0) {
        location_stats <- compute_location_summary(object$locations)
    }

    # =========================================================================
    # MOST LIKELY CLUSTER
    # =========================================================================
    most_likely <- NULL
    if (n_sig > 0) {
        most_likely <- object$clusters[1, ] # Already sorted by p-value
    }

    # =========================================================================
    # RELATIVE RISK RANGE
    # =========================================================================
    rr_range <- NULL
    if (!is.null(object$locations) && "REL_RISK" %in% names(object$locations)) {
        rr_range <- range(object$locations$REL_RISK, na.rm = TRUE)
    } else if (!is.null(object$clusters) && "CLU_RR" %in% names(object$clusters)) {
        rr_range <- range(object$clusters$CLU_RR, na.rm = TRUE)
    }

    # =========================================================================
    # ANALYSIS PARAMETERS (if stored)
    # =========================================================================
    analysis_params <- attr(object, "parameters")

    # =========================================================================
    # BUILD SUMMARY OBJECT
    # =========================================================================
    structure(
        list(
            n_clusters = n_clusters,
            n_significant = n_sig,
            n_locations = n_locs,
            n_in_clusters = n_in_cluster,
            cluster_stats = cluster_stats,
            location_stats = location_stats,
            most_likely = most_likely,
            rr_range = rr_range,
            analysis_params = analysis_params,
            summary_info = get_satscan_metadata(object),
            raw_text = object$main
        ),
        class = "summary.satscan_result"
    )
}


#' Print Method for Summary of SaTScan Results
#'
#' @param x A `summary.satscan_result` object.
#' @param raw Logical. If `TRUE` and available, print the raw SaTScan text output.
#' @param ... Additional arguments (currently unused).
#' @return Invisibly returns `x`.
#' @export
#' @method print summary.satscan_result
print.summary.satscan_result <- function(x, raw = FALSE, ...) {
    if (raw && !is.null(x$raw_text)) {
        cat("Raw SaTScan Output:\n")
        cat("==================\n")
        cat(x$raw_text)
        cat("\n")
        return(invisible(x))
    }

    cat("SaTScan Results Summary\n")
    cat("=======================\n\n")

    # Overview
    cat("Overview:\n")
    cat(sprintf(
        "  Clusters detected:  %d (%d significant at p < 0.05)\n",
        x$n_clusters, x$n_significant
    ))
    cat(sprintf(
        "  Locations analyzed: %d (%d in clusters)\n",
        x$n_locations, x$n_in_clusters
    ))

    # Relative risk range
    if (!is.null(x$rr_range)) {
        cat(sprintf(
            "  Relative risk:      %.2f - %.2f\n",
            x$rr_range[1], x$rr_range[2]
        ))
    }
    cat("\n")

    # Most likely cluster
    if (!is.null(x$most_likely)) {
        cat("Most Likely Cluster:\n")
        cat(sprintf("  Cluster ID:         %s\n", x$most_likely$CLUSTER))
        cat(sprintf("  P-value:            %.6f\n", x$most_likely$P_VALUE))
        if ("CLU_RR" %in% names(x$most_likely)) {
            cat(sprintf("  Relative Risk:      %.2f\n", x$most_likely$CLU_RR))
        }
        if ("OBSERVED" %in% names(x$most_likely)) {
            cat(sprintf("  Observed:           %d\n", x$most_likely$OBSERVED))
        }
        if ("EXPECTED" %in% names(x$most_likely)) {
            cat(sprintf("  Expected:           %.1f\n", x$most_likely$EXPECTED))
        }
        cat("\n")
    }

    # Cluster statistics
    if (!is.null(x$cluster_stats)) {
        cat("Cluster Statistics:\n")
        print(x$cluster_stats)
        cat("\n")
    }

    # Analysis parameters
    if (!is.null(x$summary_info)) {
        print_prm_summary(x$summary_info)
    } else if (!is.null(x$analysis_params)) {
        # Fallback for old objects
        cat("Analysis Parameters (Legacy):\n")
        if (!is.null(x$analysis_params$AnalysisType)) {
            cat(sprintf("  Analysis Type:      %s\n", x$analysis_params$AnalysisType))
        }
        if (!is.null(x$analysis_params$ModelType)) {
            cat(sprintf("  Model Type:         %s\n", x$analysis_params$ModelType))
        }
        if (!is.null(x$analysis_params$MonteCarloReps)) {
            cat(sprintf("  Monte Carlo Reps:   %s\n", x$analysis_params$MonteCarloReps))
        }
    }

    if (!is.null(x$raw_text)) {
        cat("\n(Raw SaTScan text available. Use print(result, raw = TRUE) to view)\n")
    }

    invisible(x)
}


# =============================================================================
# 2. BROOM-STYLE TIDIERS
# =============================================================================

#' Tidy SaTScan Results into a Data Frame
#'
#' @description
#' Returns a tidy data frame with one row per cluster, containing cluster-level
#' statistics. This follows the broom package convention for model tidying.
#'
#' @param x A `satscan_result` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame (tibble if available) with cluster-level statistics.
#'   Common columns include:
#'   \describe{
#'     \item{cluster}{Cluster identifier}
#'     \item{p_value}{Statistical significance}
#'     \item{relative_risk}{Relative risk within cluster}
#'     \item{observed}{Observed number of cases}
#'     \item{expected}{Expected number of cases}
#'     \item{obs_exp_ratio}{Observed/Expected ratio}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- satscanr(cas, geo, pop, ...)
#' tidy(result)
#' }
#'
#' @export
#' @method tidy satscan_result
tidy.satscan_result <- function(x, ...) {
    if (is.null(x$clusters)) {
        # Return empty tibble with correct structure
        return(make_tidy_df(data.frame(
            cluster = integer(),
            p_value = numeric(),
            stringsAsFactors = FALSE
        )))
    }

    df <- x$clusters

    # Base structure with proper types
    tidy_df <- data.frame(
        cluster = as.integer(df$CLUSTER),
        stringsAsFactors = FALSE
    )

    # 1. Location & ID
    if ("LOC_ID" %in% names(df)) tidy_df$center_id <- as.character(df$LOC_ID)

    # Intuitive coordinate handling (preserve source names but lowercase)
    if ("LATITUDE" %in% names(df)) tidy_df$latitude <- as.numeric(df$LATITUDE)
    if ("LONGITUDE" %in% names(df)) tidy_df$longitude <- as.numeric(df$LONGITUDE)
    if ("X" %in% names(df)) tidy_df$x <- as.numeric(df$X)
    if ("Y" %in% names(df)) tidy_df$y <- as.numeric(df$Y)
    if ("Z" %in% names(df)) tidy_df$z <- as.numeric(df$Z)

    # 2. Statistics
    if ("P_VALUE" %in% names(df)) tidy_df$p_value <- as.numeric(df$P_VALUE)

    # Relative Risk (detect multiple variations)
    if ("CLU_RR" %in% names(df)) {
        tidy_df$relative_risk <- as.numeric(df$CLU_RR)
    } else if ("RELATIVE_RISK" %in% names(df)) tidy_df$relative_risk <- as.numeric(df$RELATIVE_RISK)

    if ("LLR" %in% names(df)) tidy_df$llr <- as.numeric(df$LLR)

    # Observed/Expected
    obs_col <- if ("OBSERVED" %in% names(df)) "OBSERVED" else if ("CASES" %in% names(df)) "CASES" else NULL
    if (!is.null(obs_col)) tidy_df$observed <- as.numeric(df[[obs_col]])

    if ("EXPECTED" %in% names(df)) tidy_df$expected <- as.numeric(df$EXPECTED)

    if (!is.null(obs_col) && "EXPECTED" %in% names(df)) {
        tidy_df$obs_exp_ratio <- as.numeric(df[[obs_col]] / df$EXPECTED)
    } else if ("OBS_EXP" %in% names(df)) {
        tidy_df$obs_exp_ratio <- as.numeric(df$OBS_EXP)
    }

    # 3. Geography & Population
    if ("RADIUS" %in% names(df)) tidy_df$radius_km <- as.numeric(df$RADIUS)
    if ("POPULATION" %in% names(df)) tidy_df$population <- as.numeric(df$POPULATION)
    if ("GINI" %in% names(df)) tidy_df$gini_contribution <- as.numeric(df$GINI)

    # 4. Temporal
    if ("START_DATE" %in% names(df)) {
        tidy_df$start_date <- as.Date(df$START_DATE, format = "%Y/%m/%d")
    }
    if ("END_DATE" %in% names(df)) {
        tidy_df$end_date <- as.Date(df$END_DATE, format = "%Y/%m/%d")
    }

    # 5. Additional fields
    if ("TEST_STATISTIC" %in% names(df)) {
        tidy_df$test_statistic <- as.numeric(df$TEST_STATISTIC)
    }

    make_tidy_df(tidy_df)
}


#' Glance at SaTScan Results
#'
#' @description
#' Returns a single-row summary of the entire analysis, following the broom
#' package convention. Useful for comparing multiple analyses.
#'
#' @param x A `satscan_result` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A one-row data frame with analysis-level statistics:
#'   \describe{
#'     \item{n_clusters}{Number of clusters detected}
#'     \item{n_significant}{Number of significant clusters (p < 0.05)}
#'     \item{n_locations}{Total locations analyzed}
#'     \item{n_in_clusters}{Locations within detected clusters}
#'     \item{min_p_value}{Minimum (best) p-value}
#'     \item{max_relative_risk}{Maximum relative risk}
#'     \item{prop_in_clusters}{Proportion of locations in clusters}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- satscanr(cas, geo, pop, ...)
#' glance(result)
#' }
#'
#' @export
#' @method glance satscan_result
glance.satscan_result <- function(x, ...) {
    n_clusters <- if (!is.null(x$clusters)) nrow(x$clusters) else 0
    n_locs <- if (!is.null(x$locations)) nrow(x$locations) else 0

    n_sig <- 0
    if (n_clusters > 0 && "P_VALUE" %in% names(x$clusters)) {
        n_sig <- sum(x$clusters$P_VALUE < 0.05, na.rm = TRUE)
    }

    n_in_cluster <- 0
    if (n_locs > 0 && "CLUSTER" %in% names(x$locations)) {
        n_in_cluster <- sum(!is.na(x$locations$CLUSTER))
    }

    min_p <- as.numeric(NA)
    if (n_clusters > 0 && "P_VALUE" %in% names(x$clusters)) {
        min_p <- min(x$clusters$P_VALUE, na.rm = TRUE)
    }

    max_rr <- as.numeric(NA)
    if (n_clusters > 0) {
        if ("CLU_RR" %in% names(x$clusters)) {
            max_rr <- max(x$clusters$CLU_RR, na.rm = TRUE)
        } else if ("RELATIVE_RISK" %in% names(x$clusters)) {
            max_rr <- max(x$clusters$RELATIVE_RISK, na.rm = TRUE)
        }
    }

    max_llr <- as.numeric(NA)
    if (n_clusters > 0 && "LLR" %in% names(x$clusters)) {
        max_llr <- max(x$clusters$LLR, na.rm = TRUE)
    }

    prop_in_clusters <- if (n_locs > 0) n_in_cluster / n_locs else as.numeric(NA)

    # 1. Metadata from parameters
    meta <- get_satscan_metadata(x)

    # 2. Global statistics (if available via summaries)
    total_obs <- if (n_locs > 0 && "OBSERVED" %in% names(x$locations)) {
        sum(x$locations$OBSERVED, na.rm = TRUE)
    } else {
        as.numeric(NA)
    }

    total_pop <- if (n_locs > 0 && "POPULATION" %in% names(x$locations)) {
        sum(x$locations$POPULATION, na.rm = TRUE)
    } else {
        as.numeric(NA)
    }

    df <- data.frame(
        n_clusters = as.integer(n_clusters),
        n_significant = as.integer(n_sig),
        n_locations = as.integer(n_locs),
        n_in_clusters = as.integer(n_in_cluster),
        min_p_value = min_p,
        max_relative_risk = max_rr,
        max_llr = max_llr,
        prop_in_clusters = prop_in_clusters,
        total_observed = total_obs,
        total_population = total_pop,
        model = meta$model %||% as.character(NA),
        analysis_type = meta$analysis_type %||% as.character(NA),
        monte_carlo_reps = meta$monte_carlo %||% as.integer(NA),
        stringsAsFactors = FALSE
    )

    make_tidy_df(df)
}


#' Augment SaTScan Results with Location-Level Data
#'
#' @description
#' Adds cluster assignments and statistics to the original location data,
#' following the broom package convention.
#'
#' @param x A `satscan_result` object.
#' @param data Optional original data to augment. If NULL, uses x$locations.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with original location data plus:
#'   \describe{
#'     \item{.cluster}{Cluster assignment (NA if not in cluster)}
#'     \item{.in_cluster}{Logical: is location in a cluster?}
#'     \item{.relative_risk}{Location-specific relative risk}
#'     \item{.cluster_p_value}{P-value of assigned cluster}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- satscanr(cas, geo, pop, ...)
#' augment(result)
#'
#' # Or augment original data
#' augment(result, data = original_data)
#' }
#'
#' @export
#' @method augment satscan_result
augment.satscan_result <- function(x, data = NULL, ...) {
    if (is.null(data)) {
        data <- x$locations
    }

    if (is.null(data)) {
        stop("No data available to augment", call. = FALSE)
    }

    # Start with copy of data
    aug_data <- data

    # Add cluster assignment
    if ("CLUSTER" %in% names(aug_data)) {
        aug_data$.cluster <- aug_data$CLUSTER
        aug_data$.in_cluster <- !is.na(aug_data$CLUSTER)
    } else {
        aug_data$.cluster <- NA
        aug_data$.in_cluster <- FALSE
    }

    # Add relative risk
    if ("REL_RISK" %in% names(aug_data)) {
        aug_data$.relative_risk <- aug_data$REL_RISK
    } else {
        aug_data$.relative_risk <- NA
    }

    # Add cluster-level p-value by joining
    if (!is.null(x$clusters) && "CLUSTER" %in% names(aug_data)) {
        cluster_pvals <- x$clusters[, c("CLUSTER", "P_VALUE"), drop = FALSE]
        names(cluster_pvals)[2] <- ".cluster_p_value"

        # Ensure types match for join
        aug_data$CLUSTER <- as.character(aug_data$CLUSTER)
        cluster_pvals$CLUSTER <- as.character(cluster_pvals$CLUSTER)

        aug_data <- dplyr::left_join(aug_data, cluster_pvals, by = "CLUSTER")
    } else {
        aug_data$.cluster_p_value <- NA
    }

    make_tidy_df(aug_data)
}


# =============================================================================
# 3. SUBSETTING METHODS
# =============================================================================

#' Extract Specific Clusters
#'
#' @description
#' Subset a `satscan_result` object to include only specified clusters.
#'
#' @param x A `satscan_result` object.
#' @param i Cluster IDs to extract (numeric or character vector).
#' @param ... Additional arguments (currently unused).
#'
#' @return A new `satscan_result` object containing only the specified clusters
#'   and their member locations.
#'
#' @examples
#' \dontrun{
#' result <- satscanr(cas, geo, pop, ...)
#'
#' # Extract just cluster 1
#' cluster1 <- result[1]
#'
#' # Extract clusters 1 and 3
#' subset <- result[c(1, 3)]
#'
#' # Extract significant clusters
#' sig_clusters <- result[result$clusters$P_VALUE < 0.05]
#' }
#'
#' @export
`[.satscan_result` <- function(x, i, ...) {
    if (is.null(x$clusters)) {
        warning("No clusters to subset", call. = FALSE)
        return(x)
    }

    # Handle logical indexing
    if (is.logical(i)) {
        if (length(i) != nrow(x$clusters)) {
            stop("Logical index must match number of clusters", call. = FALSE)
        }
        cluster_ids <- x$clusters$CLUSTER[i]
    } else {
        cluster_ids <- i
    }

    # Subset clusters
    new_clusters <- x$clusters[x$clusters$CLUSTER %in% cluster_ids, , drop = FALSE]

    # Subset locations
    new_locations <- x$locations
    if (!is.null(new_locations) && "CLUSTER" %in% names(new_locations)) {
        # Keep locations either in selected clusters OR not in any cluster
        in_selected <- new_locations$CLUSTER %in% cluster_ids
        not_in_any <- is.na(new_locations$CLUSTER)
        new_locations <- new_locations[in_selected | not_in_any, , drop = FALSE]
    }

    # Build new result
    result <- list(
        clusters = new_clusters,
        locations = new_locations
    )

    # Copy raw data if present
    if (!is.null(x$col)) result$col <- x$col[x$col$CLUSTER %in% cluster_ids, , drop = FALSE]
    if (!is.null(x$gis)) result$gis <- x$gis[x$gis$CLUSTER %in% cluster_ids, , drop = FALSE]
    # Note: rr, sci, llr are location-level so would need separate filtering

    class(result) <- "satscan_result"

    # Copy attributes
    attributes(result) <- c(attributes(result), attributes(x)[!names(attributes(x)) %in% c("names", "class")])

    result
}


# =============================================================================
# 4. CONVERSION METHODS
# =============================================================================

#' Convert SaTScan Results to Data Frame
#'
#' @description
#' Converts a `satscan_result` object to a standard data frame. You can choose
#' to extract either clusters or locations.
#'
#' @param x A `satscan_result` object.
#' @param row.names Passed to as.data.frame (usually NULL).
#' @param optional Passed to as.data.frame.
#' @param what Character: "clusters" or "locations". Default is "clusters".
#' @param ... Additional arguments (currently unused).
#'
#' @return A data.frame containing either cluster-level or location-level data.
#'
#' @examples
#' \dontrun{
#' result <- satscanr(cas, geo, pop, ...)
#'
#' # Get clusters as data frame
#' clusters_df <- as.data.frame(result)
#'
#' # Get locations as data frame
#' locations_df <- as.data.frame(result, what = "locations")
#' }
#'
#' @export
as.data.frame.satscan_result <- function(x, row.names = NULL, optional = FALSE,
                                         what = c("clusters", "locations"), ...) {
    what <- match.arg(what)

    if (what == "clusters") {
        if (is.null(x$clusters)) {
            return(data.frame())
        }
        df <- x$clusters
    } else {
        if (is.null(x$locations)) {
            return(data.frame())
        }
        df <- x$locations
    }

    # Ensure it's a plain data.frame
    class(df) <- "data.frame"
    df
}


# =============================================================================
# 5. ATTRIBUTE STORAGE HELPERS
# =============================================================================
# These are not S3 methods but helper functions for storing metadata

#' Store Analysis Parameters in Result
#'
#' @description
#' Adds analysis parameters as attributes to the satscan_result object.
#' This is intended to be called internally by satscanr().
#'
#' @param result A `satscan_result` object.
#' @param params List of analysis parameters.
#' @return The result object with parameters attached.
#' @keywords internal
attach_analysis_params <- function(result, params) {
    attr(result, "parameters") <- params
    attr(result, "timestamp") <- Sys.time()
    result
}


#' Extract Analysis Parameters from Result
#'
#' @description
#' Retrieves stored analysis parameters from a satscan_result object.
#'
#' @param x A `satscan_result` object.
#' @return List of parameters, or NULL if not stored.
#' @export
get_params <- function(x) {
    if (!inherits(x, "satscan_result")) {
        stop("x must be a satscan_result object", call. = FALSE)
    }

    # 1. Check attribute (standard)
    p <- attr(x, "parameters")

    # 2. Check field (fallback for satscanr output)
    if (is.null(p)) p <- x$prm

    return(p)
}


#' Extract Human-Readable Metadata
#'
#' @param x A `satscan_result` object.
#' @return List of metadata
#' @keywords internal
get_satscan_metadata <- function(x) {
    # 1. Try pre-computed summary attached to result
    s <- attr(x, "summary_info")
    if (!is.null(s)) {
        return(s)
    }

    # 2. Fallback: Extract from stored prm
    p <- get_params(x)
    if (is.null(p)) {
        return(list())
    }

    # Use centralized summarizer
    prm_summarize(p)
}


# =============================================================================
# INTERNAL HELPER FUNCTIONS
# =============================================================================

#' Compute Cluster Summary Statistics
#'
#' @param clusters Cluster data frame
#' @return Data frame with summary stats
#' @keywords internal
compute_cluster_summary <- function(clusters) {
    stats <- list()

    if ("P_VALUE" %in% names(clusters)) {
        stats$p_value <- summary(clusters$P_VALUE)
    }

    if ("CLU_RR" %in% names(clusters)) {
        stats$relative_risk <- summary(clusters$CLU_RR)
    }

    if ("RADIUS" %in% names(clusters)) {
        stats$radius_km <- summary(clusters$RADIUS)
    }

    if (length(stats) == 0) {
        return(NULL)
    }

    # Convert to data frame safely
    res <- do.call(rbind, lapply(names(stats), function(name) {
        s <- stats[[name]]
        data.frame(
            statistic = name,
            min = as.numeric(s[1]),
            q1 = as.numeric(s[2]),
            median = as.numeric(s[3]),
            mean = as.numeric(s[4]),
            q3 = as.numeric(s[5]),
            max = as.numeric(s[6]),
            stringsAsFactors = FALSE
        )
    }))
    rownames(res) <- NULL
    res
}


#' Compute Location Summary Statistics
#'
#' @param locations Location data frame
#' @return List with summary info
#' @keywords internal
compute_location_summary <- function(locations) {
    list(
        total = nrow(locations),
        in_cluster = if ("CLUSTER" %in% names(locations)) {
            sum(!is.na(locations$CLUSTER))
        } else {
            0
        },
        with_rr = if ("REL_RISK" %in% names(locations)) {
            sum(!is.na(locations$REL_RISK))
        } else {
            0
        }
    )
}


#' Create Tidy Data Frame
#'
#' @description
#' Converts to tibble if available, otherwise returns data.frame.
#'
#' @param df A data frame
#' @return Tibble if available, otherwise data.frame
#' @keywords internal
make_tidy_df <- function(df) {
    if (requireNamespace("tibble", quietly = TRUE)) {
        tibble::as_tibble(df)
    } else {
        df
    }
}
