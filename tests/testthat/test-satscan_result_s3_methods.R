# Tests for Enhanced S3 Methods for satscan_result

# =============================================================================
# HELPER: Create Mock Result
# =============================================================================

create_test_result <- function(include_params = FALSE) {
    clusters <- data.frame(
        CLUSTER = 1:3,
        LOC_ID = paste0("CENTER", 1:3),
        LATITUDE = runif(3, 40, 41),
        LONGITUDE = runif(3, -74, -73),
        P_VALUE = c(0.001, 0.04, 0.15),
        CLU_RR = c(2.5, 1.8, 1.2),
        LLR = c(15.5, 10.2, 5.1),
        OBSERVED = c(50, 30, 20),
        EXPECTED = c(20, 18, 17),
        RADIUS = c(5.2, 3.1, 2.5),
        POPULATION = c(1000, 800, 600),
        GINI = c(0.5, 0.3, 0.1),
        START_DATE = c("2023/01/01", "2023/02/01", "2023/03/01"),
        END_DATE = c("2023/01/31", "2023/02/28", "2023/03/31"),
        stringsAsFactors = FALSE
    )

    locations <- data.frame(
        LOC_ID = paste0("LOC", 1:10),
        lat = runif(10, 40, 41),
        lon = runif(10, -74, -73),
        CLUSTER = c(1, 1, 1, 2, 2, NA, NA, 3, 3, NA),
        REL_RISK = c(2.6, 2.4, 2.5, 1.9, 1.7, 0.8, 0.9, 1.3, 1.1, 0.7),
        stringsAsFactors = FALSE
    )

    result <- list(
        clusters = clusters,
        locations = locations
    )
    class(result) <- "satscan_result"

    if (include_params) {
        attr(result, "parameters") <- list(
            AnalysisType = "1",
            ModelType = "0",
            MonteCarloReps = "999"
        )
        attr(result, "timestamp") <- Sys.time()
    }

    result
}


# =============================================================================
# TESTS: summary.satscan_result
# =============================================================================

test_that("summary.satscan_result creates correct structure", {
    result <- create_test_result()
    summ <- summary(result)

    expect_s3_class(summ, "summary.satscan_result")
    expect_equal(summ$n_clusters, 3)
    expect_equal(summ$n_significant, 2)
    expect_equal(summ$n_locations, 10)
    expect_equal(summ$n_in_clusters, 7)
})

test_that("print.satscan_result outputs raw text by default", {
    result <- create_test_result()
    result$main <- "MOCK RAW SATSCAN OUTPUT"

    expect_output(print(result), "MOCK RAW SATSCAN OUTPUT")

    # If x$main is NULL, should fallback to summary
    result$main <- NULL
    expect_output(print(result), "SaTScan Results Summary")
})

test_that("summary.satscan_result identifies most likely cluster", {
    result <- create_test_result()
    summ <- summary(result)

    expect_equal(summ$most_likely$CLUSTER, 1)
    expect_equal(summ$most_likely$P_VALUE, 0.001)
})

test_that("summary.satscan_result computes RR range", {
    result <- create_test_result()
    summ <- summary(result)

    expect_equal(length(summ$rr_range), 2)
    expect_true(summ$rr_range[1] < summ$rr_range[2])
})

test_that("summary.satscan_result handles empty results", {
    empty_result <- list(
        clusters = NULL,
        locations = data.frame()
    )
    class(empty_result) <- "satscan_result"

    summ <- summary(empty_result)
    expect_equal(summ$n_clusters, 0)
    expect_equal(summ$n_significant, 0)
})

test_that("summary.satscan_result includes analysis params if present", {
    result <- create_test_result(include_params = TRUE)
    summ <- summary(result)

    expect_false(is.null(summ$analysis_params))
    expect_equal(summ$analysis_params$AnalysisType, "1")
})

test_that("print.summary.satscan_result outputs correctly", {
    result <- create_test_result()
    summ <- summary(result)

    output <- capture.output(print(summ))

    expect_true(any(grepl("SaTScan Results Summary", output)))
    expect_true(any(grepl("Most Likely Cluster", output)))
    expect_true(any(grepl("Clusters detected", output)))
})


# =============================================================================
# TESTS: tidy.satscan_result
# =============================================================================

test_that("tidy.satscan_result returns correct structure", {
    result <- create_test_result()
    tidy_df <- tidy(result)

    expect_true(is.data.frame(tidy_df))
    expect_equal(nrow(tidy_df), 3)
    expect_true("cluster" %in% names(tidy_df))
    expect_true("p_value" %in% names(tidy_df))
    expect_true("relative_risk" %in% names(tidy_df))
})

test_that("tidy.satscan_result handles empty results", {
    empty_result <- list(
        clusters = NULL,
        locations = data.frame()
    )
    class(empty_result) <- "satscan_result"

    tidy_df <- tidy(empty_result)
    expect_equal(nrow(tidy_df), 0)
    expect_true("cluster" %in% names(tidy_df))
})

test_that("tidy.satscan_result includes all relevant columns", {
    result <- create_test_result()
    tidy_df <- tidy(result)

    expected_cols <- c(
        "cluster", "p_value", "relative_risk",
        "observed", "expected", "obs_exp_ratio",
        "radius_km", "start_date", "end_date",
        "latitude", "longitude"
    )

    for (col in expected_cols) {
        expect_true(col %in% names(tidy_df),
            info = paste("Missing column:", col)
        )
    }

    # Type checks
    expect_s3_class(tidy_df$start_date, "Date")
    expect_s3_class(tidy_df$end_date, "Date")
    expect_type(tidy_df$cluster, "integer")
    expect_type(tidy_df$observed, "double")
})

test_that("tidy.satscan_result computes obs/exp ratio correctly", {
    result <- create_test_result()
    tidy_df <- tidy(result)

    expect_equal(tidy_df$obs_exp_ratio[1], 50 / 20)
    expect_equal(tidy_df$obs_exp_ratio[2], 30 / 18)
})


# =============================================================================
# TESTS: glance.satscan_result
# =============================================================================

test_that("glance.satscan_result returns single row", {
    result <- create_test_result()
    glance_df <- glance(result)

    expect_equal(nrow(glance_df), 1)
    expect_true(is.data.frame(glance_df))
})

test_that("glance.satscan_result contains correct metrics", {
    result <- create_test_result()
    glance_df <- glance(result)

    expect_equal(glance_df$n_clusters, 3)
    expect_equal(glance_df$n_significant, 2)
    expect_equal(glance_df$n_locations, 10)
    expect_equal(glance_df$n_in_clusters, 7)
    expect_equal(glance_df$min_p_value, 0.001)
    expect_equal(glance_df$max_relative_risk, 2.5)
})

test_that("glance.satscan_result computes proportion correctly", {
    result <- create_test_result()
    glance_df <- glance(result)

    expect_equal(glance_df$prop_in_clusters, 7 / 10)
})

test_that("glance.satscan_result handles empty results", {
    empty_result <- list(
        clusters = NULL,
        locations = data.frame()
    )
    class(empty_result) <- "satscan_result"

    glance_df <- glance(empty_result)
    expect_equal(glance_df$n_clusters, 0)
    expect_true(is.na(glance_df$min_p_value))
})


# =============================================================================
# TESTS: augment.satscan_result
# =============================================================================

test_that("augment.satscan_result adds cluster columns", {
    result <- create_test_result()
    aug_df <- augment(result)

    expect_true(".cluster" %in% names(aug_df))
    expect_true(".in_cluster" %in% names(aug_df))
    expect_true(".relative_risk" %in% names(aug_df))
    expect_true(".cluster_p_value" %in% names(aug_df))
})

test_that("augment.satscan_result preserves original data", {
    result <- create_test_result()
    aug_df <- augment(result)

    expect_true("LOC_ID" %in% names(aug_df))
    expect_true("lat" %in% names(aug_df))
    expect_true("lon" %in% names(aug_df))
})

test_that("augment.satscan_result marks cluster membership correctly", {
    result <- create_test_result()
    aug_df <- augment(result)

    # First 3 locations in cluster 1
    expect_equal(sum(aug_df$.in_cluster), 7)
    expect_equal(aug_df$.cluster[1], 1)
    expect_true(aug_df$.in_cluster[1])
    expect_false(aug_df$.in_cluster[6]) # Not in cluster
})

test_that("augment.satscan_result joins p-values correctly", {
    result <- create_test_result()
    aug_df <- augment(result)

    # Locations in cluster 1 should have p-value 0.001
    loc1_pval <- aug_df$.cluster_p_value[aug_df$.cluster == 1 & !is.na(aug_df$.cluster)]
    expect_true(all(loc1_pval == 0.001))
})

test_that("augment.satscan_result can augment custom data", {
    result <- create_test_result()

    custom_data <- data.frame(
        LOC_ID = paste0("LOC", 1:5),
        population = c(1000, 2000, 1500, 1800, 1200)
    )

    # Need to merge with locations to get cluster info
    custom_with_clusters <- merge(custom_data, result$locations[, c("LOC_ID", "CLUSTER", "REL_RISK")],
        by = "LOC_ID", all.x = TRUE
    )

    aug_df <- augment(result, data = custom_with_clusters)

    expect_true("population" %in% names(aug_df))
    expect_true(".cluster" %in% names(aug_df))
})


# =============================================================================
# TESTS: [.satscan_result (subsetting)
# =============================================================================

test_that("subsetting by numeric index works", {
    result <- create_test_result()
    subset <- result[1]

    expect_s3_class(subset, "satscan_result")
    expect_equal(nrow(subset$clusters), 1)
    expect_equal(subset$clusters$CLUSTER, 1)
})

test_that("subsetting by multiple indices works", {
    result <- create_test_result()
    subset <- result[c(1, 3)]

    expect_equal(nrow(subset$clusters), 2)
    expect_equal(subset$clusters$CLUSTER, c(1, 3))
})

test_that("subsetting filters locations correctly", {
    result <- create_test_result()
    subset <- result[1]

    # Should keep only locations in cluster 1 + locations not in any cluster
    in_cluster_1 <- sum(subset$locations$CLUSTER == 1, na.rm = TRUE)
    not_in_any <- sum(is.na(subset$locations$CLUSTER))

    expect_equal(in_cluster_1, 3) # 3 locations in cluster 1
    expect_gt(not_in_any, 0) # Some locations not in any cluster
})

test_that("subsetting by logical index works", {
    result <- create_test_result()
    sig_idx <- result$clusters$P_VALUE < 0.05

    subset <- result[sig_idx]

    expect_equal(nrow(subset$clusters), 2)
    expect_true(all(subset$clusters$P_VALUE < 0.05))
})

test_that("subsetting preserves class", {
    result <- create_test_result()
    subset <- result[1]

    expect_s3_class(subset, "satscan_result")
})


# =============================================================================
# TESTS: as.data.frame.satscan_result
# =============================================================================

test_that("as.data.frame returns clusters by default", {
    result <- create_test_result()
    df <- as.data.frame(result)

    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 3)
    expect_true("CLUSTER" %in% names(df))
})

test_that("as.data.frame can return locations", {
    result <- create_test_result()
    df <- as.data.frame(result, what = "locations")

    expect_equal(nrow(df), 10)
    expect_true("LOC_ID" %in% names(df))
})

test_that("as.data.frame returns plain data.frame", {
    result <- create_test_result()
    df <- as.data.frame(result)

    expect_equal(class(df), "data.frame")
})


# =============================================================================
# TESTS: get_params
# =============================================================================

test_that("get_params retrieves stored parameters", {
    result <- create_test_result(include_params = TRUE)
    params <- get_params(result)

    expect_equal(params$AnalysisType, "1")
    expect_equal(params$ModelType, "0")
    expect_equal(params$MonteCarloReps, "999")
})

test_that("get_params returns NULL when no params stored", {
    result <- create_test_result(include_params = FALSE)
    params <- get_params(result)

    expect_null(params)
})

test_that("get_params validates input", {
    expect_error(
        get_params(list(a = 1)),
        "must be a satscan_result"
    )
})


# =============================================================================
# TESTS: Helper Functions
# =============================================================================

test_that("compute_cluster_summary returns valid stats", {
    result <- create_test_result()
    stats <- compute_cluster_summary(result$clusters)

    expect_true(is.data.frame(stats))
    expect_true("statistic" %in% names(stats))
    expect_true(any(stats$statistic == "p_value"))
    expect_true(any(stats$statistic == "relative_risk"))
})

test_that("compute_location_summary returns counts", {
    result <- create_test_result()
    stats <- compute_location_summary(result$locations)

    expect_equal(stats$total, 10)
    expect_equal(stats$in_cluster, 7)
})


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("all S3 methods work together in a workflow", {
    result <- create_test_result(include_params = TRUE)

    # Summary
    summ <- summary(result)
    expect_s3_class(summ, "summary.satscan_result")

    # Tidy
    tidy_df <- tidy(result)
    expect_equal(nrow(tidy_df), 3)

    # Glance
    glance_df <- glance(result)
    expect_equal(nrow(glance_df), 1)

    # Augment
    aug_df <- augment(result)
    expect_true(".cluster" %in% names(aug_df))

    # Subset
    subset <- result[1:2]
    expect_equal(nrow(subset$clusters), 2)

    # Convert
    df <- as.data.frame(subset)
    expect_equal(nrow(df), 2)

    # Get params
    params <- get_params(result)
    expect_false(is.null(params))
})

test_that("methods handle edge case: no significant clusters", {
    result <- create_test_result()
    result$clusters$P_VALUE <- c(0.1, 0.2, 0.3) # All non-significant

    summ <- summary(result)
    expect_equal(summ$n_significant, 0)
    expect_null(summ$most_likely)

    glance_df <- glance(result)
    expect_equal(glance_df$n_significant, 0)
})

test_that("methods handle edge case: single cluster", {
    result <- create_test_result()
    result$clusters <- result$clusters[1, , drop = FALSE]
    result$locations <- result$locations[1:3, , drop = FALSE]

    summ <- summary(result)
    expect_equal(summ$n_clusters, 1)

    tidy_df <- tidy(result)
    expect_equal(nrow(tidy_df), 1)

    glance_df <- glance(result)
    expect_equal(glance_df$n_clusters, 1)
})
