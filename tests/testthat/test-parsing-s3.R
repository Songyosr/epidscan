test_that("parse_satscan_output creates correct S3 structure", {
    # Mock inputs
    data <- data.frame(
        id = rep(c("1", "2"), each = 2),
        date = rep(as.Date(c("2020-01-01", "2020-01-02")), 2),
        cases = c(0, 5, 0, 0),
        pop = 100
    )
    geo_df <- data.frame(
        id = rep(c("1", "2"), each = 2),
        lat = c(10, 10, 20, 20),
        long = c(10, 10, 20, 20)
    )

    # Mock ss_results
    ss_results <- list(
        gis = data.frame(
            LOC_ID = c("1", "2"),
            CLUSTER = c(1, NA),
            CLU_RR = c(2.5, NA),
            CLU_ODE = c(10, NA),
            P_VALUE = c(0.01, NA)
        ),
        col = data.frame(
            CLUSTER = 1,
            START_DATE = "2020/01/02",
            END_DATE = "2020/01/02",
            P_VALUE = 0.01
        )
    )

    id_quo <- rlang::quo(id)

    # Test Default (merge_time_series = FALSE)
    res <- parse_satscan_output(ss_results, data, geo_df, id_quo, merge_time_series = FALSE)

    expect_s3_class(res, "satscan_result")
    expect_null(res$main_results)
    expect_equal(nrow(res$location_summary), 2)
    expect_equal(res$location_summary$id, c("1", "2"))
    expect_true("CLUSTER" %in% names(res$location_summary))

    # Check S3 methods
    # as.data.frame should return location_summary by default
    df_ver <- as.data.frame(res)
    expect_equal(nrow(df_ver), 2)
    expect_equal(df_ver$id, c("1", "2"))

    # Test merge_time_series = TRUE
    res_full <- parse_satscan_output(ss_results, data, geo_df, id_quo, merge_time_series = TRUE)
    expect_false(is.null(res_full$main_results))
    expect_equal(nrow(res_full$main_results), 4) # Matches input data rows
    expect_true("CLUSTER" %in% names(res_full$main_results))

    # Check as.data.frame uses main_results if present
    df_full <- as.data.frame(res_full)
    expect_equal(nrow(df_full), 4)
})

test_that("parse_satscan_output handles NULL results", {
    res <- parse_satscan_output(NULL, data.frame(), data.frame(), rlang::quo(id))
    expect_s3_class(res, "satscan_result")
    expect_null(res$location_summary)
})
