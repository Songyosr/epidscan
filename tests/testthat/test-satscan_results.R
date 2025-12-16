test_that("parse_satscan_output handles empty results gracefully", {
    # Mock SatScan result with no clusters
    mock_ss <- list(
        col = NULL,
        gis = NULL
    )

    data <- data.frame(id = 1:5)
    geo_df <- data.frame(id = as.character(1:5), lat = 10, long = 10)

    result <- parse_satscan_output(mock_ss, data, geo_df, rlang::quo(id))

    expect_s3_class(result, "satscan_result")
    expect_null(result$location_summary)

    # If we convert to DF it should be empty or NULL if no location summary
    expect_null(as.data.frame(result))
})

test_that("parse_satscan_output joins cluster info correctly", {
    # Mock SatScan GIS result
    mock_gis <- data.frame(
        LOC_ID = c("1", "2"),
        CLUSTER = c(1, 1),
        P_VALUE = c(0.001, 0.001),
        CLU_RR = c(5.5, 5.5),
        CLU_ODE = c(10, 10)
    )

    mock_ss <- list(
        col = NULL,
        gis = mock_gis
    )

    data <- data.frame(id = c(1, 2, 3))
    geo_df <- data.frame(id = as.character(c(1, 2, 3)), lat = c(10, 10, 20), long = c(10, 10, 20))

    # Test location summary (default)
    result <- parse_satscan_output(mock_ss, data, geo_df, rlang::quo(id))

    loc_summary <- result$location_summary
    expect_equal(nrow(loc_summary), 3)

    # Check IDs 1 and 2 define cluster 1
    # Note: geo_df IDs are numeric 1,2,3 but join uses char matching usually if prepared right
    expect_equal(loc_summary$CLUSTER[loc_summary$id == 1], 1)
    expect_equal(loc_summary$CLUSTER[loc_summary$id == 2], 1)

    # Check ID 3 has NA cluster
    expect_true(is.na(loc_summary$CLUSTER[loc_summary$id == 3]))
})
