test_that("parse_satscan_output handles empty results gracefully", {
    # Mock SatScan result with no clusters
    mock_ss <- list(
        col = NULL,
        gis = NULL
    )

    geo_df <- data.frame(id = as.character(1:5), lat = 10, long = 10)

    result <- parse_satscan_output(mock_ss, geo_df = geo_df, loc_id_col = "id")

    # Check that we got results
    # locations should be present but clusters NULL
    expect_s3_class(result$locations, "data.frame")
    expect_null(result$clusters)

    # Show results for user demo
    print(result)
    print(summary(result))
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
        gis = mock_gis,
        rr = NULL
    )

    geo_df <- data.frame(id = as.character(c(1, 2, 3)), lat = c(10, 10, 20), long = c(10, 10, 20))

    # Test locations summary
    result <- parse_satscan_output(mock_ss, geo_df = geo_df, loc_id_col = "id")

    locs <- result$locations
    expect_equal(nrow(locs), 3)

    # Check IDs 1 and 2 define cluster 1
    expect_equal(locs$CLUSTER[locs$id == "1"], 1)
    expect_equal(locs$CLUSTER[locs$id == "2"], 1)

    # Check ID 3 has NA cluster
    expect_true(is.na(locs$CLUSTER[locs$id == "3"]))
})
