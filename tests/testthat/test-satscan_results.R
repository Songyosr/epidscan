test_that("parse_results handles empty results gracefully", {
    # Mock SatScan result with no clusters
    mock_ss <- list(
        col = NULL,
        gis = NULL
    )

    data <- data.frame(id = 1:5)
    result <- parse_results(mock_ss, data, rlang::quo(id))

    expect_equal(nrow(result), 5)
    expect_true("CLUSTER" %in% names(result))
    expect_true(all(is.na(result$CLUSTER)))
})

test_that("parse_results joins cluster info correctly", {
    # Mock SatScan GIS result
    mock_gis <- data.frame(
        LOC_ID = c("1", "2"),
        CLUSTER = c(1, 1),
        P_VALUE = c(0.001, 0.001),
        CLU_RR = c(5.5, 5.5)
    )

    mock_ss <- list(
        col = NULL,
        gis = mock_gis
    )

    data <- data.frame(id = c(1, 2, 3))
    # Important: parse_results expects id_col to be a quosure
    result <- parse_results(mock_ss, data, rlang::quo(id))

    expect_equal(nrow(result), 3)

    # Check IDs 1 and 2 define cluster 1
    expect_equal(result$CLUSTER[result$id == 1], 1)
    expect_equal(result$CLUSTER[result$id == 2], 1)

    # Check ID 3 has NA cluster
    expect_true(is.na(result$CLUSTER[result$id == 3]))
})
