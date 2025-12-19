test_that("parse_satscan_output creates correct S3 structure", {
    # Mock inputs
    geo_df <- data.frame(
        id = c("1", "2"),
        lat = c(10, 20),
        long = c(10, 20)
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
        ),
        rr = data.frame(
            LOC_ID = c("1", "2"),
            REL_RISK = c(2.5, 0.5)
        )
    )

    # Test Default
    res <- parse_satscan_output(ss_results, geo_df, loc_id_col = "id")

    expect_s3_class(res, "satscan_result")
    expect_equal(nrow(res$locations), 2)
    # Check join
    expect_equal(res$locations$CLUSTER[res$locations$id == "1"], 1)
    expect_equal(res$locations$REL_RISK[res$locations$id == "1"], 2.5)

    # summary should return clusters
    summ <- summary(res)
    expect_equal(summ$n_clusters, 1)
})

test_that("parse_satscan_output handles NULL results", {
    res <- parse_satscan_output(NULL, data.frame(id = "1"), loc_id_col = "id")
    expect_s3_class(res, "satscan_result")
    expect_s3_class(res$locations, "data.frame")
})
