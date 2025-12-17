test_that("prep_geo handles data.frame input and swaps coordinates (X,Y -> Y,X) for Lat/Long", {
    # Input: Longitude (X), Latitude (Y)
    df <- data.frame(
        id = "A",
        long = 100,
        lat = 15,
        stringsAsFactors = FALSE
    )

    # user provides c("long", "lat") -> col1=long, col2=lat
    # prep_geo should swap to Lat, Long for SaTScan (LatLong mode)
    res <- prep_geo(df, loc_id = id, coords = c("long", "lat"))

    expect_equal(res$kind, "geo")
    expect_equal(res$spec$coord_type, "latlong")
    expect_equal(res$data$coord1, 15) # Lat
    expect_equal(res$data$coord2, 100) # Long
})

test_that("prep_geo detects cartesian and keeps X/Y order", {
    df <- data.frame(
        id = "A",
        x = 500000,
        y = 500000
    )

    # X is 500k -> Cartesian
    res <- prep_geo(df, loc_id = id, coords = c("x", "y"))
    expect_equal(res$spec$coord_type, "cartesian")
    # Should KEEP X, Y order
    expect_equal(res$data$coord1, 500000) # X
    expect_equal(res$data$coord2, 500000) # Y
})
