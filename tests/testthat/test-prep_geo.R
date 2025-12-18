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

test_that("prep_geo handles sf point input", {
    skip_if_not_installed("sf")

    # Create sf point object
    pts <- sf::st_as_sf(
        data.frame(id = c("A", "B"), long = c(100, 101), lat = c(13, 14)),
        coords = c("long", "lat"),
        crs = 4326
    )

    res <- prep_geo(pts, loc_id = id)

    expect_equal(res$kind, "geo")
    expect_equal(res$spec$coord_type, "latlong")
    expect_equal(nrow(res$data), 2)
    # Lat should be first (swapped for SaTScan)
    expect_equal(res$data$coord1[1], 13) # Lat
    expect_equal(res$data$coord2[1], 100) # Long
})

test_that("prep_geo extracts centroids from sf polygons", {
    skip_if_not_installed("sf")

    # Create simple polygon
    poly <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
    sf_poly <- sf::st_sf(
        id = "P1",
        geometry = sf::st_sfc(poly, crs = 4326)
    )

    res <- prep_geo(sf_poly, loc_id = id)

    expect_equal(res$kind, "geo")
    # Centroid of unit square is (0.5, 0.5)
    expect_equal(res$data$coord1[1], 0.5, tolerance = 0.01) # Lat
    expect_equal(res$data$coord2[1], 0.5, tolerance = 0.01) # Long
})

test_that("prep_geo errors without coords for data.frame", {
    df <- data.frame(id = "A", x = 1, y = 2)

    expect_error(
        prep_geo(df, loc_id = id),
        "coords"
    )
})
