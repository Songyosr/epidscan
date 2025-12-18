# Tests for prep_grd (Grid File Preparation)

test_that("prep_grd acts as wrapper around prep_geo", {
    df <- data.frame(id = "G1", x = 1, y = 2)
    res <- prep_grd(df, loc_id = id, coords = c("x", "y"))

    expect_equal(res$kind, "grd")
    expect_equal(res$data$coord1, 2) # Y/Lat
    expect_equal(res$data$coord2, 1) # X/Long
})

test_that("prep_grd detects latlong coordinates and swaps correctly", {
    df <- data.frame(
        id = c("G1", "G2"),
        long = c(100, 101),
        lat = c(13, 14)
    )

    res <- prep_grd(df, loc_id = id, coords = c("long", "lat"))

    expect_equal(res$kind, "grd")
    expect_equal(res$spec$coord_type, "latlong")
    # Should swap to Lat, Long for SaTScan
    expect_equal(res$data$coord1[1], 13) # Lat
    expect_equal(res$data$coord2[1], 100) # Long
})

test_that("prep_grd handles sf input", {
    skip_if_not_installed("sf")

    # Create sf point grid
    grid_pts <- sf::st_as_sf(
        data.frame(id = c("G1", "G2"), long = c(100, 101), lat = c(13, 14)),
        coords = c("long", "lat"),
        crs = 4326
    )

    res <- prep_grd(grid_pts, loc_id = id)

    expect_equal(res$kind, "grd")
    expect_equal(res$spec$coord_type, "latlong")
    expect_equal(nrow(res$data), 2)
})

test_that("prep_grd produces correct structure for satscanr", {
    df <- data.frame(id = "G1", x = 500000, y = 600000)
    res <- prep_grd(df, loc_id = id, coords = c("x", "y"))

    # Check expected columns
    expect_true("loc_id" %in% names(res$data))
    expect_true("coord1" %in% names(res$data))
    expect_true("coord2" %in% names(res$data))

    # Kind should be "grd" not "geo"
    expect_equal(res$kind, "grd")
})
