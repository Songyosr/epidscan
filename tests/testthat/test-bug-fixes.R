test_that("extract_geometry enforces CRS 4326 for projected data", {
    skip_if_not_installed("sf")

    # Create a projected point (Pseudo-Mercator)
    # 100 E, 10 N -> approx mercator: 11131949, 1118890
    p <- sf::st_sfc(sf::st_point(c(11131949, 1118890)), crs = 3857)
    df <- sf::st_sf(id = 1, geometry = p)

    # Should automatically transform to Lat/Long (approx 100, 10)
    res <- extract_geometry(df)

    expect_equal(res$lat, 10, tolerance = 0.1)
    expect_equal(res$long, 100, tolerance = 0.1)
})

test_that("write_satscan_files respects Year precision in output", {
    temp_dir <- tempdir()
    geo_df <- data.frame(id = 1, lat = 10, long = 100)
    export_df <- data.frame(
        id = 1,
        cases = 5,
        date = as.Date("2024-01-01"),
        pop = 100
    )

    # 1. Test Year Precision (1)
    files_yr <- write_satscan_files(geo_df, export_df, temp_dir, "test_yr", time_precision = 1L)

    cas_content <- read.table(files_yr$cas_file)
    # Expected format: 1 5 2024
    expect_equal(as.character(cas_content$V3[1]), "2024")

    pop_content <- read.table(files_yr$pop_file)
    # Expected format: 1 2024 100
    expect_equal(as.character(pop_content$V2[1]), "2024")


    # 2. Test Month Precision (2)
    files_mo <- write_satscan_files(geo_df, export_df, temp_dir, "test_mo", time_precision = 2L)
    cas_content_mo <- read.table(files_mo$cas_file)
    # Expected format: 1 5 2024/01
    expect_equal(as.character(cas_content_mo$V3[1]), "2024/01")
})
