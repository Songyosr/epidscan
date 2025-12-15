test_that("extract_geometry works with sf objects", {
    # Create a simple sf object
    coords <- matrix(c(100, 101, 10, 11), ncol = 2)
    data <- sf::st_as_sf(
        data.frame(id = 1:2, cases = c(5, 10)),
        geometry = sf::st_sfc(sf::st_point(coords[1, ]), sf::st_point(coords[2, ])),
        crs = 4326
    )

    result <- extract_geometry(data, NULL, NULL)

    expect_equal(nrow(result), 2)
    expect_equal(result$lat, c(10, 11))
    expect_equal(result$long, c(100, 101))
})

test_that("extract_geometry works with data.frame and specified columns", {
    data <- data.frame(
        id = 1:2,
        cases = c(5, 10),
        latitude = c(10, 11),
        longitude = c(100, 101)
    )

    # Note: extract_geometry expects quosures for lat/long if not NULL
    # But in the helper it uses eval_tidy, so we need to pass them correctly or simulate the call

    # Let's check how extract_geometry is implemented: it takes quosures.
    # So we should pass quosures.

    lat_quo <- rlang::quo(latitude)
    long_quo <- rlang::quo(longitude)

    result <- extract_geometry(data, lat_quo, long_quo)

    expect_equal(nrow(result), 2)
    expect_equal(result$lat, c(10, 11))
    expect_equal(result$long, c(100, 101))
})

test_that("extract_geometry errors when columns missing", {
    data <- data.frame(id = 1:2)
    expect_error(extract_geometry(data, rlang::quo(), rlang::quo()))
})

test_that("write_satscan_files creates correct files", {
    # Setup data
    temp_dir <- tempdir()
    geo_df <- data.frame(id = 1:2, lat = c(10, 11), long = c(100, 101))

    export_df <- data.frame(
        type = "cases",
        id = 1:2,
        cases = c(5, 10),
        date = as.Date(c("2024-01-01", "2024-01-02"))
    )

    # Mock project name
    project_name <- "test_project"

    # Run
    files <- write_satscan_files(geo_df, export_df, temp_dir, project_name)

    # Check file existence
    expect_true(file.exists(files$cas_file))
    expect_true(file.exists(files$geo_file))

    # specific checks for content could be added here

    # Cleanup
    unlink(files$cas_file)
    unlink(files$geo_file)
    if (!is.null(files$pop_file)) unlink(files$pop_file)
})
