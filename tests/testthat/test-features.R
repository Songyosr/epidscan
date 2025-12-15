test_that("extract_geometry supports Cartesian coordinates", {
    skip_if_not_installed("sf")

    # Create a projected point (Pseudo-Mercator)
    p <- sf::st_sfc(sf::st_point(c(1000, 2000)), crs = 3857)
    df <- sf::st_sf(id = 1, geometry = p)

    # 1. Default (latlong) -> transform to WGS84
    # 1000m/2000m is very close to 0,0 lat/long
    res_latlong <- extract_geometry(df, geo_type = "latlong")
    expect_true(abs(res_latlong$lat) < 1)

    # 2. Cartesian -> no transform (keep 1000, 2000)
    res_cart <- extract_geometry(df, geo_type = "cartesian")
    expect_equal(res_cart$lat, 2000)
    expect_equal(res_cart$long, 1000)
})

test_that("build_satscan_options handles Cartesian type", {
    files <- list(cas_file = "cases.cas", geo_file = "geo.geo", pop_file = "pop.pop")
    export_df <- data.frame(id = 1, date = as.Date("2024-01-01"))

    # geo_type="cartesian" -> CoordinatesType=0
    opts <- build_satscan_options(files, export_df, time_precision = 1, type = "space-time", model = "poisson", geo_type = "cartesian")
    expect_equal(opts$CoordinatesType, 0)

    # geo_type="latlong" -> CoordinatesType=1
    opts2 <- build_satscan_options(files, export_df, time_precision = 1, type = "space-time", model = "poisson", geo_type = "latlong")
    expect_equal(opts2$CoordinatesType, 1)
})

test_that("epid_satscan accepts start_date/end_date arguments", {
    files <- list(cas_file = "cases.cas", geo_file = "geo.geo", pop_file = "pop.pop")
    export_df <- data.frame(id = 1, date = as.Date("2024-01-15"))

    # 1. Auto-detect dates (min/max from data)
    opts_auto <- build_satscan_options(files, export_df, time_precision = 3, type = "space-time", model = "poisson")
    expect_equal(opts_auto$StartDate, "2024/01/15")
    expect_equal(opts_auto$EndDate, "2024/01/15")

    # 2. Explicit dates override data
    # Precision 3 (Day)
    opts_explicit <- build_satscan_options(files, export_df,
        time_precision = 3, type = "space-time", model = "poisson",
        start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31")
    )
    expect_equal(opts_explicit$StartDate, "2024/01/01")
    expect_equal(opts_explicit$EndDate, "2024/12/31")

    # 3. Precision 1 (Year) - formats correctly
    opts_yr <- build_satscan_options(files, export_df,
        time_precision = 1, type = "space-time", model = "poisson",
        start_date = as.Date("2024-01-01"), end_date = as.Date("2025-01-01")
    )
    expect_equal(opts_yr$StartDate, "2024")
    expect_equal(opts_yr$EndDate, "2025")
})

test_that("epid_satscan validates date arguments", {
    # Check simple string logic
    df <- data.frame(id = 1, cases = 1, date = as.Date("2024-01-01"), lat = 0, long = 0)

    # Must fail if numeric or other structure passed where string expected
    expect_error(
        epid_satscan(df, obs_col = cases, date_col = date, lat_col = lat, long_col = long, start_date = 12345),
        "start_date must be Date, POSIXt or character string"
    )
})
