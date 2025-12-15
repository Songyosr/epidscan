test_that("build_satscan_options supports extended Analysis and Model types", {
    files <- list(cas_file = "c", geo_file = "g")
    df <- data.frame(id = 1)

    # Standard string mapping
    opts1 <- build_satscan_options(files, df, 3, "spatial-variation-in-temporal-trends", "multinomial")
    expect_equal(opts1$AnalysisType, 5L)
    expect_equal(opts1$ModelType, 7L)

    # Integer passthrough
    opts2 <- build_satscan_options(files, df, 3, 7, 9)
    expect_equal(opts2$AnalysisType, 7L)
    expect_equal(opts2$ModelType, 9L)

    # String passthrough
    opts3 <- build_satscan_options(files, df, 3, "7", "9")
    expect_equal(opts3$AnalysisType, 7L) # Should convert to int if numeric string
    expect_equal(opts3$ModelType, 9L)
})

test_that("epid_satscan supports Prospective Scanning parameters", {
    files <- list(cas_file = "c", geo_file = "g")
    df <- data.frame(id = 1, date = as.Date("2024-01-01"))

    # Normal retrospective (default)
    opts_retro <- build_satscan_options(files, df, 3, "space-time", "poisson", monitor_mode = "retrospective")
    expect_null(opts_retro$ProspectiveStartDate)

    # Prospective
    p_date <- as.Date("2024-06-01")
    opts_pro <- build_satscan_options(files, df, 3, "space-time", "poisson",
        monitor_mode = "prospective", prospective_start_date = p_date
    )
    expect_equal(opts_pro$ProspectiveStartDate, "2024/06/01")
})

test_that("epid_satscan validates prospective requirements", {
    df <- data.frame(id = 1, cases = 1, lat = 0, long = 0, date = as.Date("2024-01-01"))

    # Missing prospective_start_date
    expect_error(
        epid_satscan(df,
            obs_col = cases, date_col = date, lat_col = lat, long_col = long,
            monitor_mode = "prospective"
        ),
        "prospective_start_date is required"
    )
})
