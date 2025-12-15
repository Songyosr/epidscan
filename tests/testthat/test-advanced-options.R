test_that("build_satscan_options supports extended Analysis and Model types", {
    files <- list(cas_file = "c", geo_file = "g")
    df <- data.frame(id = 1)

    # Standard string mapping
    opts1 <- build_satscan_options(files, df, 3, "spatial-variation-in-temporal-trends", "multinomial")
    expect_equal(opts1$AnalysisType, 5L)
    expect_equal(opts1$ModelType, 7L)

    # Integer passthrough - 7 is invalid
    expect_error(
        build_satscan_options(files, df, 3, 7, 9),
        "AnalysisType 7 \\(Bernoulli\\) is not a valid SaTScan analysis type"
    )

    # String passthrough - "7" -> 7 -> Invalid
    expect_error(
        build_satscan_options(files, df, 3, "7", "9"),
        "AnalysisType 7 \\(Bernoulli\\) is not a valid SaTScan analysis type"
    )
})

test_that("build_satscan_options validates AnalysisType", {
    files <- list(cas_file = "c", geo_file = "g")
    df <- data.frame(id = 1)

    # Error on "bernoulli" as type (it's a model)
    expect_error(
        build_satscan_options(files, df, 3, "bernoulli", "poisson"),
        "AnalysisType 'bernoulli' is not a valid SaTScan analysis type"
    )

    # Warning on unknown type
    expect_warning(
        build_satscan_options(files, df, 3, 99, "poisson"),
        "Unknown AnalysisType: 99"
    )
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
