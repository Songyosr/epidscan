test_that("detect_time_precision defaults correctly", {
    # Date inputs -> 3 (Day)
    dates <- as.Date(c("2024-01-01", "2024-01-02"))
    expect_equal(detect_time_precision(dates, NULL), 3)

    # POSIXct inputs -> 3 (Day)
    times <- as.POSIXct(c("2024-01-01 12:00", "2024-01-02 12:00"))
    expect_equal(detect_time_precision(times, NULL), 3)

    # Numeric inputs -> 0 (Generic)
    numeric_times <- c(1, 2, 3)
    expect_equal(detect_time_precision(numeric_times, NULL), 0)
})

test_that("detect_time_precision respects user input", {
    dates <- as.Date(c("2024-01-01", "2024-01-02"))

    expect_equal(detect_time_precision(dates, "Year"), 1)
    expect_equal(detect_time_precision(dates, "Month"), 2)
    expect_equal(detect_time_precision(dates, "Day"), 3)
    expect_equal(detect_time_precision(dates, "Generic"), 0)
})

test_that("build_satscan_options merges parameters correctly", {
    # Mock inputs
    files <- list(cas_file = "case.cas", geo_file = "geo.geo", pop_file = "pop.pop")
    export_df <- data.frame(date = as.Date("2024-01-01"))

    # Base options
    base_opts <- build_satscan_options(
        files = files,
        export_df = export_df,
        time_precision = 3,
        type = "space-time",
        model = "poisson"
    )

    expect_equal(base_opts$AnalysisType, 3) # Space-Time
    expect_equal(base_opts$ModelType, 0) # Poisson
    expect_equal(base_opts$TimeAggregationUnits, 3)
})

test_that("apply_user_overrides works", {
    opts <- list(AnalysisType = 1)
    user_opts <- list(AnalysisType = 2, MonteCarloReps = 9)

    merged <- apply_user_overrides(opts, user_opts)

    expect_equal(merged$AnalysisType, 2)
    expect_equal(merged$MonteCarloReps, 9)
})
