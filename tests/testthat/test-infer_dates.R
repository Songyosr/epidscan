test_that("infer_dates_from_data works correctly", {
    # Mock options and data
    current_opts_empty <- c(StartDate = "", EndDate = "")
    current_opts_set <- c(StartDate = "2024/01/01", EndDate = "2024/12/31")

    # Data mocks
    df_day <- data.frame(time = c("2024/01/01", "2024/06/15"))
    df_month <- data.frame(time = c("2024/01", "2024/06"))
    df_year <- data.frame(time = c("2024", "2025"))

    # 1. No inference needed (already set)
    res <- infer_dates_from_data(current_opts_set, df_day, "day")
    expect_null(res)

    # 2. Inference needed - Day
    res <- infer_dates_from_data(current_opts_empty, df_day, "day")
    expect_equal(res$StartDate, "2024/01/01")
    expect_equal(res$EndDate, "2024/06/15")

    # 3. Inference needed - Month
    res <- infer_dates_from_data(current_opts_empty, df_month, "month")
    expect_equal(res$StartDate, "2024/01/01")
    # End date should be end of June (30 days)
    expect_equal(res$EndDate, "2024/06/30")

    # 4. Inference needed - Year
    res <- infer_dates_from_data(current_opts_empty, df_year, "year")
    expect_equal(res$StartDate, "2024/01/01")
    expect_equal(res$EndDate, "2025/12/31")

    # 5. Missing one end only
    opts_partial <- c(StartDate = "2024/01/01", EndDate = "")
    res <- infer_dates_from_data(opts_partial, df_year, "year")
    expect_null(res$StartDate) # Should be missing/null since we don't need it
    expect_equal(res$EndDate, "2025/12/31")
})
