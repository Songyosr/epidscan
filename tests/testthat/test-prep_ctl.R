test_that("prep_ctl handles Space-Time (time precision)", {
    df <- data.frame(
        id = c("A", "A"),
        cases = c(1, 1),
        date = as.Date(c("2023-01-01", "2023-01-15")),
        stringsAsFactors = FALSE
    )

    res <- prep_ctl(df, loc_id = id, time = date, cases = cases, time_precision = "month")

    expect_true("time" %in% names(res$data))
    expect_equal(res$data$time[1], "2023/01")
    expect_equal(res$spec$time_precision, "month")
})

test_that("prep_ctl enforces sparsity", {
    df <- data.frame(
        id = c("A", "B"),
        cases = c(1, 0),
        stringsAsFactors = FALSE
    )

    res <- prep_ctl(df, loc_id = id, cases = cases, style = "aggregated")

    expect_equal(res$kind, "ctl")
    expect_equal(nrow(res$data), 1)
    expect_equal(res$data$cases, 1)
})

test_that("prep_ctl handles casewise", {
    df <- data.frame(id = "A")
    res <- prep_ctl(df, loc_id = id, style = "casewise")
    expect_equal(res$data$cases, 1)
})
