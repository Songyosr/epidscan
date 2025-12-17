test_that("prep_cas enforces sparsity", {
    df <- data.frame(
        id = c("A", "B", "C"),
        cases = c(10, 0, 5),
        stringsAsFactors = FALSE
    )

    # When style is aggregated
    res <- prep_cas(df, loc_id = id, cases = cases, style = "aggregated", time_precision = "generic")

    expect_equal(nrow(res$data), 2)
    expect_equal(res$data$loc_id, c("A", "C"))
    expect_equal(res$kind, "cas")
})

test_that("prep_cas handles casewise input", {
    df <- data.frame(
        id = c("A", "B"),
        stringsAsFactors = FALSE
    )

    res <- prep_cas(df, loc_id = id, style = "casewise", time_precision = "generic")

    expect_equal(res$spec$case_style, "casewise")
    expect_equal(nrow(res$data), 2)
    expect_equal(res$data$cases, c(1, 1))
})

test_that("prep_cas formats dates correctly for 'day'", {
    df <- data.frame(
        id = "A",
        d = as.Date("2023-01-01"),
        cases = 1
    )

    res <- prep_cas(df, loc_id = id, time = d, cases = cases, style = "aggregated", time_precision = "day")

    expect_equal(res$data$time, "2023/01/01")
})
