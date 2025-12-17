test_that("prep_pop handles basic structure", {
    df <- data.frame(
        id = c("A", "B"),
        year = c(2020, 2020),
        pop = c(100, 200),
        stringsAsFactors = FALSE
    )

    res <- prep_pop(df, loc_id = id, time = year, pop = pop)

    expect_equal(res$kind, "pop")
    expect_equal(nrow(res$data), 2)
    expect_equal(res$data$time, c("2020", "2020"))
})

test_that("prep_pop handles covariates", {
    df <- data.frame(
        id = "A",
        year = 2020,
        pop = 100,
        age = "Young",
        stringsAsFactors = FALSE
    )

    res <- prep_pop(df, loc_id = id, time = year, pop = pop, covars = "age")

    expect_true("age" %in% names(res$data))
    expect_equal(res$spec$covars, "age")
})
