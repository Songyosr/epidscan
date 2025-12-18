# Tests for satscan_table S3 class

test_that("satscan_table creates valid object with correct structure", {
    df <- data.frame(
        loc_id = c("A", "B"),
        cases = c(5, 10),
        stringsAsFactors = FALSE
    )

    result <- satscan_table(df, kind = "cas", spec = list(time_precision = "generic"))

    expect_s3_class(result, "satscan_table")
    expect_equal(result$kind, "cas")
    expect_equal(result$spec$time_precision, "generic")
    expect_equal(nrow(result$data), 2)
})

test_that("satscan_table validates kind argument", {
    df <- data.frame(x = 1)

    expect_error(
        satscan_table(df, kind = "invalid_kind"),
        "Invalid kind"
    )
})

test_that("satscan_table validates data is data.frame", {
    expect_error(
        satscan_table(list(x = 1, y = 2), kind = "cas"),
        "must be a data.frame"
    )

    expect_error(
        satscan_table("not a dataframe", kind = "cas"),
        "must be a data.frame"
    )
})

test_that("satscan_table accepts all valid kinds", {
    df <- data.frame(x = 1)

    valid_kinds <- c("cas", "pop", "geo", "ctl", "grd")

    for (k in valid_kinds) {
        result <- satscan_table(df, kind = k)
        expect_equal(result$kind, k)
    }
})

test_that("satscan_table works with empty spec", {
    df <- data.frame(loc_id = "A", cases = 1)

    result <- satscan_table(df, kind = "cas")

    expect_s3_class(result, "satscan_table")
    expect_equal(length(result$spec), 0)
})

test_that("print.satscan_table outputs expected format", {
    df <- data.frame(
        loc_id = c("A", "B"),
        cases = c(5, 10)
    )
    tbl <- satscan_table(df, kind = "cas", spec = list(time_precision = "day"))

    # Capture output
    output <- capture.output(print(tbl))

    expect_true(any(grepl("<SaTScan Table: cas>", output)))
    expect_true(any(grepl("Metadata:", output)))
    expect_true(any(grepl("time_precision: day", output)))
    expect_true(any(grepl("Data: 2 rows", output)))
})

test_that("print.satscan_table returns invisible object", {
    df <- data.frame(loc_id = "A", cases = 1)
    tbl <- satscan_table(df, kind = "geo")

    # Check invisible return
    result <- withVisible(print(tbl))
    expect_false(result$visible)
    expect_s3_class(result$value, "satscan_table")
})
