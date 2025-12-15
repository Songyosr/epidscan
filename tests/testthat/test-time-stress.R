test_that("Stress test time precision: Year", {
    # Create temp work dir
    tmp_wd <- file.path(tempdir(), "test_year")
    if (dir.exists(tmp_wd)) unlink(tmp_wd, recursive = TRUE)
    dir.create(tmp_wd)

    # Data with Date objects spanning years
    df <- data.frame(
        id = 1:5,
        cases = c(1, 1, 1, 1, 1),
        date = seq(as.Date("2020-01-01"), as.Date("2024-01-01"), by = "year"),
        lat = rnorm(5),
        long = rnorm(5)
    )

    # Check what error is actually thrown
    expect_error(
        epid_satscan(
            data = df,
            obs_col = cases,
            date_col = date,
            lat_col = lat,
            long_col = long,
            id_col = id,
            time_precision = "Year",
            work_dir = tmp_wd
        ),
        "SatScan path not set"
    )

    # Check generated .cas file
    cas_file <- file.path(tmp_wd, "epid.cas")
    expect_true(file.exists(cas_file))

    if (file.exists(cas_file)) {
        cas_content <- readLines(cas_file)
        # Verify format is YYYY
        # Line format: id cases date
        # e.g. "1 1 2020"
        parts <- strsplit(cas_content[1], "\\s+")[[1]]
        date_part <- parts[3]
        expect_match(date_part, "^\\d{4}$") # Should differ from YYYY/MM/DD
    }
})

test_that("Stress test time precision: Month", {
    tmp_wd <- file.path(tempdir(), "test_month")
    if (dir.exists(tmp_wd)) unlink(tmp_wd, recursive = TRUE)
    dir.create(tmp_wd)

    df <- data.frame(
        id = 1:5,
        cases = 1,
        date = seq(as.Date("2020-01-01"), as.Date("2020-05-01"), by = "month"),
        lat = rnorm(5),
        long = rnorm(5)
    )

    expect_error(
        epid_satscan(
            data = df,
            obs_col = cases,
            date_col = date,
            lat_col = lat,
            long_col = long,
            id_col = id,
            time_precision = "Month",
            work_dir = tmp_wd
        ),
        "SatScan path not set"
    )

    cas_file <- file.path(tmp_wd, "epid.cas")
    expect_true(file.exists(cas_file))

    if (file.exists(cas_file)) {
        cas_content <- readLines(cas_file)
        # Verify format is YYYY/MM
        parts <- strsplit(cas_content[1], "\\s+")[[1]]
        date_part <- parts[3]
        expect_match(date_part, "^\\d{4}/\\d{2}$")
    }
})

test_that("Stress test time precision: Generic (Week Numbers)", {
    tmp_wd <- file.path(tempdir(), "test_generic")
    if (dir.exists(tmp_wd)) unlink(tmp_wd, recursive = TRUE)
    dir.create(tmp_wd)

    df <- data.frame(
        id = 1:5,
        cases = 1,
        week = c(1, 2, 3, 4, 52),
        lat = rnorm(5),
        long = rnorm(5)
    )

    expect_error(
        epid_satscan(
            data = df,
            obs_col = cases,
            date_col = week,
            lat_col = lat,
            long_col = long,
            id_col = id,
            time_precision = "Generic", # Explicit or Auto logic check
            work_dir = tmp_wd
        ),
        "SatScan path not set"
    )

    cas_file <- file.path(tmp_wd, "epid.cas")
    expect_true(file.exists(cas_file))

    if (file.exists(cas_file)) {
        cas_content <- readLines(cas_file)
        # Verify format is just the number
        parts <- strsplit(cas_content[5], "\\s+")[[1]]
        date_part <- parts[3]
        expect_equal(date_part, "52")
    }
})

test_that("Stress test time precision: Mismatch (Day specified for Numeric)", {
    tmp_wd <- file.path(tempdir(), "test_mismatch")
    if (dir.exists(tmp_wd)) unlink(tmp_wd, recursive = TRUE)
    dir.create(tmp_wd)

    df <- data.frame(
        id = 1:5,
        cases = 1,
        day = 1:5,
        lat = rnorm(5),
        long = rnorm(5)
    )

    expect_error(
        epid_satscan(
            data = df,
            obs_col = cases,
            date_col = day,
            lat_col = lat,
            long_col = long,
            id_col = id,
            time_precision = "Day",
            work_dir = tmp_wd
        )
    )
})

test_that("Stress test time precision: Year (Character)", {
    tmp_wd <- file.path(tempdir(), "test_year_char")
    if (dir.exists(tmp_wd)) unlink(tmp_wd, recursive = TRUE)
    dir.create(tmp_wd)

    df <- data.frame(
        id = 1:5,
        cases = 1,
        year_char = as.character(c(2020, 2021, 2022, 2023, 2024)),
        lat = rnorm(5),
        long = rnorm(5)
    )

    expect_error(
        epid_satscan(
            data = df,
            obs_col = cases,
            date_col = year_char,
            lat_col = lat,
            long_col = long,
            id_col = id,
            time_precision = "Year",
            work_dir = tmp_wd
        ),
        "SatScan path not set"
    )

    cas_file <- file.path(tmp_wd, "epid.cas")
    expect_true(file.exists(cas_file))

    if (file.exists(cas_file)) {
        cas_content <- readLines(cas_file)
        parts <- strsplit(cas_content[1], "\\s+")[[1]]
        date_part <- parts[3]
        expect_match(date_part, "^2020$")
    }
})
