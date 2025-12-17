test_that("write_satscan_files writes covariates correctly", {
    # Mock data
    geo_df <- data.frame(id = 1:2, lat = c(10, 10), long = c(20, 20))
    export_df <- data.frame(
        id = c(1, 1, 2, 2),
        cases = c(10, 5, 20, 10),
        pop = c(100, 100, 200, 200),
        age_group = c("Child", "Adult", "Child", "Adult"),
        sex = c("M", "F", "M", "F")
    )

    work_dir <- tempdir()

    # Write files with covariates
    res <- write_satscan_files(geo_df, export_df, work_dir, covariates = c("age_group", "sex"))

    # Check Case File
    cas <- read.table(res$cas_file)
    expect_equal(ncol(cas), 4) # ID, Cases, Age, Sex (No date here)
    expect_equal(cas[, 3], c("Child", "Adult", "Child", "Adult"))
    expect_equal(cas[, 4], c("M", "F", "M", "F"))

    # Check Pop File
    pop <- read.table(res$pop_file)
    expect_equal(ncol(pop), 4) # ID, Pop, Age, Sex (No date)
    expect_equal(pop[, 3], c("Child", "Adult", "Child", "Adult"))
})

test_that("epid_satscan accepts covariates argument", {
    # Just checking if the argument exists and doesn't crash
    # We won't run full satscan here as we don't have the binary in CI environment usually,
    # but we can mock run_satscan or just rely on writing files test above.

    # Mock run_satscan to avoid strict requirement
    mock_run <- function(...) {
        return(NULL)
    }

    # We need to temporarily assign it in the package namespace or just trust the integration?
    # Integration test:
    expect_error(
        epid_satscan(
            data = data.frame(id = 1, lat = 0, long = 0, cases = 1, pop = 10, age = "A"),
            obs_col = cases,
            pop_col = pop,
            id_col = id,
            lat_col = lat,
            long_col = long,
            covariates = c("age"),
            verbose = FALSE
        ),
        "SatScan path not set" # Expected error showing it got past the covariate checks
    )
})
