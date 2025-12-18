library(testthat)
library(epidscan)

# Mocking run_satscan to avoid binary execution
# We will use testthat::mockery or similar if available, but simple function masking works in testthat scope usually?
# Actually, we can just ensure set_satscan_path is set to something dummy and catch the error or mock run_satscan.

test_that("satscanr accepts ss_tbl inputs and generates correct files", {
    # 1. Setup Data
    cas_df <- data.frame(id = "A", cases = 10, date = as.Date("2021-01-01"))
    geo_df <- data.frame(id = "A", lat = 10, long = 20)

    ss_cas <- as_satscan_case(cas_df, loc_id = "id", cases = "cases", time = "date", time_precision = "month")
    ss_geo <- as_satscan_coordinates(geo_df, loc_id = "id", coord1 = "lat", coord2 = "long", coord_type = "latlong")

    # 2. Mocking
    # We'll use a temporary directory for output
    out_dir <- tempfile()
    dir.create(out_dir)

    # Mock run_satscan to prevent actual execution and just return a dummy result
    # We need to mock 'run_satscan' and 'get_satscan_path'
    # Since we are testing integration within the package, we might need 'mockery' or 'local_mocked_bindings'

    # Alternative: We can inspect the intermediate files in 'work_dir'.
    # But satscanr creates work_dir internally via tempdir().
    # We can't easily peek inside unless we mock.

    # Let's try mocking with testthat::local_mocked_bindings
    # Note: this requires the function to be exported presumably or internal?
    # 'run_satscan' is internal (or we assume). It might be exported if used by others.
    # Let's check: in satscan_runner.R it doesn't seem to be exported in the snippet?
    # Wait, usually runner is internal?
    # Let's check R/satscan_runner.R.
    # Actually we can mock `run_satscan` which is called by `satscanr`.

    mock_run <- function(...) {
        return(list(
            col = data.frame(loc_id = "A", cluster = 1),
            rr = data.frame(cluster = 1, rel_risk = 2)
        ))
    }

    mock_get_path <- function(...) "/usr/bin/satscan"
    mock_macos_path <- function(...) list(ss_location = "/usr/bin/satscan", ss_batch = "/usr/bin/SaTScanBatch")

    local_mocked_bindings(
        run_satscan = mock_run,
        get_satscan_path = mock_get_path,
        get_macos_satscan_path = mock_macos_path,
        .package = "epidscan"
    )

    # 3. Execution
    # We pass verbose=TRUE to see messages if needed
    # We need to catch the "work_dir" if we want to verify the files?
    # satscanr returns 'res' which contains 'work_dir'.

    res <- satscanr(
        cas = ss_cas,
        geo = ss_geo,
        prm_path = NULL,
        verbose = FALSE
    )

    # 4. Verification
    expect_true(!is.null(res$work_dir))
    expect_true(dir.exists(res$work_dir))

    # Check generated files
    f_cas <- file.path(res$work_dir, "epid.cas")
    f_geo <- file.path(res$work_dir, "epid.geo")
    f_prm <- file.path(res$work_dir, "epid.prm")

    expect_true(file.exists(f_cas))
    expect_true(file.exists(f_geo))
    expect_true(file.exists(f_prm))

    # Check content of CAS (should be formatted)
    cas_content <- read.table(f_cas)
    # ID(A), Cases(10), Time(2021/01 because spec=month)
    expect_equal(as.character(cas_content[, 3]), "2021/01")

    # Check PRM settings derived from ss_tbl
    prm_content <- readLines(f_prm)
    # Check CoordinatesType=1 (LatLong)
    expect_true(any(grepl("^CoordinatesType=1", prm_content)))
    # Check PrecisionCaseTimes=2 (Month)
    expect_true(any(grepl("^PrecisionCaseTimes=2", prm_content)))
})

test_that("satscanr rejects mismatched ss_tbl types", {
    cas_df <- data.frame(id = "A", cases = 10)
    ss_bad <- as_satscan_coordinates(cas_df, loc_id = "id", coord1 = "cases", coord2 = "cases")

    expect_error(
        satscanr(cas = ss_bad, geo = ss_bad),
        "cas input must be a satscan_table of kind 'cas' or ss_tbl of type 'cas'"
    )
})
