test_that("epid_satscan separates work and output directories", {
    data <- data.frame(
        id = 1:5,
        cases = c(0, 1, 0, 1, 0),
        lat = 0,
        long = 0
    )

    # Create temp dirs
    my_work <- tempfile("work")
    my_out <- tempfile("out")
    dir.create(my_work)
    dir.create(my_out)

    # Mock run_satscan to avoid needing actual SatScan binary for directory logic test?
    # The copy logic happens AFTER run_satscan returns non-null.
    # So we probably need to mock run_satscan to return a dummy list and Create dummy files.

    # For now, let's rely on mocking in the test file or just checking directory creation if we stub execution.
    # Actually, `epid_satscan` calls `run_satscan`. We can mock `run_satscan` using `testthat::mock`.

    local_mocked_bindings(
        run_satscan = function(work_dir, ...) {
            # Simulate creation of result files in work_dir
            file.create(file.path(work_dir, "epid.txt"))
            file.create(file.path(work_dir, "epid.shp"))
            return(list(p_val = 0.05)) # Minimal return to pass null check
        },
        .package = "epidscan"
    )

    # We also need to mock ss_options inside build_satscan_options or just let it run (it writes invalid PRM but we mock execution).
    # `get_satscan_path` check might fail if not set.
    local_mocked_bindings(
        get_satscan_path = function() "/mock/path",
        get_macos_satscan_path = function(...) list(ss_location = "/mock/bin", ss_batch = "/mock/batch"),
        .package = "epidscan"
    )

    res <- epid_satscan(
        data,
        obs_col = cases,
        lat_col = lat, long_col = long,
        work_dir = my_work,
        output_dir = my_out,
        verbose = FALSE
    )

    # Check validation
    expect_true(file.exists(file.path(my_work, "epid.cas"))) # Input created in work
    expect_true(file.exists(file.path(my_work, "epid.txt"))) # Result created in work (simulated)

    expect_true(file.exists(file.path(my_out, "epid.txt"))) # Copied to output
    expect_true(file.exists(file.path(my_out, "epid.shp"))) # Copied to output
    expect_false(file.exists(file.path(my_out, "epid.cas"))) # Input NOT copied
})
