test_that("set_satscan_path sets option", {
    # Mock path
    mock_path <- tempdir()

    # Set path
    expect_message(set_satscan_path(mock_path), "SatScan path set to")

    # Check option
    expect_equal(getOption("epidscan.satscan_path"), mock_path)
})

test_that("get_satscan_path retrieves correctly", {
    options(epidscan.satscan_path = "/custom/path")
    expect_equal(get_satscan_path(), "/custom/path")
})

test_that("set_satscan_path warns if file doesn't exist", {
    expect_warning(set_satscan_path("/non/existent/path"), "Path does not exist")
})
