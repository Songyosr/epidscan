test_that("pipeline functions exist", {
  expect_true(exists("run_pipeline"))
  expect_true(exists("process_geo"))
  expect_true(exists("process_population"))
  expect_true(exists("process_cases"))
})

test_that("satscan configuration works", {
  params <- create_satscan_params(
    start_date = "2025/01/01",
    end_date = "2025/12/31"
  )
  expect_type(params, "list")
  expect_equal(params$StartDate, "2025/01/01")
})

test_that("satscan path can be set", {
  # Mock path
  mock_path <- tempfile()
  file.create(mock_path)
  
  expect_message(set_satscan_path(mock_path), "SatScan path set to")
  expect_equal(get_satscan_path(), mock_path)
})
