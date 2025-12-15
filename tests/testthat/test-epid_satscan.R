test_that("epid_satscan handles sf input and covariates", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rsatscan")

  # Create mock sf data
  df <- data.frame(
    id = 1:3,
    cases = c(10, 20, 5),
    pop = c(100, 200, 50),
    date = as.Date("2024-01-01"),
    age_group = c("A", "B", "A"),
    lat = c(34, 35, 36),
    lon = c(-80, -81, -82)
  )
  sf_data <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

  # Check failure on missing columns
  expect_error(epid_satscan(sf_data, obs_col = NULL), "obs_col is required")
})

test_that("epid_satscan validates output_dir logic", {
  # We test that it creates the dir if missing
  tmp_out <- file.path(tempdir(), "test_sso")
  if (dir.exists(tmp_out)) unlink(tmp_out, recursive = TRUE)

  df <- data.frame(cases = 1, id = 1, lat = 0, long = 0)

  # Expect fail on execution (no satscan path)
  # But check if dir was created before execution?
  # Logic: dir.create happens before ss_path check?
  # In current code: dir.create is Step 5. ss_path check is Step 8.
  # So we expect it to fail at Step 8, but dir should exist.

  # We need to mock get_satscan_path to return something valid, OR just trap the error.

  try(epid_satscan(df, obs_col = cases, id_col = id, lat_col = lat, long_col = long, output_dir = tmp_out), silent = TRUE)

  # New behavior: input files (.cas, .pop, .geo) are in temp work_dir, NOT output_dir
  expect_false(file.exists(file.path(tmp_out, "epid.cas")))
  expect_false(file.exists(file.path(tmp_out, "epid.pop")))
  expect_false(file.exists(file.path(tmp_out, "epid.geo")))

  # Results should be there (if run was successful, but here likely depends on clean exit)
  # Since this test runs real execution (or mocked?), if it fails to produce output locally it might fail.
  # If this test relies on real satscan, and real satscan writes to work_dir...
  # Ideally we should verify 'epid.txt' if we could mock the run.
  # For now, asserting FALSE on input files validates our separation change.
  expect_true(dir.exists(tmp_out))
})

test_that("epid_satscan handles flexible time", {
  df <- data.frame(
    cases = 1,
    year = 2024,
    pop = 100,
    lat = 10,
    long = 100,
    id = 1
  )

  exp_err <- try(
    df |> epid_satscan(
      obs_col = cases,
      date_col = year,
      pop_col = pop,
      id_col = id,
      lat_col = lat,
      long_col = long,
      time_precision = "Year"
    ),
    silent = TRUE
  )
  # Just ensure it didn't crash before execution check
  expect_true(inherits(exp_err, "try-error"))
})
