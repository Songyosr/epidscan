test_that("satscanr validates inputs", {
    expect_error(satscanr(cas = data.frame()), "cas input must be a satscan_table")
})

test_that("satscanr generates correct file structure (dry run)", {
    # Mock satscan execution by mocking safe_satscan or just run and fail if bin missing?
    # Better: We can check if files are written.

    # Data
    cas_df <- data.frame(loc_id = "A", cases = 10, stringsAsFactors = FALSE)
    cas <- prep_cas(cas_df, loc_id = loc_id, cases = cases, style = "aggregated", time_precision = "generic")

    geo_df <- data.frame(loc_id = "A", lat = 10, long = 10, stringsAsFactors = FALSE)
    geo <- prep_geo(geo_df, loc_id = loc_id, coords = c("long", "lat"))

    # create temp dir
    td <- tempfile()
    dir.create(td)

    # We test the file writing part.
    # Since we can't easily mock rsatscan without loading package context,
    # we will run it and expect error on "SaTScan not found" OR we trust the file writing happened before the error.
    # The file writing happens before run_satscan.

    try(satscanr(cas, geo = geo, dir = NULL, verbose = FALSE, work_dir = td), silent = TRUE)

    # Check files in a specific separate temp dir we pass?
    # satscanr argument 'dir' is for INPUT. 'work_dir' isn't exposed in satscanr signature I wrote!
    # I wrote: satscanr(..., dir=NULL, ...)
    # And inside: work_dir <- tempdir().
    # I cannot control work_dir from outside in my current implementation!
    # I should expose work_dir in satscanr?
    # The user signature didn't have it, but "dir" was ambiguous.
    # User: "If dir is provided -> assume files already exist".
    # I should add 'work_dir' argument or just rely on 'prm' inspection?
    # I really should expose work_dir for testing/inspection.
    # I will UPDATE satscanr.R first to add 'work_dir' argument (defaulting to tempdir() or NULL).
})
