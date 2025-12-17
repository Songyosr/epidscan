test_that("read_prm parses legacy PRM correctly", {
    prm_path <- system.file("legacy/example/epid.prm", package = "epidscan")
    if (prm_path == "") {
        # Fallback to absolute path for dev env if package not installed
        prm_path <- "/Users/tonn/Dev/epidscan/legacy/example/epid.prm"
    }
    skip_if_not(file.exists(prm_path), "Legacy PRM not found")

    prm <- read_prm(prm_path)

    expect_type(prm, "list")

    # Check specific known values from epid.prm
    expect_equal(prm$CaseFile, "epid.cas")
    expect_equal(prm$AnalysisType, "3") # Read as string
    expect_equal(prm$ModelType, "0")
    expect_equal(prm$StartDate, "2024/01/01")
})

test_that("read_prm handles comments and sections", {
    # Create temp prm
    f <- tempfile(fileext = ".prm")
    writeLines(c(
        "[Input]",
        "; This is a comment",
        "CaseFile=test.cas",
        "  Start Date = 2020/01/01  ", # Spaces around keys/values
        "[Analysis]",
        "AnalysisType=1"
    ), f)

    prm <- read_prm(f)
    expect_equal(prm$CaseFile, "test.cas")
    expect_equal(prm$`Start Date`, "2020/01/01") # Trimmed
    expect_equal(prm$AnalysisType, "1")
    expect_false("Input" %in% names(prm)) # Sections ignored
})
