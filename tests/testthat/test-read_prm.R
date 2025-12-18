test_that("read_prm parses PRM correctly", {
    # Use bundled template file
    prm_path <- system.file("extdata/prm_template/v10_3.prm", package = "epidscan")
    skip_if_not(file.exists(prm_path), "Template PRM not found")

    prm <- read_prm(prm_path)

    expect_type(prm, "list")

    # Check expected keys exist
    expect_true("AnalysisType" %in% names(prm))
    expect_true("ModelType" %in% names(prm))
    expect_true("MonteCarloReps" %in% names(prm))
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
