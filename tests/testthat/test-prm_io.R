# Tests for PRM I/O System

test_that("prm_parse works with file path", {
    # Use the template file
    template_path <- system.file("extdata", "template.prm", package = "epidscan")
    skip_if(template_path == "", "Template file not found")

    prm <- prm_parse(template_path)

    expect_s3_class(prm, "prm_list")
    expect_true(length(prm) > 50) # Should have many params
    expect_true("CaseFile" %in% names(prm))
    expect_true("AnalysisType" %in% names(prm))

    # Check attributes
    expect_false(is.null(attr(prm, "skeleton")))
    expect_false(is.null(attr(prm, "sections")))
    expect_false(is.null(attr(prm, "line_map")))
})

test_that("prm_parse works with character vector", {
    lines <- c(
        "[Input]",
        ";test comment",
        "CaseFile=test.cas",
        "ControlFile=",
        "[Analysis]",
        "AnalysisType=1"
    )

    prm <- prm_parse(lines)

    expect_equal(prm$CaseFile, "test.cas")
    expect_equal(prm$ControlFile, "")
    expect_equal(prm$AnalysisType, "1")

    # Check sections
    sections <- attr(prm, "sections")
    expect_equal(sections["CaseFile"], c(CaseFile = "Input"))
    expect_equal(sections["AnalysisType"], c(AnalysisType = "Analysis"))
})

test_that("prm_defaults loads rsatscan defaults", {
    skip_if_not_installed("rsatscan")
    library(rsatscan) # Must be loaded, not just namespace attached

    prm <- prm_defaults()

    expect_s3_class(prm, "prm_list")
    expect_true("CaseFile" %in% names(prm))
    expect_true("AnalysisType" %in% names(prm))
    expect_true("MonteCarloReps" %in% names(prm))

    # Skeleton should start with [Input]
    skeleton <- attr(prm, "skeleton")
    expect_equal(skeleton[1], "[Input]")
})

test_that("prm_set modifies existing parameters", {
    prm <- prm_parse(c(
        "[Input]",
        "CaseFile=old.cas",
        "ControlFile="
    ))

    prm2 <- prm_set(prm, CaseFile = "new.cas")

    expect_equal(prm2$CaseFile, "new.cas")
    expect_equal(prm2$ControlFile, "") # Unchanged
})

test_that("prm_set errors on unknown keys in strict mode", {
    prm <- prm_parse(c("[Input]", "CaseFile=test.cas"))

    expect_error(
        prm_set(prm, FakeParam = "value", .strict = TRUE),
        "not found"
    )
})

test_that("prm_set warns on unknown keys in non-strict mode", {
    prm <- prm_parse(c("[Input]", "CaseFile=test.cas"))

    expect_warning(
        prm2 <- prm_set(prm, FakeParam = "value", .strict = FALSE),
        "not found"
    )

    # Original param unchanged
    expect_equal(prm2$CaseFile, "test.cas")
    # Fake param not added
    expect_false("FakeParam" %in% names(prm2))
})

test_that("prm_add adds new parameter to existing section", {
    prm <- prm_parse(c(
        "[Input]",
        "CaseFile=test.cas",
        "[Analysis]",
        "AnalysisType=1"
    ))

    prm2 <- prm_add(prm, "NewParam", "NewValue", "Input", info = "my new param")

    expect_equal(prm2$NewParam, "NewValue")
    expect_equal(attr(prm2, "sections")["NewParam"], c(NewParam = "Input"))

    # Check skeleton was updated
    skeleton <- attr(prm2, "skeleton")
    expect_true(any(grepl("NewParam=NewValue", skeleton)))
    expect_true(any(grepl(";my new param", skeleton)))
})

test_that("prm_add creates new section if needed", {
    prm <- prm_parse(c("[Input]", "CaseFile=test.cas"))

    prm2 <- prm_add(prm, "CustomKey", "CustomValue", "CustomSection")

    expect_equal(prm2$CustomKey, "CustomValue")

    skeleton <- attr(prm2, "skeleton")
    expect_true(any(skeleton == "[CustomSection]"))
    expect_true(any(grepl("CustomKey=CustomValue", skeleton)))
})

test_that("prm_add errors if key already exists", {
    prm <- prm_parse(c("[Input]", "CaseFile=test.cas"))

    expect_error(
        prm_add(prm, "CaseFile", "new.cas", "Input"),
        "already exists"
    )
})

test_that("prm_write produces valid output via skeleton injection", {
    prm <- prm_parse(c(
        "[Input]",
        ";case file",
        "CaseFile=original.cas",
        "ControlFile=",
        "[Analysis]",
        "AnalysisType=1"
    ))

    # Modify
    prm <- prm_set(prm, CaseFile = "modified.cas", AnalysisType = "3")

    # Write
    tmp <- tempfile(fileext = ".prm")
    prm_write(prm, tmp)

    # Read back
    lines <- readLines(tmp)

    expect_equal(lines[1], "[Input]")
    expect_true(any(grepl("CaseFile=modified.cas", lines)))
    expect_true(any(grepl("AnalysisType=3", lines)))
    # Comment preserved
    expect_true(any(grepl(";case file", lines)))

    unlink(tmp)
})

test_that("round-trip preserves structure", {
    skip_if_not_installed("rsatscan")
    library(rsatscan) # Must be loaded, not just namespace attached

    prm1 <- prm_defaults()

    # Modify
    prm2 <- prm_set(prm1, CaseFile = "roundtrip.cas", AnalysisType = "4")

    # Write
    tmp <- tempfile(fileext = ".prm")
    prm_write(prm2, tmp)

    # Parse again
    prm3 <- prm_parse(tmp)

    expect_equal(prm3$CaseFile, "roundtrip.cas")
    expect_equal(prm3$AnalysisType, "4")

    # Structure preserved
    expect_equal(length(prm3), length(prm1))

    unlink(tmp)
})

# -----------------------------------------------------------------------------
# Version Attribute Tests
# -----------------------------------------------------------------------------

test_that("prm_parse extracts version attribute from [System] section", {
    lines <- c(
        "[Input]",
        "CaseFile=test.cas",
        "[System]",
        "Version=10.3.0"
    )

    prm <- prm_parse(lines)

    expect_equal(attr(prm, "version"), "10.3.0")
})

test_that("prm_parse handles missing version gracefully", {
    lines <- c(
        "[Input]",
        "CaseFile=test.cas"
    )

    prm <- prm_parse(lines)

    expect_null(attr(prm, "version"))
})

test_that("prm_defaults includes version attribute", {
    prm <- prm_defaults("10.3")

    expect_equal(attr(prm, "version"), "10.3.0")
})

# -----------------------------------------------------------------------------
# prm_validate Tests
# -----------------------------------------------------------------------------

test_that("prm_validate returns valid=TRUE for complete template", {
    prm <- prm_defaults("10.3")
    result <- prm_validate(prm)

    expect_true(result$valid)
    expect_equal(length(result$missing), 0)
    expect_equal(length(result$extra), 0)
})

test_that("prm_validate detects missing keys", {
    # Create minimal PRM missing almost all keys
    lines <- c(
        "[Input]",
        "CaseFile=test.cas"
    )

    prm <- prm_parse(lines)
    result <- prm_validate(prm, version = "10.3")

    expect_false(result$valid)
    expect_true(length(result$missing) > 100) # Many missing
    expect_true("AnalysisType" %in% result$missing)
    expect_true("MonteCarloReps" %in% result$missing)
})

test_that("prm_validate detects extra keys", {
    prm <- prm_defaults("10.3")

    # Add a custom key
    prm <- prm_add(prm, "CustomKey", "CustomValue", "Custom")
    result <- prm_validate(prm, version = "10.3")

    # Still valid (extra keys are OK)
    expect_true(result$valid)
    expect_true("CustomKey" %in% result$extra)
})

test_that("prm_validate uses prm version attribute when version=NULL", {
    lines <- c(
        "[Input]",
        "CaseFile=test.cas",
        "[System]",
        "Version=9.7.0"
    )

    prm <- prm_parse(lines)
    result <- prm_validate(prm) # version=NULL, should use 9.7

    expect_equal(result$ref_version, "9.7")
    expect_equal(result$prm_version, "9.7.0")
})
