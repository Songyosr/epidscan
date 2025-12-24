# Test New Object-Oriented Parameter Flow

test_that("prm_options constructor works", {
    # 1. Basic defaults
    p0 <- prm_options()
    expect_s3_class(p0, "satscan_prm")
    expect_true(length(p0) > 50)

    # 2. Overrides
    p1 <- prm_options(AnalysisType = "3", ModelType = "0")
    expect_equal(p1$AnalysisType, "3")
    expect_equal(p1$ModelType, "0")

    # 3. Extending existing
    p2 <- prm_options(base_prm = p1, MonteCarloReps = "1234")
    expect_equal(p2$AnalysisType, "3") # Inherited
    expect_equal(p2$MonteCarloReps, "1234") # New
})

test_that("prm_options handles version string", {
    # Force a specific version
    p <- prm_options(version = "10.3")
    expect_equal(attr(p, "version"), "10.3.0")
})

test_that("prm_options validation works", {
    # Use a custom param that is NOT in defaults to test addition logic
    p <- prm_options(CustomParam = "123")
    expect_equal(p$CustomParam, "123")
})
