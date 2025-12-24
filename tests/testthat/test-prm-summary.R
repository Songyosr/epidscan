test_that("prm_summarize correctly formats retrospective space-time analysis", {
    prm <- list(
        ModelType = "0", # Discrete Poisson
        AnalysisType = "3", # Retrospective Space-Time
        ScanAreas = "1", # High Rates
        PrecisionCaseTimes = "2", # Month
        StartDate = "2024/01/01",
        EndDate = "2024/12/31",
        SpatialWindowShapeType = "0",
        MaxSpatialSizeInPopulationAtRisk = "50",
        UseMaxCirclePopulationFileOption = "n",
        UseDistanceFromCenterOption = "n",
        MonteCarloReps = "999"
    )

    s <- prm_summarize(prm)

    expect_equal(s$model, "Discrete Poisson")
    expect_equal(s$analysis_type, "Retrospective Space-Time")
    expect_equal(s$scan_areas, "High Rates")
    expect_equal(s$study_period, "2024/01/01 to 2024/12/31")
    expect_equal(s$time_precision, "Month")
    expect_equal(s$spatial_window, "Circular (Max: 50% pop)")
    expect_equal(s$monte_carlo, 999)
})

test_that("prm_summarize handles elliptic window", {
    prm <- list(
        ModelType = "1", # Bernoulli
        AnalysisType = "1", # Purely Spatial
        SpatialWindowShapeType = "1", # Elliptic
        MaxSpatialSizeInPopulationAtRisk = "25",
        UseMaxCirclePopulationFileOption = "n",
        UseDistanceFromCenterOption = "n"
    )

    s <- prm_summarize(prm)

    expect_equal(s$spatial_window, "Elliptic (Max: 25% pop)")
})

test_that("prm_summarize handles missing keys gracefully", {
    prm <- list(ModelType = "0") # Minimal

    s <- prm_summarize(prm)

    expect_equal(s$model, "Discrete Poisson")
    expect_true(is.na(s$analysis_type))
    expect_equal(s$study_period, "? to ?")
})

test_that("prm_summarize handles distance constraint", {
    prm <- list(
        UseDistanceFromCenterOption = "y",
        MaxSpatialSizeInDistanceFromCenter = "100"
    )

    s <- prm_summarize(prm)
    expect_match(s$spatial_window, "Max: 100 dist")
})
