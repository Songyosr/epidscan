test_that("prep_grd acts as wrapper around prep_geo", {
    df <- data.frame(id = "G1", x = 1, y = 2)
    res <- prep_grd(df, loc_id = id, coords = c("x", "y"))

    expect_equal(res$kind, "grd")
    expect_equal(res$data$coord1, 2) # Y/Lat
    expect_equal(res$data$coord2, 1) # X/Long
})
