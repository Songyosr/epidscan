# Tests for map_clusters()

# =============================================================================
# HELPER FUNCTIONS FOR TESTS
# =============================================================================

create_mock_result <- function(lat_col = "lat", lon_col = "lon") {
    locs <- data.frame(
        LOC_ID = c("A", "B", "C"),
        CLUSTER = c(1, 1, NA)
    )
    locs[[lat_col]] <- c(10, 11, 12)
    locs[[lon_col]] <- c(20, 21, 22)

    result <- list(
        clusters = data.frame(
            CLUSTER = 1,
            P_VALUE = 0.01,
            OBSERVED = 100,
            EXPECTED = 50,
            CLU_RR = 2.0,
            RADIUS = 10
        ),
        locations = locs
    )
    class(result) <- "satscan_result"
    result
}

create_mock_result_with_pvalues <- function() {
    result <- list(
        clusters = data.frame(
            CLUSTER = 1:3,
            P_VALUE = c(0.001, 0.04, 0.2), # One very sig, one sig, one not
            RADIUS = 10
        ),
        locations = data.frame(
            LOC_ID = c("A", "B", "C", "D", "E"),
            lat = c(10, 11, 12, 20, 21),
            lon = c(30, 31, 32, 40, 41),
            CLUSTER = c(1, 1, 2, 2, 3)
        )
    )
    class(result) <- "satscan_result"
    result
}

# =============================================================================
# TESTS
# =============================================================================

test_that("map_clusters requires leaflet package", {
    skip_if_not_installed("leaflet")
    result <- create_mock_result()
    expect_s3_class(map_clusters(result), "leaflet")
})

test_that("map_clusters validates input object", {
    expect_error(
        map_clusters(list(x = 1)),
        "must be a 'satscan_result'"
    )

    # Empty locations
    empty_result <- list(
        clusters = data.frame(),
        locations = data.frame()
    )
    class(empty_result) <- "satscan_result"

    expect_error(
        map_clusters(empty_result),
        "No location data"
    )
})

test_that("map_clusters detects coordinate columns", {
    skip_if_not_installed("leaflet")

    # Test with standard lat/lon names
    result <- create_mock_result(lat_col = "lat", lon_col = "lon")
    m <- map_clusters(result)
    expect_s3_class(m, "leaflet")

    # Test with alternate names
    result2 <- create_mock_result(lat_col = "latitude", lon_col = "longitude")
    m2 <- map_clusters(result2)
    expect_s3_class(m2, "leaflet")

    # Test with x/y
    result3 <- create_mock_result(lat_col = "y", lon_col = "x")
    m3 <- map_clusters(result3)
    expect_s3_class(m3, "leaflet")
})

test_that("map_clusters filters by significance", {
    skip_if_not_installed("leaflet")

    result <- create_mock_result_with_pvalues()

    # All clusters
    m1 <- map_clusters(result, significance_only = FALSE)
    expect_s3_class(m1, "leaflet")

    # Only significant
    expect_warning(
        m2 <- map_clusters(result, significance_only = TRUE),
        NA # Should not warn if significant clusters exist
    )
    expect_s3_class(m2, "leaflet")
})

test_that("map_clusters handles missing cluster coordinates gracefully", {
    skip_if_not_installed("leaflet")

    # Result with clusters but no coordinates in cluster table
    result <- list(
        clusters = data.frame(
            CLUSTER = 1:2,
            P_VALUE = c(0.001, 0.05),
            RADIUS = 10
        ),
        locations = data.frame(
            LOC_ID = c("A", "B", "C"),
            lat = c(10, 11, 12),
            lon = c(20, 21, 22),
            CLUSTER = c(1, 1, 2)
        )
    )
    class(result) <- "satscan_result"

    # Should calculate centroids from locations
    m <- map_clusters(result)
    expect_s3_class(m, "leaflet")
})

test_that("map_clusters customization options work", {
    skip_if_not_installed("leaflet")

    result <- create_mock_result()

    # Custom provider
    m1 <- map_clusters(result, provider = "OpenStreetMap")
    expect_s3_class(m1, "leaflet")

    # Custom colors
    m2 <- map_clusters(
        result,
        cluster_opacity = 0.8,
        location_color = "#ff0000"
    )
    expect_s3_class(m2, "leaflet")

    # Without locations
    m3 <- map_clusters(result, show_locations = FALSE)
    expect_s3_class(m3, "leaflet")
})

test_that("map_clusters popup_vars works", {
    skip_if_not_installed("leaflet")

    result <- create_mock_result()
    result$locations$population <- c(1000, 2000, 3000)
    result$locations$cases <- c(10, 20, 30)

    m <- map_clusters(result, popup_vars = c("population", "cases"))
    expect_s3_class(m, "leaflet")
})

test_that("detect_coordinates handles various column names", {
    # Test lowercase
    df1 <- data.frame(lat = 1, lon = 2)
    coords <- detect_coordinates(df1)
    expect_equal(coords$lat, "lat")
    expect_equal(coords$lon, "lon")

    # Test full names
    df2 <- data.frame(latitude = 1, longitude = 2)
    coords2 <- detect_coordinates(df2)
    expect_equal(coords2$lat, "latitude")
    expect_equal(coords2$lon, "longitude")

    # Test x/y
    df3 <- data.frame(y = 1, x = 2)
    coords3 <- detect_coordinates(df3)
    expect_equal(coords3$lat, "y")
    expect_equal(coords3$lon, "x")

    # Test semantic names (ss_lat/ss_long)
    df4 <- data.frame(ss_lat = 1, ss_long = 2)
    coords4 <- detect_coordinates(df4)
    expect_equal(coords4$lat, "ss_lat")
    expect_equal(coords4$lon, "ss_long")
})

test_that("map_clusters handles RR coloring and temporal popups", {
    skip_if_not_installed("leaflet")

    result <- create_mock_result()
    # Add RR and dates
    result$clusters$CLU_RR <- 5.5
    result$clusters$START_DATE <- as.Date("2020-01-01")
    result$clusters$END_DATE <- as.Date("2020-01-31")

    m <- map_clusters(result)
    expect_s3_class(m, "leaflet")

    # Check if popup text contains the date and RR
    # (Internal check of the prepare_cluster_data helper)
    cluster_data <- prepare_cluster_data(result, FALSE, list(lat = "lat", lon = "lon"))
    expect_match(cluster_data$popup, "Relative Risk: 5.50")
    expect_match(cluster_data$popup, "Period: 2020-01-01 to 2020-01-31")

    # Check if colors are assigned
    colors <- assign_cluster_colors(cluster_data, c("red", "blue"))
    expect_true("fill_color" %in% names(colors))
    expect_true("stroke_color" %in% names(colors))
})
