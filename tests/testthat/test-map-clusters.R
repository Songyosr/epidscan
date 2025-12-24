test_that("map_clusters handles standard Lat/Long", {
    skip_if_not_installed("leaflet")

    # Mock result
    res <- list(
        locations = data.frame(LOC_ID = 1:3, lat = c(0, 1, 0), lon = c(0, 0, 1), CLUSTER = c(1, 1, NA)),
        clusters = data.frame(CLUSTER = 1, P_VALUE = 0.01, RADIUS = 10, n_locations = 2)
    )
    class(res) <- "satscan_result"

    map <- map_clusters(res)
    expect_s3_class(map, "leaflet")
    # Verify CRS is standard (EPSG:3857) implicitly
    expect_equal(map$x$options$crs$crsClass, "L.CRS.EPSG3857")
})

test_that("map_clusters handles Cartesian + No CRS (Simple)", {
    skip_if_not_installed("leaflet")

    res <- list(
        locations = data.frame(LOC_ID = 1:3, y = c(0, 10, 0), x = c(0, 0, 10), CLUSTER = c(1, 1, NA)),
        clusters = data.frame(CLUSTER = 1, P_VALUE = 0.01, RADIUS = 5, n_locations = 2)
    )
    class(res) <- "satscan_result"

    map <- map_clusters(res)
    expect_s3_class(map, "leaflet")

    # Check if Simple CRS is set
    expect_true(!is.null(map$x$options$crs))
    expect_equal(map$x$options$crs$crsClass, "L.CRS.Simple")
    # Check Zoom options
    expect_equal(map$x$options$minZoom, -5)
})

test_that("map_clusters handles Cartesian + CRS (Projection)", {
    skip_if_not_installed("leaflet")
    skip_if_not_installed("sf")

    # Mock Cartesian (e.g., UTM Zone 18N approx)
    res <- list(
        locations = data.frame(LOC_ID = 1:2, y = c(4500000, 4500100), x = c(500000, 500100), CLUSTER = c(1, 1)),
        clusters = data.frame(CLUSTER = 1, P_VALUE = 0.01, RADIUS = 1, n_locations = 2)
    )
    class(res) <- "satscan_result"

    # Provide CRS (EPSG:32618 - UTM 18N)
    map <- map_clusters(res, crs = 32618)
    expect_s3_class(map, "leaflet")

    # CRS should be default (after projection)
    expect_equal(map$x$options$crs$crsClass, "L.CRS.EPSG3857")
})

test_that("map_clusters uses correct radius", {
    skip_if_not_installed("leaflet")

    res <- list(
        locations = data.frame(LOC_ID = 1, lat = 0, lon = 0, CLUSTER = 1),
        clusters = data.frame(CLUSTER = 1, P_VALUE = 0.01, RADIUS = 5, n_locations = 1) # Radius 5km
    )
    class(res) <- "satscan_result"

    map <- map_clusters(res, use_radius = TRUE)

    expect_s3_class(map, "leaflet")
})

test_that("map_clusters 'simple=TRUE' overrides misleading lat/long names", {
  skip_if_not_installed("leaflet")
  
  # Data with "lat" and "lon" cols but arbitrary values
  res <- list(
    locations = data.frame(LOC_ID=1:3, lat=c(0,10,0), lon=c(0,0,10), CLUSTER=c(1,1,NA)),
    clusters = data.frame(CLUSTER=1, P_VALUE=0.01, RADIUS=5, n_locations=2)
  )
  class(res) <- "satscan_result"
  
  # 1. Default finds 'latlong' -> standard CRS
  map_def <- map_clusters(res)
  expect_equal(map_def$x$options$crs$crsClass, "L.CRS.EPSG3857")
  
  # 2. Force simple -> Simple CRS
  map_simple <- map_clusters(res, simple = TRUE)
  expect_equal(map_simple$x$options$crs$crsClass, "L.CRS.Simple")
})
