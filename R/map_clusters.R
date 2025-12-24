#' Interactive Map of SaTScan Clusters
#'
#' @description
#' Creates an interactive leaflet map displaying detected spatial clusters and
#' their member locations. Clusters are shown as circles with size proportional
#' to the number of locations, colored by statistical significance.
#'
#' @param x A `satscan_result` object from `satscanr()`.
#' @param significance_only Logical. If `TRUE`, only display clusters with
#'   p-value < 0.05. Default is `FALSE` (show all clusters).
#' @param provider Character. Leaflet tile provider. Options include
#'   `"OpenStreetMap"`, `"CartoDB.Positron"`, `"Esri.WorldTopoMap"`, etc.
#'   See \url{https://leaflet-extras.github.io/leaflet-providers/preview/}
#'   for full list. Default is `"CartoDB.Positron"`.
#' @param cluster_opacity Numeric between 0 and 1. Opacity of cluster circles.
#'   Default is 0.6.
#' @param color_palette Character vector of colors for cluster significance levels.
#'   Default is `c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850")`
#'   (red to green spectrum).
#' @param location_color Character. Color for individual location markers.
#'   Default is `"#4575b4"` (blue).
#' @param show_locations Logical. If `TRUE`, show individual location points
#'   in addition to cluster boundaries. Default is `TRUE`.
#' @param popup_vars Character vector. Additional variables from `$locations`
#'   to include in location popups. Default is `NULL`.
#' @param crs Optional. CRS for projecting Cartesian coordinates (e.g., 32618).
#'   If provided, coordinates are projected to WGS84 for standard mapping.
#' @param use_radius Logical. If `TRUE` (default), use the physical radius from
#'   SaTScan results for circle size. If `FALSE`, size by number of locations.
#' @param simple Logical. If `TRUE`, force a simple coordinate system (pixels)
#'   regardless of data type. Useful for debugging or non-geographic scans.
#' @param verbose Logical. If `TRUE`, show progress messages. Default is `FALSE`.
#' @param ... Additional arguments passed to `leaflet::addCircles()` for clusters.
#'
#' @return A `leaflet` map object (htmlwidget) that can be printed to display
#'   or saved with `htmlwidgets::saveWidget()`.
#'
#' @details
#' The function requires the `leaflet` package to be installed. If not available,
#' an informative error message is displayed.
#'
#' **Map Layers:**
#' - **Cluster circles**: Sized by number of member locations. Fill color
#'   represents Relative Risk (RR). Border color and thickness indicate significance.
#' - **Location points**: Individual locations within clusters (if `show_locations = TRUE`)
#' - **Popups**: Click on clusters or locations for detailed information.
#'   Cluster popups include counts, RR, and time period (if available).
#'
#' **Color Scheme:**
#' - **Fill**: Mapped to Relative Risk (`CLU_RR`) using the provided `color_palette`.
#'   If RR is unavailable, falls back to p-value coloring.
#' - **Border (Stroke)**: Red and bold for significant clusters (p < 0.05).
#'   Gray and thin for non-significant clusters.
#'
#' **Coordinate Detection:**
#' The function automatically detects coordinate columns from the locations table:
#' - Latitude: `lat`, `latitude`, `y`, `LATITUDE`
#' - Longitude: `lon`, `long`, `longitude`, `x`, `LONGITUDE`
#'
#' @export
#' @export
map_clusters <- function(x,
                         significance_only = FALSE,
                         provider = "CartoDB.Positron",
                         cluster_opacity = 0.6,
                         color_palette = c(
                           "#d73027", "#fc8d59", "#fee08b",
                           "#d9ef8b", "#91cf60", "#1a9850"
                         ),
                         location_color = "#4575b4",
                         show_locations = TRUE,
                         popup_vars = NULL,
                         verbose = FALSE,
                         crs = NULL,
                         use_radius = TRUE,
                         simple = FALSE,
                         ...) {
  # =========================================================================
  # PACKAGE CHECK
  # =========================================================================
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive mapping.", call. = FALSE)
  }

  if (!is.null(crs) && !requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required when providing 'crs'.", call. = FALSE)
  }

  # =========================================================================
  # INPUT VALIDATION
  # =========================================================================
  if (!inherits(x, "satscan_result")) {
    stop("x must be a 'satscan_result' object from satscanr()", call. = FALSE)
  }

  if (is.null(x$locations) || nrow(x$locations) == 0) {
    stop("No location data available in result object", call. = FALSE)
  }

  # =========================================================================
  # 3. DETECT & TRANSFORM COORDINATES
  # =========================================================================
  # Detect coords from locations table (using metadata if available)
  summary_info <- attr(x, "summary_info")
  coord_info <- detect_coordinates(x$locations, summary_info)

  # Determine Leaflet CRS mode
  # If Lat/Long: Standard EPSG:3857/4326 logic (Default)
  # If Cartesian + CRS provided: Project to 4326
  # If Cartesian + No CRS: Use CRS.Simple

  leaflet_crs <- leaflet::leafletCRS(crsClass = "L.CRS.EPSG3857") # Default
  is_simple <- FALSE

  locations_mapped <- x$locations

  # Determine if Cartesian based on detection OR metadata
  is_cartesian <- (coord_info$type == "cartesian")
  if (!is.null(summary_info$coord_type_code) && summary_info$coord_type_code == 0) {
    is_cartesian <- TRUE
  }

  if (simple || is_cartesian) {
    if (!is.null(crs) && !simple) {
      # Project to Lat/Lon using sf (Only if NOT forced simple)
      if (verbose) message("Projecting Cartesian coordinates to Lat/Long using CRS: ", crs)

      # Create temporary sf object
      loc_sf <- sf::st_as_sf(
        locations_mapped,
        coords = c(coord_info$lon, coord_info$lat),
        crs = crs
      )

      # Transform to WGS84
      loc_sf <- sf::st_transform(loc_sf, 4326)

      # Extract transformed coords
      coords_mat <- sf::st_coordinates(loc_sf)
      locations_mapped[[coord_info$lon]] <- coords_mat[, 1]
      locations_mapped[[coord_info$lat]] <- coords_mat[, 2]

      # Treat as lat/lon now
      coord_info$type <- "latlong"
    } else {
      # Stay Cartesian -> Simple CRS
      # Either natively cartesian OR forced simple (e.g. unknown x/y named as lat/long)
      if (verbose) message("Using Simple CRS (pixels) for blank map.")
      leaflet_crs <- leaflet::leafletCRS(crsClass = "L.CRS.Simple")
      is_simple <- TRUE
    }
  }

  # =========================================================================
  # 4. PREPARE DATA
  # =========================================================================
  # Prepare clusters (with updated locations_mapped)
  cluster_data <- prepare_cluster_data(
    x = list(clusters = x$clusters, locations = locations_mapped), # Pass updated locs
    significance_only = significance_only,
    coords = coord_info
  )

  # Prepare locations
  location_data <- prepare_location_data(
    x = list(locations = locations_mapped), # Pass updated locs
    coords = coord_info,
    popup_vars = popup_vars
  )

  # =========================================================================
  # 5. BUILD MAP
  # =========================================================================
  # Initial map options

  if (is_simple) {
    # Use Simple CRS (Blank Map)
    # set min/max zoom to allow zooming in on pixels
    m <- leaflet::leaflet(options = leaflet::leafletOptions(
      crs = leaflet_crs,
      minZoom = -5,
      maxZoom = 5
    ))

    if (verbose) message("Rendering Cartesian map (no tiles).")
  } else {
    # Standard Map
    m <- leaflet::leaflet(options = leaflet::leafletOptions(crs = leaflet_crs))
    m <- m |> leaflet::addProviderTiles(provider)
  }

  # Add cluster circles
  if (!is.null(cluster_data) && nrow(cluster_data) > 0) {
    m <- add_cluster_circles(
      map = m,
      cluster_data = cluster_data,
      color_palette = color_palette,
      opacity = cluster_opacity,
      use_radius = use_radius,
      is_simple = is_simple,
      ...
    )
  }

  # Add location points
  if (show_locations && !is.null(location_data) && nrow(location_data) > 0) {
    m <- add_location_points(
      map = m,
      location_data = location_data,
      color = location_color,
      coords = coord_info
    )
  }

  # Add legend
  if (!is.null(cluster_data) && nrow(cluster_data) > 0) {
    m <- add_cluster_legend(m, cluster_data, color_palette)
  }

  # For Simple/Cartesian, fit bounds
  if (is_simple) {
    m <- leaflet::fitBounds(
      m,
      min(locations_mapped[[coord_info$lon]]), min(locations_mapped[[coord_info$lat]]),
      max(locations_mapped[[coord_info$lon]]), max(locations_mapped[[coord_info$lat]])
    )
  }

  m
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Detect Coordinate Columns
#'
#' @param df Data frame with location coordinates
#' @param meta Optional summary_info list with 'coord_cols' and 'coord_type_code'
#' @return List with lat_col, lon_col, and type
#' @keywords internal
detect_coordinates <- function(df, meta = NULL) {
  # 1. Check Metadata first (Highest Priority)
  if (!is.null(meta$coord_cols)) {
    x_c <- meta$coord_cols$x
    y_c <- meta$coord_cols$y

    if (!is.null(x_c) && !is.null(y_c) && x_c %in% names(df) && y_c %in% names(df)) {
      # Determine type from metadata code (0=Cartesian, 1=LatLong)
      type <- "latlong"
      if (!is.null(meta$coord_type_code) && meta$coord_type_code == 0) {
        type <- "cartesian"
      }
      return(list(lat = y_c, lon = x_c, type = type))
    }
  }

  # 2. Heuristic Detection
  # Priority: 1. ss_tbl roles, 2. semantic names, 3. common names
  candidates <- list(
    lat = c("ss_lat", "lat", "latitude", "LATITUDE", "Latitude"),
    lon = c("ss_long", "ss_lon", "lon", "long", "longitude", "LONGITUDE", "Longitude"),
    y = c("y", "Y", "coord_y", "y_km"),
    x = c("x", "X", "coord_x", "x_km")
  )

  find_col <- function(choices) {
    for (c in choices) {
      if (c %in% names(df)) {
        return(c)
      }
    }
    NULL
  }

  # Check Lat/Long first
  lat_col <- find_col(candidates$lat)
  lon_col <- find_col(candidates$lon)

  if (!is.null(lat_col) && !is.null(lon_col)) {
    return(list(lat = lat_col, lon = lon_col, type = "latlong"))
  }

  # Check Cartesian X/Y
  y_col <- find_col(candidates$y)
  x_col <- find_col(candidates$x)

  if (!is.null(y_col) && !is.null(x_col)) {
    return(list(lat = y_col, lon = x_col, type = "cartesian"))
  }

  stop("Could not detect spatial coordinates (lat/long or x/y) in locations table.", call. = FALSE)
}


#' Prepare Cluster Data for Mapping
#'
#' @param x satscan_result object
#' @param significance_only Filter for p < 0.05?
#' @param coords List with lat and lon column names
#' @return Data frame ready for mapping
#' @keywords internal
prepare_cluster_data <- function(x, significance_only, coords) {
  if (is.null(x$clusters) || nrow(x$clusters) == 0) {
    return(NULL)
  }

  clusters <- x$clusters

  # Filter by significance
  if (significance_only && "P_VALUE" %in% names(clusters)) {
    clusters <- clusters[clusters$P_VALUE < 0.05, ]
    if (nrow(clusters) == 0) {
      return(NULL)
    }
  }

  # Calculate centroids from locations (most robust)
  # This avoids issues if the SaTScan output table lacks coordinates
  if (!is.null(x$locations) && "CLUSTER" %in% names(x$locations)) {
    cluster_stats <- x$locations |>
      dplyr::filter(!is.na(CLUSTER)) |>
      dplyr::group_by(CLUSTER) |>
      dplyr::summarise(
        n_locations = dplyr::n(),
        centroid_lat = mean(!!rlang::sym(coords$lat), na.rm = TRUE),
        centroid_lon = mean(!!rlang::sym(coords$lon), na.rm = TRUE),
        .groups = "drop"
      )

    # Careful merge to preserve types
    clusters$CLUSTER <- as.character(clusters$CLUSTER)
    cluster_stats$CLUSTER <- as.character(cluster_stats$CLUSTER)

    clusters <- merge(clusters, cluster_stats, by = "CLUSTER", all.x = TRUE)
  }

  # Create popup
  clusters$popup <- create_cluster_popup(clusters)

  clusters
}


#' Create Cluster Popup HTML
#'
#' @param clusters Cluster data frame
#' @return Character vector of HTML popup text
#' @keywords internal
create_cluster_popup <- function(clusters) {
  popups <- character(nrow(clusters))

  for (i in seq_len(nrow(clusters))) {
    row <- clusters[i, ]
    lines <- c(
      sprintf("<b>Cluster %s</b>", row$CLUSTER)
    )

    if ("P_VALUE" %in% names(row)) {
      lines <- c(lines, sprintf("P-value: %.4f", row$P_VALUE))
    }
    if ("n_locations" %in% names(row)) {
      lines <- c(lines, sprintf("Locations: %d", row$n_locations))
    }
    if ("RADIUS" %in% names(row)) {
      lines <- c(lines, sprintf("Radius: %.2f km", row$RADIUS))
    }
    if ("CLU_RR" %in% names(row)) {
      lines <- c(lines, sprintf("Relative Risk: %.2f", row$CLU_RR))
    }
    if (all(c("START_DATE", "END_DATE") %in% names(row))) {
      lines <- c(lines, sprintf("Period: %s to %s", row$START_DATE, row$END_DATE))
    }
    if ("OBSERVED" %in% names(row)) {
      lines <- c(lines, sprintf("Observed: %d", row$OBSERVED))
    }
    if ("EXPECTED" %in% names(row)) {
      lines <- c(lines, sprintf("Expected: %.1f", row$EXPECTED))
    }

    popups[i] <- paste(lines, collapse = "<br/>")
  }

  popups
}


#' Prepare Location Data for Mapping
#'
#' @param x satscan_result object
#' @param coords List with lat and lon column names
#' @param popup_vars Additional variables for popups
#' @return Data frame ready for mapping
#' @keywords internal
prepare_location_data <- function(x, coords, popup_vars) {
  if (is.null(x$locations)) {
    return(NULL)
  }

  locs <- x$locations

  # Only show locations in clusters
  if ("CLUSTER" %in% names(locs)) {
    locs <- locs[!is.na(locs$CLUSTER), ]
  }

  if (nrow(locs) == 0) {
    return(NULL)
  }

  # Create popup
  locs$popup <- create_location_popup(locs, popup_vars)

  locs
}

#' Create Location Popup HTML
#' @keywords internal
create_location_popup <- function(locs, popup_vars) {
  popups <- character(nrow(locs))
  for (i in seq_len(nrow(locs))) {
    row <- locs[i, ]
    lines <- character(0)
    if ("LOC_ID" %in% names(row)) lines <- c(lines, sprintf("<b>%s</b>", row$LOC_ID))
    if ("CLUSTER" %in% names(row)) lines <- c(lines, sprintf("Cluster: %s", row$CLUSTER))
    if ("REL_RISK" %in% names(row)) lines <- c(lines, sprintf("Relative Risk: %.2f", row$REL_RISK))

    if (!is.null(popup_vars)) {
      for (var in popup_vars) {
        if (var %in% names(row)) lines <- c(lines, sprintf("%s: %s", var, row[[var]]))
      }
    }
    popups[i] <- paste(lines, collapse = "<br/>")
  }
  popups
}

#' Add Cluster Circles to Map
#' @keywords internal
add_cluster_circles <- function(map, cluster_data, color_palette, opacity, use_radius = TRUE, is_simple = FALSE, ...) {
  cluster_data <- assign_cluster_colors(cluster_data, color_palette)

  # Calculate radius in meters (or pixels for simple CRS)
  cluster_data$radius_m <- calculate_radius(cluster_data, use_radius, is_simple)

  map |>
    leaflet::addCircles(
      data = cluster_data,
      lng = ~centroid_lon, lat = ~centroid_lat,
      radius = ~radius_m,
      color = ~stroke_color,
      fillColor = ~fill_color,
      fillOpacity = opacity,
      weight = ~stroke_weight,
      popup = ~popup,
      group = "Clusters",
      ...
    )
}

#' Add Location Points to Map
#' @keywords internal
add_location_points <- function(map, location_data, color, coords) {
  map |>
    leaflet::addCircleMarkers(
      data = location_data,
      lng = ~ get(coords$lon), lat = ~ get(coords$lat),
      radius = 4, color = color, fillColor = color, fillOpacity = 0.7,
      weight = 1, popup = ~popup, group = "Locations"
    )
}

#' Add Legend to Map
#' @keywords internal
add_cluster_legend <- function(map, cluster_data, color_palette) {
  if ("CLU_RR" %in% names(cluster_data)) {
    # Use RR legend
    rr_range <- range(cluster_data$CLU_RR, na.rm = TRUE)
    pal <- leaflet::colorBin(color_palette, domain = rr_range, bins = length(color_palette))

    map <- map |>
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = cluster_data$CLU_RR,
        title = "Cluster RR",
        opacity = 1
      )
  } else if ("P_VALUE" %in% names(cluster_data)) {
    # Fallback to P-value legend if RR missing
    map <- map |>
      leaflet::addLegend(
        position = "bottomright",
        colors = color_palette,
        labels = c("< 0.001", "0.001-0.01", "0.01-0.05", "0.05-0.1", "0.1-0.5", "> 0.5"),
        title = "Cluster P-value", opacity = 1
      )
  }

  # Add small significance note if stroke is used
  if ("P_VALUE" %in% names(cluster_data)) {
    map <- map |>
      leaflet::addLegend(
        position = "bottomright",
        colors = c("#e31a1c", "#999999"),
        labels = c("Significant (p < 0.05)", "Non-significant"),
        title = "Significance",
        opacity = 1
      )
  }

  map
}

# =============================================================================
# INTERNAL UTILS
# =============================================================================

assign_cluster_colors <- function(df, palette) {
  # Defaults
  df$fill_color <- "#999999"
  df$stroke_color <- "#666666"
  df$stroke_weight <- 1

  # 1. Fill by Relative Risk
  if ("CLU_RR" %in% names(df) && any(!is.na(df$CLU_RR))) {
    pal <- leaflet::colorBin(palette, domain = df$CLU_RR, bins = length(palette))
    df$fill_color <- pal(df$CLU_RR)
  } else if ("P_VALUE" %in% names(df)) {
    # Fallback legacy p-value color for fill if RR missing
    breaks <- c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1)
    df$fill_color <- as.character(cut(df$P_VALUE, breaks = breaks, labels = palette, include.lowest = TRUE))
  }

  # 2. Highlight significance with border
  if ("P_VALUE" %in% names(df)) {
    is_sig <- df$P_VALUE < 0.05
    df$stroke_color <- ifelse(is_sig, "#e31a1c", "#999999") # Red vs Dark Gray
    df$stroke_weight <- ifelse(is_sig, 3, 1)

    # Optional: Fade the fill for non-significant clusters?
    # No, keep it as is for now as RR is still useful info.
  }

  df
}

calculate_radius <- function(df, use_radius = TRUE, is_simple = FALSE) {
  if (use_radius && "RADIUS" %in% names(df)) {
    # Use physical radius
    if (is_simple) {
      # Cartesian/Pixel units
      df$RADIUS
    } else {
      # Lat/Long units (assumed Kilometers -> Meters)
      df$RADIUS * 1000
    }
  } else {
    # Fallback to heuristic based on location counts
    if ("n_locations" %in% names(df)) {
      scale <- if (is_simple) 10 else 3000
      sqrt(df$n_locations) * scale
    } else {
      if (is_simple) 50 else 10000
    }
  }
}
