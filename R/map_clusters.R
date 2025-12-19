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
#' - **Cluster circles**: Sized by number of member locations, colored by p-value
#' - **Location points**: Individual locations within clusters (if `show_locations = TRUE`)
#' - **Popups**: Click on clusters or locations for detailed information
#'
#' **Color Scheme:**
#' By default, clusters are colored on a gradient from red (most significant,
#' p < 0.001) to green (least significant, p > 0.1). Non-significant clusters
#' (p >= 0.05) are shown in muted colors when `significance_only = FALSE`.
#'
#' **Coordinate Detection:**
#' The function automatically detects coordinate columns from the locations table:
#' - Latitude: `lat`, `latitude`, `y`, `LATITUDE`
#' - Longitude: `lon`, `long`, `longitude`, `x`, `LONGITUDE`
#'
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
                         ...) {
  # =========================================================================
  # PACKAGE CHECK
  # =========================================================================
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop(
      "Package 'leaflet' is required for interactive mapping.\n",
      "Install it with: install.packages('leaflet')",
      call. = FALSE
    )
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
  # 3. DETECT COORDINATES
  # =========================================================================
  # Detect coords from locations (most reliable source)
  coords <- detect_coordinates(x$locations)

  # =========================================================================
  # 4. PREPARE DATA
  # =========================================================================
  # Prepare clusters
  cluster_data <- prepare_cluster_data(
    x = x,
    significance_only = significance_only,
    coords = coords
  )

  # Prepare locations
  location_data <- prepare_location_data(
    x = x,
    coords = coords,
    popup_vars = popup_vars
  )

  # =========================================================================
  # 5. BUILD MAP
  # =========================================================================
  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(provider)

  # Add cluster circles
  if (!is.null(cluster_data) && nrow(cluster_data) > 0) {
    m <- add_cluster_circles(
      map = m,
      cluster_data = cluster_data,
      color_palette = color_palette,
      opacity = cluster_opacity,
      ...
    )
  }

  # Add location points
  if (show_locations && !is.null(location_data) && nrow(location_data) > 0) {
    m <- add_location_points(
      map = m,
      location_data = location_data,
      color = location_color,
      coords = coords
    )
  }

  # Add legend
  if (!is.null(cluster_data) && nrow(cluster_data) > 0 && "P_VALUE" %in% names(cluster_data)) {
    m <- add_cluster_legend(m, color_palette)
  }

  m
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Detect Coordinate Columns
#'
#' @param df Data frame with location coordinates
#' @return List with lat_col and lon_col
#' @keywords internal
detect_coordinates <- function(df) {
  # Priority: 1. ss_tbl roles, 2. semantic names, 3. common names
  candidates <- list(
    lat = c("ss_lat", "lat", "latitude", "y", "LATITUDE", "Latitude"),
    lon = c("ss_long", "ss_lon", "lon", "long", "longitude", "x", "LONGITUDE", "Longitude")
  )

  find_col <- function(choices) {
    for (c in choices) {
      if (c %in% names(df)) {
        return(c)
      }
    }
    NULL
  }

  lat_col <- find_col(candidates$lat)
  lon_col <- find_col(candidates$lon)

  if (is.null(lat_col) || is.null(lon_col)) {
    stop("Could not detect spatial coordinates in locations table.", call. = FALSE)
  }

  list(lat = lat_col, lon = lon_col)
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
      dplyr::filter(!is.na(.data$CLUSTER)) |>
      dplyr::group_by(.data$CLUSTER) |>
      dplyr::summarise(
        n_locations = dplyr::n(),
        centroid_lat = mean(.data[[coords$lat]], na.rm = TRUE),
        centroid_lon = mean(.data[[coords$lon]], na.rm = TRUE),
        .groups = "drop"
      )

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
add_cluster_circles <- function(map, cluster_data, color_palette, opacity, ...) {
  cluster_data <- assign_cluster_colors(cluster_data, color_palette)
  cluster_data$radius_m <- calculate_radius(cluster_data)

  map |>
    leaflet::addCircles(
      data = cluster_data,
      lng = ~centroid_lon, lat = ~centroid_lat,
      radius = ~radius_m,
      color = ~color, fillColor = ~color, fillOpacity = opacity,
      weight = 2, popup = ~popup, group = "Clusters",
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
add_cluster_legend <- function(map, color_palette) {
  map |>
    leaflet::addLegend(
      position = "bottomright",
      colors = color_palette,
      labels = c("< 0.001", "0.001-0.01", "0.01-0.05", "0.05-0.1", "0.1-0.5", "> 0.5"),
      title = "Cluster P-value", opacity = 1
    )
}

# =============================================================================
# INTERNAL UTILS
# =============================================================================

assign_cluster_colors <- function(df, palette) {
  if ("P_VALUE" %in% names(df)) {
    breaks <- c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1)
    df$color <- as.character(cut(df$P_VALUE, breaks = breaks, labels = palette, include.lowest = TRUE))
  } else {
    df$color <- palette[3]
  }
  df
}

calculate_radius <- function(df) {
  if ("n_locations" %in% names(df)) {
    sqrt(df$n_locations) * 3000 # 3km base for visibility
  } else if ("RADIUS" %in% names(df)) {
    df$RADIUS * 1000
  } else {
    10000
  }
}
