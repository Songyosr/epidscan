# Visualization Functions
#
# This module provides reusable functions for creating SatScan result visualizations.

#' Prepare Cluster Map Data
#'
#' Joins SatScan results with shapefile for visualization.
#'
#' @param satscan_results SatScan results object from rsatscan
#' @param shapefile sf object with geographic boundaries
#' @param location_id_field Character. Name of location ID field in shapefile
#' @param p_threshold Numeric. P-value threshold for significance (default 0.05)
#'
#' @return sf object with joined cluster information
#'
#' @examples
#' map_data <- prepare_cluster_map_data(
#'     satscan_results = results,
#'     shapefile = shp,
#'     location_id_field = "tambon_code"
#' )
prepare_cluster_map_data <- function(
    satscan_results,
    shapefile,
    location_id_field = "tambon_code",
    p_threshold = 0.05) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("sf package is required")
    }
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    # Validate inputs
    if (is.null(satscan_results$col) || nrow(satscan_results$col) == 0) {
        warning("No clusters in results")
        return(shapefile)
    }

    if (!location_id_field %in% names(shapefile)) {
        stop("location_id_field '", location_id_field, "' not found in shapefile")
    }

    # Get cluster information
    cluster_info <- satscan_results$col %>%
        dplyr::select(CLUSTER, REL_RISK, SPAN, START_DATE, END_DATE, P_VALUE) %>%
        dplyr::rename(CLUSTER_RR = REL_RISK, CLUSTER_P_VALUE = P_VALUE) %>%
        dplyr::mutate(significant = CLUSTER_P_VALUE < p_threshold)

    # Get location-level data
    if (!is.null(satscan_results$gis)) {
        cluster_locs <- satscan_results$gis %>%
            dplyr::left_join(cluster_info, by = "CLUSTER") %>%
            dplyr::filter(!is.na(CLUSTER_P_VALUE))
    } else {
        stop("No GIS data in results")
    }

    # Get tambon-level relative risk
    tambon_rr <- NULL
    if (!is.null(satscan_results$rr)) {
        tambon_rr <- satscan_results$rr %>%
            dplyr::select(LOC_ID, OBSERVED, EXPECTED, REL_RISK) %>%
            dplyr::rename(
                TAMBON_OBS = OBSERVED,
                TAMBON_EXP = EXPECTED,
                TAMBON_RR = REL_RISK
            )
    }

    # Join with shapefile
    map_data <- shapefile %>%
        dplyr::mutate(location_id = as.character(.data[[location_id_field]])) %>%
        dplyr::left_join(
            cluster_locs %>%
                dplyr::select(
                    LOC_ID, CLUSTER, CLUSTER_RR, LOC_RR, LOC_OBS, CLUSTER_P_VALUE,
                    significant, SPAN, START_DATE, END_DATE
                ),
            by = c("location_id" = "LOC_ID")
        )

    # Add tambon-level RR if available
    if (!is.null(tambon_rr)) {
        map_data <- map_data %>%
            dplyr::left_join(tambon_rr, by = c("location_id" = "LOC_ID"))
    }

    # Add cluster membership flag
    map_data <- map_data %>%
        dplyr::mutate(
            in_cluster = !is.na(CLUSTER),
            cluster_sig = ifelse(is.na(significant), NA, significant)
        )

    message("Map data prepared: ", nrow(map_data), " locations")
    message("  In clusters: ", sum(map_data$in_cluster, na.rm = TRUE))
    message("  In significant clusters: ", sum(map_data$cluster_sig, na.rm = TRUE))

    return(map_data)
}

#' Create Cluster Boundaries
#'
#' Generates cluster boundary polygons by unioning locations within each cluster.
#'
#' @param map_data sf object from prepare_cluster_map_data()
#'
#' @return sf object with cluster boundaries
create_cluster_boundaries <- function(map_data) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("sf package is required")
    }
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    if (!"in_cluster" %in% names(map_data)) {
        stop("map_data must be prepared with prepare_cluster_map_data()")
    }

    cluster_boundaries <- map_data %>%
        dplyr::filter(in_cluster) %>%
        dplyr::group_by(CLUSTER) %>%
        dplyr::summarise(
            geometry = sf::st_union(geometry),
            significant = dplyr::first(significant),
            CLUSTER_RR = dplyr::first(CLUSTER_RR),
            SPAN = dplyr::first(SPAN),
            START_DATE = dplyr::first(START_DATE),
            END_DATE = dplyr::first(END_DATE),
            .groups = "drop"
        )

    message("Created ", nrow(cluster_boundaries), " cluster boundaries")

    return(cluster_boundaries)
}

#' Create Diverging Color Palette for Relative Risk
#'
#' Creates a symmetric log-scale color palette centered on RR=1.
#'
#' @param data Numeric vector of relative risk values
#' @param na_color Character. Color for NA values (default gray)
#'
#' @return leaflet colorNumeric function
create_rr_palette <- function(data, na_color = "#E8E8E8") {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
        stop("leaflet package is required")
    }

    # Log transform (handle 0s and negative values)
    log_data <- log(pmax(data, 0.01, na.rm = TRUE))

    # Get symmetric range around 0 (log(1) = 0)
    rr_range <- max(abs(log_data), na.rm = TRUE)

    # Create diverging palette: blue (low) - white (neutral) - red (high)
    pal <- leaflet::colorNumeric(
        palette = grDevices::colorRampPalette(
            c("#2166AC", "#D1E5F0", "#FFFFFF", "#FDDBC7", "#B2182B")
        )(100),
        domain = c(-rr_range, rr_range),
        na.color = na_color
    )

    return(pal)
}

#' Add Cluster Polygons to Leaflet Map
#'
#' Adds cluster polygon layers to a Leaflet map with styling.
#'
#' @param map Leaflet map object
#' @param map_data sf object with cluster data
#' @param rr_field Character. Field name for relative risk to display
#' @param use_cluster_avg Logical. If TRUE, use CLUSTER_RR; if FALSE, use LOC_RR
#' @param name_field Character. Field for location names (optional)
#'
#' @return Updated Leaflet map
add_cluster_polygons <- function(
    map,
    map_data,
    rr_field = "CLUSTER_RR",
    use_cluster_avg = TRUE,
    name_field = NULL) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
        stop("leaflet package is required")
    }

    # Determine which RR to display
    map_data_vis <- map_data
    if (use_cluster_avg) {
        map_data_vis$display_rr <- ifelse(map_data$in_cluster, map_data$CLUSTER_RR, NA)
    } else {
        # For local RR, only show if location has cases
        map_data_vis$display_rr <- ifelse(
            map_data$in_cluster & map_data$LOC_OBS > 0,
            map_data$LOC_RR,
            NA
        )
    }

    # Create color palette
    map_data_vis$log_display_rr <- log(pmax(map_data_vis$display_rr, 0.01, na.rm = TRUE))
    pal <- create_rr_palette(map_data_vis$display_rr)

    # Add polygons
    map <- map %>%
        leaflet::addPolygons(
            data = map_data_vis,
            fillColor = ~ pal(log_display_rr),
            fillOpacity = ~ ifelse(in_cluster, 0.7, 0.1),
            color = "#999999",
            weight = 0.5,
            popup = ~ create_cluster_popup(
                map_data_vis,
                name_field = name_field,
                use_cluster_avg = use_cluster_avg
            )
        )

    return(map)
}

#' Create Cluster Popup Text
#'
#' Generates HTML popup text for cluster locations.
#'
#' @param data Data frame with cluster information
#' @param name_field Character. Field for location names
#' @param use_cluster_avg Logical. Whether showing cluster average or local RR
#'
#' @return Character vector of HTML popup text
create_cluster_popup <- function(data, name_field = NULL, use_cluster_avg = TRUE) {
    # Build base info
    if (!is.null(name_field) && name_field %in% names(data)) {
        popup <- paste0("<b>", data[[name_field]], "</b><br/>")
    } else {
        popup <- ""
    }

    # Add location ID
    if ("location_id" %in% names(data)) {
        popup <- paste0(popup, "ID: ", data$location_id, "<br/><hr>")
    }

    # Add cluster info
    popup <- ifelse(
        data$in_cluster,
        paste0(
            popup,
            "<b>Cluster ", data$CLUSTER, "</b>",
            ifelse(data$significant, " ⭐ <i>(Significant)</i>", " <i>(Not significant)</i>"),
            "<br/>",
            "<b>Cluster Avg RR: ", round(data$CLUSTER_RR, 2), "</b><br/>",
            "Time Span: ", round(data$SPAN, 1), " days<br/>",
            "Period: ", data$START_DATE, " to ", data$END_DATE, "<br/>"
        ),
        paste0(popup, "Not in any cluster")
    )

    # Add local RR if not using cluster average
    if (!use_cluster_avg && "LOC_RR" %in% names(data)) {
        popup <- ifelse(
            data$in_cluster,
            paste0(
                popup,
                "<hr>Local RR: ", round(data$LOC_RR, 2), "<br/>",
                "P-value: ", format(data$CLUSTER_P_VALUE, digits = 3)
            ),
            popup
        )
    } else {
        popup <- ifelse(
            data$in_cluster,
            paste0(popup, "P-value: ", format(data$CLUSTER_P_VALUE, digits = 3)),
            popup
        )
    }

    return(popup)
}

#' Add RR Legend to Map
#'
#' Adds a relative risk legend with meaningful values.
#'
#' @param map Leaflet map object
#' @param palette Color palette function
#' @param title Character. Legend title
#' @param position Character. Legend position
#'
#' @return Updated Leaflet map
add_rr_legend <- function(
    map,
    palette,
    title = "Relative Risk",
    position = "bottomright") {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
        stop("leaflet package is required")
    }

    # Standard RR values for legend
    legend_vals <- c(0.25, 0.5, 1, 2, 4, 8, 16)
    legend_colors <- sapply(log(legend_vals), function(x) palette(x))

    map <- map %>%
        leaflet::addLegend(
            colors = legend_colors,
            labels = legend_vals,
            title = title,
            position = position,
            opacity = 1
        )

    return(map)
}

#' Create Province Border Layer
#'
#' Creates a province border from shapefile for map overlay.
#'
#' @param shapefile sf object with geographic boundaries
#'
#' @return sf object with province boundary
create_province_border <- function(shapefile) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("sf package is required")
    }
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    province_border <- shapefile %>%
        dplyr::summarise(geometry = sf::st_union(geometry))

    return(province_border)
}
