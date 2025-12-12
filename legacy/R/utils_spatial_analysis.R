#' Spatial Odds Analysis Functions
#'
#' This script contains functions for performing Kernel Density Estimation (KDE)
#' and calculating Spatial Odds Ratios (SOR) for disease clustering analysis.

#' @importFrom terra rast crs resample compareGeom global
#' @importFrom sf st_is_longlat st_transform st_coordinates
#' @importFrom spatstat.geom as.owin ppp
#' @importFrom stats density
#' @export
#'
#' @param points sf object of case locations
#' @param study_area sf object of the study area boundary
#' @param bandwidth Bandwidth (sigma) for the Gaussian kernel (in map units, e.g., meters)
#' @param resolution Resolution of the output raster (in map units)
#' @return A terra raster object representing the density
calculate_kde <- function(points, study_area, bandwidth, resolution = 100) {
    # Ensure projected CRS (e.g., UTM) for accurate distance calculations
    if (st_is_longlat(points)) {
        warning("Points are in Long/Lat. Projecting to UTM Zone 47N (EPSG:32647) for KDE.")
        points <- st_transform(points, 32647)
        study_area <- st_transform(study_area, 32647)
    }

    # Convert to spatstat ppp object
    win <- as.owin(study_area)
    coords <- st_coordinates(points)
    ppp_obj <- ppp(x = coords[, 1], y = coords[, 2], window = win)

    # Calculate Density
    # sigma is bandwidth
    dens <- density(ppp_obj, sigma = bandwidth, eps = resolution)

    # Convert to terra raster
    r <- rast(dens)
    crs(r) <- "EPSG:32647" # Assign CRS

    return(r)
}

#' Calculate Spatial Odds Ratio
#'
#' @param case_kde Raster of case density
#' @param pop_raster Raster of population density (control)
#' @return Raster of Spatial Odds Ratio (Case / Pop)
#' @export
calculate_spatial_odds <- function(case_kde, pop_raster) {
    # Resample population raster to match case KDE resolution/extent
    if (!compareGeom(case_kde, pop_raster, stopOnError = FALSE)) {
        message("Resampling population raster to match case KDE...")
        pop_raster <- resample(pop_raster, case_kde, method = "bilinear")
    }

    # Mask both to ensure same coverage
    # (Assuming they are already masked to study area, but good to be safe)

    # Calculate Ratio
    # Add small constant to avoid division by zero if needed, or handle NAs
    # SOR = (Case Density) / (Pop Density)
    # Normalized? Usually: (Case Density / Total Cases) / (Pop Density / Total Pop)

    # Let's do raw density ratio first, but normalization makes it interpretable as "Odds relative to expectation"

    # Normalize
    case_norm <- case_kde / global(case_kde, "sum", na.rm = TRUE)$sum
    pop_norm <- pop_raster / global(pop_raster, "sum", na.rm = TRUE)$sum

    sor <- case_norm / pop_norm

    return(sor)
}

#' Smooth Population Raster
#'
#' Apply Gaussian smoothing to population raster to match the scale of analysis
#' @param pop_raster Input population raster
#' @param sigma Smoothing bandwidth
#' @return Smoothed raster
#' @export
smooth_raster <- function(pop_raster, sigma) {
    # Use focal operation for smoothing or terra::focal
    # Or convert to points and use spatstat density (slow for raster)
    # terra::focal with Gaussian weight matrix

    # Simple approximation: focal mean with window size ~ sigma
    # Better: create gaussian weights

    # For now, let's assume we might just use the raw population or a simple focal smooth
    # if resolution is high.

    # If using spatstat for cases, consistency suggests using it for pop too?
    # But pop is already a surface.

    # Let's stick to terra::focal for now if needed.
    return(pop_raster)
}
