# Interactive Map of SaTScan Clusters

Creates an interactive leaflet map displaying detected spatial clusters
and their member locations. Clusters are shown as circles with size
proportional to the number of locations, colored by statistical
significance.

## Usage

``` r
map_clusters(
  x,
  significance_only = FALSE,
  provider = "CartoDB.Positron",
  cluster_opacity = 0.6,
  color_palette = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
  location_color = "#4575b4",
  show_locations = TRUE,
  popup_vars = NULL,
  verbose = FALSE,
  crs = NULL,
  use_radius = TRUE,
  simple = FALSE,
  ...
)
```

## Arguments

- x:

  A `satscan_result` object from
  [`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md).

- significance_only:

  Logical. If `TRUE`, only display clusters with p-value \< 0.05.
  Default is `FALSE` (show all clusters).

- provider:

  Character. Leaflet tile provider. Options include `"OpenStreetMap"`,
  `"CartoDB.Positron"`, `"Esri.WorldTopoMap"`, etc. See
  <https://leaflet-extras.github.io/leaflet-providers/preview/> for full
  list. Default is `"CartoDB.Positron"`.

- cluster_opacity:

  Numeric between 0 and 1. Opacity of cluster circles. Default is 0.6.

- color_palette:

  Character vector of colors for cluster significance levels. Default is
  `c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850")`
  (red to green spectrum).

- location_color:

  Character. Color for individual location markers. Default is
  `"#4575b4"` (blue).

- show_locations:

  Logical. If `TRUE`, show individual location points in addition to
  cluster boundaries. Default is `TRUE`.

- popup_vars:

  Character vector. Additional variables from `$locations` to include in
  location popups. Default is `NULL`.

- verbose:

  Logical. If `TRUE`, show progress messages. Default is `FALSE`.

- crs:

  Optional. CRS for projecting Cartesian coordinates (e.g., 32618). If
  provided, coordinates are projected to WGS84 for standard mapping.

- use_radius:

  Logical. If `TRUE` (default), use the physical radius from SaTScan
  results for circle size. If `FALSE`, size by number of locations.

- simple:

  Logical. If `TRUE`, force a simple coordinate system (pixels)
  regardless of data type. Useful for debugging or non-geographic scans.

- ...:

  Additional arguments passed to
  [`leaflet::addCircles()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
  for clusters.

## Value

A `leaflet` map object (htmlwidget) that can be printed to display or
saved with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).

## Details

The function requires the `leaflet` package to be installed. If not
available, an informative error message is displayed.

**Map Layers:**

- **Cluster circles**: Sized by number of member locations. Fill color
  represents Relative Risk (RR). Border color and thickness indicate
  significance.

- **Location points**: Individual locations within clusters (if
  `show_locations = TRUE`)

- **Popups**: Click on clusters or locations for detailed information.
  Cluster popups include counts, RR, and time period (if available).

**Color Scheme:**

- **Fill**: Mapped to Relative Risk (`CLU_RR`) using the provided
  `color_palette`. If RR is unavailable, falls back to p-value coloring.

- **Border (Stroke)**: Red and bold for significant clusters (p \<
  0.05). Gray and thin for non-significant clusters.

**Coordinate Detection:** The function automatically detects coordinate
columns from the locations table:

- Latitude: `lat`, `latitude`, `y`, `LATITUDE`

- Longitude: `lon`, `long`, `longitude`, `x`, `LONGITUDE`
