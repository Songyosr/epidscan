# Prepare SaTScan Geometry File

**\[superseded\]**

## Usage

``` r
prep_geo(
  x,
  loc_id,
  coords = NULL,
  geometry = NULL,
  coord_type = c("auto", "latlong", "cartesian")
)
```

## Arguments

- x:

  Input object. Can be an `sf` object or a `data.frame`.

- loc_id:

  Column name for location ID (unquoted).

- coords:

  Character vector of length 2 specifying coordinate columns if x is a
  data.frame.

- geometry:

  Column name for geometry if x is sf and not active geometry
  (optional).

- coord_type:

  Coordinate system: "auto" (default), "latlong", or "cartesian".

  - "auto": Attempts to detect from data range or CRS.

  - "latlong": Forces interpretation as Latitude/Longitude.

  - "cartesian": Forces interpretation as Cartesian (X/Y).

## Value

A `satscan_table` object of kind "geo".

## Details

Prepares a coordinate file for SaTScan. Handles `sf` objects (extracting
centroids) or plain data frames.

## SaTScan File Specification

The Coordinates File has the following structure:
`<LocationID> <Latitude/Y> <Longitude/X>`

- **LocationID**: Unique identifier matching Case/Population files.

- **Coordinate Order**:

  - **Lat/Long**: SaTScan expects `Latitude` first, then `Longitude`.

  - **Cartesian**: SaTScan expects `X` first, then `Y`.

**Note on Coordinate Ordering**: This function automatically handles the
swapping for you:

- If `sf` object (Lat/Long crs) -\> Swaps to `Lat, Long`.

- If `data.frame` (Lat/Long detected) -\> Swaps to `Lat, Long`.

- If `data.frame` (Cartesian detected) -\> Keeps as `X, Y`.

## See also

[`ss_geo()`](https://Songyosr.github.io/epidscan/reference/ss_geo.md)
for the new `ss_tbl` interface.

## Examples

``` r
if (FALSE) { # \dontrun{
# From sf object
library(sf)
my_shapes <- st_read("shapes.shp")
geo_obj <- prep_geo(my_shapes, loc_id = GEOID)

# From data frame (Lat/Long)
df <- data.frame(id = "A", lon = 100, lat = 13)
geo_obj2 <- prep_geo(df, loc_id = id, coords = c("lon", "lat"), coord_type = "latlong")

# From data frame (Cartesian)
df_cart <- data.frame(id = "A", x = 500000, y = 100000)
geo_obj3 <- prep_geo(df_cart, loc_id = id, coords = c("x", "y"), coord_type = "cartesian")
} # }
```
