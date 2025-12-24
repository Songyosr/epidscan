# Prepare SaTScan Grid File

**\[superseded\]**

## Usage

``` r
prep_grd(x, loc_id = NULL, coords = NULL)
```

## Arguments

- x:

  Input object (sf or data.frame).

- loc_id:

  Column name for location ID (unquoted).

- coords:

  Character vector of length 2 (required if x is data.frame).

## Value

A `satscan_table` object of kind "grd".

## Details

Prepares a grid file for SaTScan, which defines the centers of the
scanning windows. Leverages `prep_geo` logic for coordinate extraction
and formatting.

## SaTScan File Specification

The Grid File has the same structure as the Coordinates File:
`<LocationID> <Latitude/Y> <Longitude/X>`

- **LocationID**: Unique identifier for the grid point.

- **Coordinates**: Same rules as `prep_geo` (Lat/Long order vs X/Y
  order).

## See also

[`ss_grd()`](https://Songyosr.github.io/epidscan/reference/ss_grd.md)
for the new `ss_tbl` interface.

## Examples

``` r
if (FALSE) { # \dontrun{
# From sf grid
grid_sf <- sf::st_make_grid(study_area, n = c(10, 10)) %>% sf::st_sf()
grid_sf$id <- 1:nrow(grid_sf)

grd_obj <- prep_grd(grid_sf, loc_id = id)
} # }
```
