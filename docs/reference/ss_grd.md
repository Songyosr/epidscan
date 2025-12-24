# Coerce Data to SaTScan Grid Table

Creates an `ss_tbl` object of type `"grd"` (SaTScan Grid File). A grid
file specifies the set of candidate centroids at which SaTScan evaluates
potential clusters. When a grid is supplied, SaTScan scans only at these
grid points (rather than at every observed location), which can reduce
computation and enforce a user-defined scanning lattice.

This constructor records the coordinate columns (and optional
constraints) without modifying the input data. Coordinate ordering rules
match `ss_geo` and are applied by the writer when producing the SaTScan
ASCII file.

## Usage

``` r
ss_grd(
  data,
  coord1,
  coord2,
  z = NULL,
  earliest_start = NULL,
  latest_start = NULL,
  earliest_end = NULL,
  latest_end = NULL,
  grid_variant = "basic"
)
```

## Arguments

- data:

  A data frame containing grid locations.

- coord1:

  Column name for the first coordinate.

- coord2:

  Column name for the second coordinate.

- z:

  Optional column name for Z-coordinate.

- earliest_start:

  Optional column for earliest start time constraint.

- latest_start:

  Optional column for latest start time constraint.

- earliest_end:

  Optional column for earliest end time constraint.

- latest_end:

  Optional column for latest end time constraint.

- grid_variant:

  Variant of grid file ("basic", etc.).

## Value

An `ss_tbl` object of type "grd".

## SaTScan File Specification

The Grid File has the same structure as the Coordinates File:
`<LocationID> <Latitude/Y> <Longitude/X>`

- **LocationID**: Unique identifier for the grid point.

- **Coordinates**: Same rules as `ss_geo` (Lat/Long order vs X/Y order).
