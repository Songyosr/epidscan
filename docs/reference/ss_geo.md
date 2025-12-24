# Coerce Data to SaTScan Coordinates Table

Creates an `ss_tbl` object of type `"geo"` (SaTScan Coordinates File). A
coordinates file defines the spatial location of each analysis unit, and
SaTScan uses these locations to compute distances and to construct
circular/elliptic scanning windows. Coordinates may be provided in
latitude/longitude or Cartesian units; the required output column order
differs between these coordinate types.

If `data` is an `sf` object, this function extracts feature centroids
and maps them to SaTScan’s expected coordinate order (Lat, Long for
lat/long CRS; X, Y for projected CRS). If `data` is a plain data frame,
it records the user-supplied coordinate columns and (when
`coord_type = "auto"`) applies a simple heuristic to infer lat/long vs
Cartesian. No projection or coordinate transformation is performed.

## Usage

``` r
ss_geo(
  data,
  loc_id,
  coord1 = NULL,
  coord2 = NULL,
  z = NULL,
  coord_type = c("auto", "latlong", "cartesian")
)
```

## Arguments

- data:

  A data frame or `sf` object containing coordinate data.

- loc_id:

  Column name for location ID.

- coord1:

  Column name for the first coordinate (X/Longitude), required if `data`
  is not `sf`.

- coord2:

  Column name for the second coordinate (Y/Latitude), required if `data`
  is not `sf`.

- z:

  Optional column name for altitude/Z-coordinate.

- coord_type:

  Coordinate system: "auto" (default), "latlong", or "cartesian".

## Value

An `ss_tbl` object of type "geo".

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
