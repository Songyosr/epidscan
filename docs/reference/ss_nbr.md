# Coerce Data to SaTScan Neighbors Table

Creates an `ss_tbl` object of type `"nbr"` (SaTScan Non-Euclidean
Neighbors File). A neighbors file defines, for each centroid location,
an ordered list of neighboring locations ranked by closeness. SaTScan
constructs candidate clusters by progressively expanding each row
(centroid, then centroid + first neighbor, then + second neighbor, etc.)
until the row ends.

This constructor expects neighbors in a wide format (one row per
centroid, multiple neighbor columns in rank order). It preserves the
input data and records `neighbor_cols` so the writer can emit the
correct SaTScan ASCII structure without reshaping user data.

## Usage

``` r
ss_nbr(data, loc_id, neighbor_cols)
```

## Arguments

- data:

  A data frame containing neighbor lists.

- loc_id:

  Column name for the location ID.

- neighbor_cols:

  Character vector of column names representing neighbors.

## Value

An `ss_tbl` object of type "nbr".

## SaTScan File Specification

The Neighbors File has the following structure:
`<LocationID> <Neighbor1ID> <Neighbor2ID> ...`

- **LocationID**: Identifier for the location.

- **NeighborIDs**: One or more columns, each representing a neighbor of
  the `LocationID`. The order of these columns matters for SaTScan's
  interpretation of neighbor hierarchy.
