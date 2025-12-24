# Coerce Data to SaTScan Network Table

Creates an `ss_tbl` object of type `"nwk"` (SaTScan Network File). A
network file defines adjacency between locations as an edge list,
optionally with a distance or weight for each connection. SaTScan can
use this structure to define cluster expansion on a network (rather than
by Euclidean distance), making it suitable for road networks or other
connectivity-based proximity.

This constructor records which columns define the source location, the
neighbor location, and the optional distance/weight. It does not compute
distances or alter the network; those decisions are deferred to SaTScan
and the analysis configuration.

## Usage

``` r
ss_nwk(data, loc_id, neighbor_id, distance = NULL, distance_units = NULL)
```

## Arguments

- data:

  A data frame containing network edges.

- loc_id:

  Column name for the source location ID.

- neighbor_id:

  Column name for the connected neighbor ID.

- distance:

  Optional column name for distance/weight for the connection.

- distance_units:

  Units for distance (optional string).

## Value

An `ss_tbl` object of type "nwk".

## SaTScan File Specification

The Network File has the following structure:
`<LocationID> <NeighborID> <Distance>`

- **LocationID**: Identifier for the source location.

- **NeighborID**: Identifier for the connected neighbor location.

- **Distance**: Optional. Numeric distance or weight between the two
  locations.
