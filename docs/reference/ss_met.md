# Coerce Data to SaTScan Meta Locations

Creates an `ss_tbl` object of type `"met"` (SaTScan Meta Locations
File). A meta locations file defines groupings of multiple locations
into a single meta-location (e.g., aggregating tracts into counties).
SaTScan uses these groupings to treat members as belonging to the same
higher-level unit when a meta-location analysis is requested.

This constructor expects a wide representation (one row per
meta-location, with one or more member columns). It preserves the input
data and records `member_cols` so the writer can output the correct
SaTScan ASCII format.

## Usage

``` r
ss_met(data, meta_loc_id, member_cols)
```

## Arguments

- data:

  A data frame containing meta-location definitions.

- meta_loc_id:

  Column name for the meta-location ID.

- member_cols:

  Character vector of column names representing member locations.

## Value

An `ss_tbl` object of type "met".

## SaTScan File Specification

The Meta Locations File has the following structure:
`<MetaLocationID> <Member1ID> <Member2ID> ...`

- **MetaLocationID**: Identifier for the meta-location (e.g., a county).

- **MemberIDs**: One or more columns, each representing a member
  location (e.g., a census tract) that belongs to the `MetaLocationID`.
