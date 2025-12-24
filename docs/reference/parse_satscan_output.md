# Parse SaTScan Output into S3 Object

Parse SaTScan Output into S3 Object

## Usage

``` r
parse_satscan_output(
  ss_results,
  geo_df,
  loc_id_col,
  keep_raw = FALSE,
  verbose = FALSE
)
```

## Arguments

- ss_results:

  Result from read_satscan_files()

- geo_df:

  Data frame from ss_geo() with location IDs and coordinates

- loc_id_col:

  Name of the location ID column in geo_df

- keep_raw:

  Logical. Include raw output files?

- verbose:

  Print debug info?

## Value

Object of class "satscan_result"
