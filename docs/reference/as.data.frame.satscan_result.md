# Convert SaTScan Results to Data Frame

Converts a `satscan_result` object to a standard data frame. You can
choose to extract either clusters or locations.

## Usage

``` r
# S3 method for class 'satscan_result'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("clusters", "locations"),
  ...
)
```

## Arguments

- x:

  A `satscan_result` object.

- row.names:

  Passed to as.data.frame (usually NULL).

- optional:

  Passed to as.data.frame.

- what:

  Character: "clusters" or "locations". Default is "clusters".

- ...:

  Additional arguments (currently unused).

## Value

A data.frame containing either cluster-level or location-level data.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- satscanr(cas, geo, pop, ...)

# Get clusters as data frame
clusters_df <- as.data.frame(result)

# Get locations as data frame
locations_df <- as.data.frame(result, what = "locations")
} # }
```
