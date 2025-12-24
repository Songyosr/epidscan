# Enhanced Summary for SaTScan Results

Creates a detailed summary object containing cluster statistics,
location summaries, and analysis metadata. Returns a structured object
of class `summary.satscan_result` with a custom print method.

## Usage

``` r
# S3 method for class 'satscan_result'
summary(object, ...)
```

## Arguments

- object:

  A `satscan_result` object from
  [`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md).

- ...:

  Additional arguments (currently unused).

## Value

An object of class `summary.satscan_result` containing:

- n_clusters:

  Total number of detected clusters

- n_significant:

  Number of clusters with p \< 0.05

- n_locations:

  Total number of locations analyzed

- n_in_clusters:

  Number of locations belonging to clusters

- cluster_stats:

  Summary statistics for clusters (if available)

- location_stats:

  Summary statistics for locations

- most_likely:

  Details of the most likely cluster (if exists)

- rr_range:

  Range of relative risks (if available)

- analysis_params:

  Analysis parameters (if stored)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- satscanr(cas, geo, pop, ...)
summary(result)
} # }
```
