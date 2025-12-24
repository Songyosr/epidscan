# Glance at SaTScan Results

Returns a single-row summary of the entire analysis, following the broom
package convention. Useful for comparing multiple analyses.

## Usage

``` r
# S3 method for class 'satscan_result'
glance(x, ...)
```

## Arguments

- x:

  A `satscan_result` object.

- ...:

  Additional arguments (currently unused).

## Value

A one-row data frame with analysis-level statistics:

- n_clusters:

  Number of clusters detected

- n_significant:

  Number of significant clusters (p \< 0.05)

- n_locations:

  Total locations analyzed

- n_in_clusters:

  Locations within detected clusters

- min_p_value:

  Minimum (best) p-value

- max_relative_risk:

  Maximum relative risk

- prop_in_clusters:

  Proportion of locations in clusters

## Examples

``` r
if (FALSE) { # \dontrun{
result <- satscanr(cas, geo, pop, ...)
glance(result)
} # }
```
