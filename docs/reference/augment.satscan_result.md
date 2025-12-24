# Augment SaTScan Results with Location-Level Data

Adds cluster assignments and statistics to the original location data,
following the broom package convention.

## Usage

``` r
# S3 method for class 'satscan_result'
augment(x, data = NULL, ...)
```

## Arguments

- x:

  A `satscan_result` object.

- data:

  Optional original data to augment. If NULL, uses x\$locations.

- ...:

  Additional arguments (currently unused).

## Value

A data frame with original location data plus:

- .cluster:

  Cluster assignment (NA if not in cluster)

- .in_cluster:

  Logical: is location in a cluster?

- .relative_risk:

  Location-specific relative risk

- .cluster_p_value:

  P-value of assigned cluster

## Examples

``` r
if (FALSE) { # \dontrun{
result <- satscanr(cas, geo, pop, ...)
augment(result)

# Or augment original data
augment(result, data = original_data)
} # }
```
