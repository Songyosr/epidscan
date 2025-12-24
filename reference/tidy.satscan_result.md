# Tidy SaTScan Results into a Data Frame

Returns a tidy data frame with one row per cluster, containing
cluster-level statistics. This follows the broom package convention for
model tidying.

## Usage

``` r
# S3 method for class 'satscan_result'
tidy(x, ...)
```

## Arguments

- x:

  A `satscan_result` object.

- ...:

  Additional arguments (currently unused).

## Value

A data frame (tibble if available) with cluster-level statistics. Common
columns include:

- cluster:

  Cluster identifier

- p_value:

  Statistical significance

- relative_risk:

  Relative risk within cluster

- observed:

  Observed number of cases

- expected:

  Expected number of cases

- obs_exp_ratio:

  Observed/Expected ratio

## Examples

``` r
if (FALSE) { # \dontrun{
result <- satscanr(cas, geo, pop, ...)
tidy(result)
} # }
```
