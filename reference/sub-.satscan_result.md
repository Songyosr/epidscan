# Extract Specific Clusters

Subset a `satscan_result` object to include only specified clusters.

## Usage

``` r
# S3 method for class 'satscan_result'
x[i, ...]
```

## Arguments

- x:

  A `satscan_result` object.

- i:

  Cluster IDs to extract (numeric or character vector).

- ...:

  Additional arguments (currently unused).

## Value

A new `satscan_result` object containing only the specified clusters and
their member locations.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- satscanr(cas, geo, pop, ...)

# Extract just cluster 1
cluster1 <- result[1]

# Extract clusters 1 and 3
subset <- result[c(1, 3)]

# Extract significant clusters
sig_clusters <- result[result$clusters$P_VALUE < 0.05]
} # }
```
