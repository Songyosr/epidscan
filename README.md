
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epidscan

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**A Modern R Interface for Spatio-Temporal Scan Statistics.**

`epidscan` brings modern data science workflows to epidemiological
cluster detection. Currently powering
[SaTScan™](https://www.satscan.org/), it is designed to be a flexible,
type-safe, and stateless interface for spatial and space-time analysis.

## Design Principles

- **Stateless**: No global side-effects.
- **Type-Safe**: `ss_tbl` classes ensure your data is analysis-ready
  before you run a single line of code.
- **Spatial-Native**: Built-in support for `sf` simple features.

## Installation

``` r
# Install from GitHub
pak::pak("Songyosr/epidscan")
```

**Requirements:** [SaTScan™](https://www.satscan.org/) must be installed
on your system.

## Quick Start

Here is a complete space-time analysis of the New Mexico lung cancer
dataset (included in the package).

``` r
library(epidscan)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(broom)

# 1. Prepare Data
# Load included datasets
data("NMlung_cas", "NMlung_pop", "NMlung_geo")

# Create validated inputs
cas <- ss_cas(NMlung_cas, loc_id = "county", cases = "cases", time = "date", time_precision = "month", covars = c("age_group", "sex"))
pop <- ss_pop(NMlung_pop, loc_id = "county", population = "population", time = "year", time_precision = "year", covars = c("age_group", "sex"))
geo <- ss_geo(NMlung_geo, loc_id = "county", coord1 = "x_km", coord2 = "y_km", coord_type = "cartesian")

# 2. Run Analysis
# Set path to SaTScan (adjust for your system)
# Note: This path is specific to macOS; adjust for Windows/Linux
# set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")
set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")
#> SatScan path set to: /Applications/SaTScan.app/Contents/app/satscan

# Configure with prm_options (Object-Oriented Parameter Management)
# This creates a reproducible parameter object that can be inspected and reused.
my_prm <- prm_options(
  AnalysisType = 3, # Retrospective Space-Time
  ModelType = 0, # Poisson
  MonteCarloReps = 999
)

# Run the analysis
result <- satscanr(
  cas, pop, geo,
  prm = my_prm,
  verbose = FALSE
)

# 3. Explore Results
# Tidy summary of detected clusters
summary(result)
#> SaTScan Results Summary
#> =======================
#> 
#> ── SaTScan Analysis Summary ──────────────────────────────────
#> Model: Discrete Poisson [High Rates]
#> Scan:  Retrospective Space-Time
#> Time:  1973/01/01 to 1991/12/31 (Month)
#> Space: Circular (Max: 50% pop)
#> Sims:  999 Monte Carlo Reps
#> ──────────────────────────────────────────────────────────────
#> 
#> Overview:
#>   Clusters detected:  6 (2 significant at p < 0.05)
#>   Locations analyzed: 37 (17 in clusters)
#>   Relative risk:      0.30 - 1.36
#> 
#> Most Likely Cluster:
#>   Cluster ID:         1
#>   P-value:            0.000000
#>   Observed:           714
#>   Expected:           502.0
#> 
#> Cluster Statistics:
#>   statistic        min        q1 median       mean        q3     max
#> 1   p_value 6.7257e-13 0.2020006  0.866  0.6128335   0.94275   0.996
#> 2 radius_km 0.0000e+00 0.0000000 58.860 64.3225655 121.00681 146.113
#> 
#> 
#> (Raw SaTScan text available. Use print(result, raw = TRUE) to view)

# Analysis-level metrics
tidy(result)
#> # A tibble: 6 × 13
#>   cluster center_id     x     y  p_value   llr observed expected obs_exp_ratio
#>     <int> <chr>     <dbl> <dbl>    <dbl> <dbl>    <dbl>    <dbl>         <dbl>
#> 1       1 Chaves      418   156 6.73e-13 42.1       714   502.            1.42
#> 2       2 Socorro     199   222 7.71e- 7 25.7      2474  2177.            1.14
#> 3       3 Lea         528   103 8.08e- 1  8.60       10     1.88          5.32
#> 4       4 Chaves      418   156 9.24e- 1  8.11      467   387.            1.21
#> 5       5 Sierra      166   127 9.49e- 1  7.91       10     2.05          4.88
#> 6       6 SanJuan      96   531 9.96e- 1  6.88       10     2.34          4.28
#> # ℹ 4 more variables: radius_km <dbl>, population <dbl>, start_date <date>,
#> #   end_date <date>

# Visualization
# map_clusters(result, simple = TRUE) # Interactive map (view locally)
```

For a detailed walkthrough, implementation details, and advanced
configuration, see the [Getting Started
vignette](vignettes/getting-started.Rmd).

## Available Input Types

| Function   | File Type | Purpose                 |  status   |
|------------|-----------|-------------------------|:---------:|
| `ss_cas()` | `.cas`    | Case counts             |     ✓     |
| `ss_pop()` | `.pop`    | Population denominators |     ✓     |
| `ss_geo()` | `.geo`    | Spatial coordinates     |     ✓     |
| `ss_ctl()` | `.ctl`    | Controls (Bernoulli)    |     ✓     |
| `ss_grd()` | `.grd`    | Custom scan centers     |     ✓     |
| `ss_nwk()` | `.nwk`    | Network edges           | Under Dev |
| `ss_nbr()` | `.nbr`    | Neighbor rankings       | Under Dev |
| `ss_met()` | `.met`    | Meta-locations          | Under Dev |

## Documentation

- [Getting Started](doc/getting-started.html)
- [Parameter Reference](doc/satscan-parameters.html)
- [Report Issues](https://github.com/Songyosr/epidscan/issues)

## Citation

Please cite the original SaTScan software and methodology:

> Kulldorff M. (1997). A spatial scan statistic. Communications in
> Statistics: Theory and Methods, 26:1481-1496.

> Kulldorff M. and Information Management Services Inc. (2018). SaTScan™
> User Guide. www.satscan.org.

To cite this R package:

``` r
citation("epidscan")
```

## License

MIT © Songyos Rajborirug
