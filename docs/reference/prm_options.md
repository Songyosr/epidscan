# Create SaTScan Parameter Object

Constructor for a `satscan_prm` object. Defaults are loaded based on the
configured SaTScan version (if available), and user overrides are
applied.

## Usage

``` r
prm_options(..., version = NULL, base_prm = NULL)
```

## Arguments

- ...:

  Named parameters to override (e.g., `AnalysisType = 3`). These must
  match standard SaTScan parameter names (case-sensitive). Common
  parameters include:

  - `AnalysisType`:

    - 1 = Purely Spatial

    - 2 = Purely Temporal

    - 3 = Retrospective Space-Time

    - 4 = Prospective Space-Time

    - 5 = Spatial Variation in Temporal Trends

    - 6 = Prospective Purely Temporal

    - 7 = Seasonal Temporal

  - `ModelType`:

    - 0 = Discrete Poisson

    - 1 = Bernoulli

    - 2 = Space-Time Permutation

    - 3 = Ordinal

    - 4 = Exponential

    - 5 = Normal

    - 6 = Continuous Poisson

    - 7 = Multinomial

    - 8 = Rank

    - 9 = Uniform Time

    - 10 = Batched

  - `ScanAreas`:

    - 1 = High Rates

    - 2 = Low Rates

    - 3 = Both High and Low Rates

  - `TimeAggregationUnits` / `PrecisionCaseTimes`:

    - 1 = Year

    - 2 = Month

    - 3 = Day

    - 4 = Generic

  - `MonteCarloReps`: Number of replications (e.g., 999)

- version:

  SaTScan version string (e.g., "10.2"). If NULL (default), attempts to
  auto-detect from the configured SaTScan executable.

- base_prm:

  Optional existing `satscan_prm` object to extend.

## Value

An object of class `satscan_prm` (which is a subclass of `prm_list`).

## See also

[`prm_defaults`](https://Songyosr.github.io/epidscan/reference/prm_defaults.md),
[`prm_set`](https://Songyosr.github.io/epidscan/reference/prm_set.md)
[`vignette("satscan-parameters", package = "epidscan")`](https://Songyosr.github.io/epidscan/articles/satscan-parameters.md)
