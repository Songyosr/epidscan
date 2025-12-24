# Coerce Data to SaTScan Population Table

Creates an `ss_tbl` object of type `"pop"` (SaTScan Population File). A
population file provides the population-at-risk by location and time and
is used as the denominator for Poisson-based scan statistics. If
multiple time points are supplied, SaTScan may interpolate population
size between time anchors depending on the analysis setup.

This constructor records the mappings for location ID, time, population,
and optional covariate strata without changing the input data. Any time
formatting is deferred to
[`write_satscan()`](https://Songyosr.github.io/epidscan/reference/write_satscan.md),
using `time_precision` if provided.

## Usage

``` r
ss_pop(data, loc_id, time, population, covars = NULL, time_precision = NULL)
```

## Arguments

- data:

  A data frame containing population data.

- loc_id:

  Column name for location ID.

- time:

  Column name for time (census year/time).

- population:

  Column name for population count.

- covars:

  Optional vector of column names for covariates.

- time_precision:

  User-specified time precision (e.g. "year" to format Dates as YYYY).

## Value

An `ss_tbl` object of type "pop".

## SaTScan File Specification

The Population File has the following structure:
`<LocationID> <Year/Time> <Population> <Covariate1> ...`

- **LocationID**: Character or numeric identifier. Match cases and geo.

- **Year/Time**: The "census year" or time point. For daily analysis,
  you can still use yearly population anchors (e.g. 2023, 2024). SaTScan
  interpolates linearly.

- **Population**: The count of people at risk.

- **Covariates**: Optional. If used, population must be stratified by
  these covariates.
