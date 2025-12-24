# Coerce Data to SaTScan Control Table

Creates an `ss_tbl` object of type `"ctl"` (SaTScan Control File). A
control file provides the number of controls by location and,
optionally, by time and covariate strata. When used with a case file
(Bernoulli models), SaTScan compares the spatial/temporal concentration
of cases against the background distribution implied by controls.

This constructor preserves the original data and stores column role
mappings. Time precision is not enforced here; if `time` is provided, it
will be formatted at write-time (via
[`write_satscan()`](https://Songyosr.github.io/epidscan/reference/write_satscan.md)),
using `time_precision` if supplied.

## Usage

``` r
ss_ctl(
  data,
  loc_id,
  controls,
  time = NULL,
  covars = NULL,
  time_precision = NULL
)
```

## Arguments

- data:

  A data frame containing control data.

- loc_id:

  Column name for location ID.

- controls:

  Column name for control counts.

- time:

  Optional column name for time.

- covars:

  Optional vector of column names for covariates.

- time_precision:

  User-specified time precision.

## Value

An `ss_tbl` object of type "ctl".

## SaTScan File Specification

The Control File has the following structure:
`<LocationID> <NoControls> <Date> <Covariate1> ...`

- **LocationID**: Identifier matching Case/Geo files.

- **NoControls**: Number of controls.

- **Date**: Required for Space-Time Bernoulli. Same format rules as Case
  file.

- **Covariates**: Optional.
