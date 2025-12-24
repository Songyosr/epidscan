# Prepare SaTScan Control File (Bernoulli)

**\[superseded\]**

## Usage

``` r
prep_ctl(
  df,
  loc_id,
  time = NULL,
  cases = NULL,
  covars = NULL,
  style = c("casewise", "aggregated"),
  time_precision = c("day", "month", "year", "generic")
)
```

## Arguments

- df:

  Data frame containing control data.

- loc_id:

  Column name for location ID (unquoted).

- time:

  Column name for time/date (unquoted, optional). Required for
  Space-Time analyses.

- cases:

  Column name for control counts (unquoted, optional). If NULL, assumes
  1 control per row.

- covars:

  Character vector of covariate column names.

- style:

  Input style: "casewise" or "aggregated".

- time_precision:

  Time resolution: "day", "month", "year", or "generic".

## Value

A `satscan_table` object of kind "ctl".

## Details

Prepares a control file for Bernoulli models. Similar to `prep_cas`, but
for controls (non-cases).

## SaTScan File Specification

The Control File has the following structure:
`<LocationID> <NoControls> <Date> <Covariate1> ...`

- **LocationID**: Identifier matching Case/Geo files.

- **NoControls**: Number of controls.

- **Date**: Required for Space-Time Bernoulli. Same format rules as Case
  file.

- **Covariates**: Optional.

## See also

[`ss_ctl()`](https://Songyosr.github.io/epidscan/reference/ss_ctl.md)
for the new `ss_tbl` interface.
