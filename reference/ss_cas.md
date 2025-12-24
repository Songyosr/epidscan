# Coerce Data to SaTScan Case Table

Creates an `ss_tbl` object of type `"cas"` (SaTScan Case File). A case
file provides the observed case counts by location and, optionally, by
time and covariate strata. SaTScan uses these counts as the primary
numerator when evaluating candidate spatial/temporal clusters against
the null model.

This constructor does not alter the input data. It records which columns
represent the required SaTScan fields (location ID, case count, and
optional time/covariates). Any time formatting is deferred to write-time
via
[`write_satscan()`](https://Songyosr.github.io/epidscan/reference/write_satscan.md)
(and
[`format_satscan_time()`](https://Songyosr.github.io/epidscan/reference/format_satscan_time.md)),
based on `time_precision`.

## Usage

``` r
ss_cas(data, loc_id, cases, time = NULL, covars = NULL, time_precision = NULL)
```

## Arguments

- data:

  A data frame containing case data.

- loc_id:

  Column name for location ID (character or numeric).

- cases:

  Column name for case counts (numeric).

- time:

  Optional column name for time (Date, numeric, or character).

- covars:

  Optional vector of column names for covariates.

- time_precision:

  User-specified time precision ("day", "month", "year", "generic").

## Value

An `ss_tbl` object of type "cas".

## SaTScan File Specification

The Case File has the following structure:
`<LocationID> <NoCases> <Date> <Covariate1> ...`

- **LocationID**: Character or numeric identifier. Matching ID must
  exist in Geo file.

- **NoCases**: Number of cases. For "casewise" style without a count
  column, this is set to 1. Zero-case rows are removed (implicit zeros).

- **Date**: Formatted according to `time_precision`:

  - "day": `YYYY/MM/DD`

  - "month": `YYYY/MM`

  - "year": `YYYY`

  - "generic": Time string/number

- **Covariates**: Optional categorical variables.

## Examples

``` r
df <- data.frame(id = c("A", "B"), count = c(10, 5), year = 2020)
ss_cas_obj <- ss_cas(df, loc_id = "id", cases = "count", time = "year", time_precision = "year")
```
