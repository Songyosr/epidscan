# Prepare SaTScan Case File

**\[superseded\]**

## Usage

``` r
prep_cas(
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

  Data frame containing case data.

- loc_id:

  Column name for location ID (unquoted).

- time:

  Column name for time/date (unquoted, optional).

- cases:

  Column name for case counts (unquoted, optional). If NULL, assumes 1
  case per row (casewise).

- covars:

  Character vector of covariate column names.

- style:

  Case input style:

  - "casewise": Each row is a single case (cases=1).

  - "aggregated": Rows represent counts.

- time_precision:

  Time resolution for formatting: "day", "month", "year", or "generic".

## Value

A `satscan_table` object of kind "cas".

## Details

Prepares a case file for SaTScan, enforcing strict sparsity (no
zero-case rows) and handling time precision formatting.

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

## See also

[`ss_cas()`](https://Songyosr.github.io/epidscan/reference/ss_cas.md)
for the new `ss_tbl` interface.

## Examples

``` r
if (FALSE) { # \dontrun{
# aggregated daily
head(my_daily_data)
#   zipcode       date cases age_group
# 1   10001 2023-01-01     2     child
# 2   10002 2023-01-01     0     child  <-- Removed

cas_obj <- prep_cas(my_daily_data,
    loc_id = zipcode, time = date, cases = cases,
    covars = "age_group", time_precision = "day"
)

# casewise
head(my_cases)
#      id diagnosis_date
# 1 P001     2023-01-15

cas_obj2 <- prep_cas(my_cases,
    loc_id = id, time = diagnosis_date,
    style = "casewise", time_precision = "day"
)
} # }
```
