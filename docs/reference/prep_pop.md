# Prepare SaTScan Population File

**\[superseded\]**

## Usage

``` r
prep_pop(df, loc_id, time, pop, covars = NULL)
```

## Arguments

- df:

  Data frame containing population data.

- loc_id:

  Column name for location ID (unquoted).

- time:

  Column name for time/year (unquoted). Population times are anchors.

- pop:

  Column name for population count (unquoted).

- covars:

  Character vector of covariate column names.

## Value

A `satscan_table` object of kind "pop".

## Details

Prepares a population file for SaTScan, used as the denominator for
Poisson models. Population data is treated as "census anchors" - SaTScan
interpolates between these time points.

## SaTScan File Specification

The Population File has the following structure:
`<LocationID> <Year/Time> <Population> <Covariate1> ...`

- **LocationID**: Character or numeric identifier. Match cases and geo.

- **Year/Time**: The "census year" or time point. For daily analysis,
  you can still use yearly population anchors (e.g. 2023, 2024). SaTScan
  interpolates linearly.

- **Population**: The count of people at risk.

- **Covariates**: Optional. If used, population must be stratified by
  these covariates (i.e., you need a row for every combination of
  Location, Time, and Covariate levels).

## See also

[`ss_pop()`](https://Songyosr.github.io/epidscan/reference/ss_pop.md)
for the new `ss_tbl` interface.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic yearly population
head(pop_data)
#   county year  pop
# 1    001 2020 5000
# 2    001 2021 5100

pop_obj <- prep_pop(pop_data, loc_id = county, time = year, pop = pop)

# Stratified by sex
head(pop_strat)
#   county year  pop sex
# 1    001 2020 2500   M
# 2    001 2020 2500   F

pop_obj2 <- prep_pop(pop_strat, loc_id = county, time = year, pop = pop, covars = "sex")
} # }
```
