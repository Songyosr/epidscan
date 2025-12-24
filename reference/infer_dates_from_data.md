# Infer Dates from Data Infer Dates from Data

Helper to infer StartDate and EndDate from the case data if missing from
options.

## Usage

``` r
infer_dates_from_data(
  current_opts,
  time_values,
  time_precision_char,
  verbose = FALSE
)
```

## Arguments

- current_opts:

  List of current SaTScan options

- time_values:

  Vector of time values (from case or pop file)

- time_precision_char:

  Character string: "day", "month", or "year"

- verbose:

  Logical, print messages

## Value

Named list of inferred dates (StartDate, EndDate) or NULL if nothing
inferred.
