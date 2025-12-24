# Modify PRM Parameters

Updates existing parameters in a prm_list. By default, errors if a
parameter is not found (strict mode).

## Usage

``` r
prm_set(prm, ..., .strict = TRUE)
```

## Arguments

- prm:

  A `prm_list` object from
  [`prm_parse()`](https://Songyosr.github.io/epidscan/reference/prm_parse.md).

- ...:

  Named parameters to modify (e.g., `CaseFile = "test.cas"`).

- .strict:

  If `TRUE` (default), errors on unknown keys. If `FALSE`, warns.

## Value

Modified `prm_list`.

## Examples

``` r
if (FALSE) { # \dontrun{
prm <- prm_defaults()
prm <- prm_set(prm, CaseFile = "my_cases.cas", AnalysisType = 3)
} # }
```
