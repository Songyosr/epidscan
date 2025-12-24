# Parse a SaTScan PRM Source

Converts a PRM file or character vector into a structured R list. The
original text is preserved as the "skeleton" attribute for later
writing.

## Usage

``` r
prm_parse(source)
```

## Arguments

- source:

  Either a file path (character of length 1) or a character vector of
  PRM lines (e.g., from
  [`readLines()`](https://rdrr.io/r/base/readLines.html) or
  `ssenv$.ss.params.defaults`).

## Value

A named list of parameter values. Attributes:

- `skeleton`: Original text vector

- `sections`: Named character vector mapping param names to section
  names

- `line_map`: Named integer vector mapping param names to line numbers

## Examples

``` r
if (FALSE) { # \dontrun{
prm <- prm_parse("path/to/file.prm")
prm$CaseFile
# [1] "epid.cas"
} # }
```
