# Read a SaTScan PRM File (Simple)

Parses a SaTScan `.prm` parameter file into a plain named R list. This
is a simpler alternative to
[`prm_parse()`](https://Songyosr.github.io/epidscan/reference/prm_parse.md)
that returns a plain list instead of a `prm_list` object with metadata.

## Usage

``` r
read_prm(path)
```

## Arguments

- path:

  Path to a `.prm` file.

## Value

A named list of SaTScan parameters (e.g.,
`list(AnalysisType = "3", ...)`). Values are returned as character
strings; numeric conversion is left to the caller.

## See also

[`prm_parse`](https://Songyosr.github.io/epidscan/reference/prm_parse.md)
for parsing with full metadata

## Examples

``` r
if (FALSE) { # \dontrun{
prm <- read_prm("path/to/analysis.prm")
prm$AnalysisType
# [1] "3"
} # }
```
