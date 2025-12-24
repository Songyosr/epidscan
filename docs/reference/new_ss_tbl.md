# Create a SaTScan Table

Creates a new `ss_tbl` object, which is a `vctrs` subclass of
`data.frame` tailored for SaTScan data.

## Usage

``` r
new_ss_tbl(data, type, roles, spec = list())
```

## Arguments

- data:

  A data.frame.

- type:

  type string (e.g. "cas", "geo").

- roles:

  Named character vector mapping schema roles to data columns.

- spec:

  List of additional specifications.
