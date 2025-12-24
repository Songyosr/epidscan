# Create a SaTScan Table Object

Constructs a validated `satscan_table` object from a data frame. These
objects are used as inputs to
[`satscanr`](https://Songyosr.github.io/epidscan/reference/satscanr.md).

## Usage

``` r
satscan_table(data, kind, spec = list())
```

## Arguments

- data:

  A data.frame containing the table data.

- kind:

  Character. Type of SaTScan file:

  - `"cas"`: Case file

  - `"pop"`: Population file

  - `"geo"`: Coordinates/geometry file

  - `"ctl"`: Control file (Bernoulli model)

  - `"grd"`: Grid file (custom scan centers)

- spec:

  List of metadata about the table (e.g., time_precision, coord_type).

## Value

A validated `satscan_table` object with components:

- `data`: The input data.frame

- `kind`: The table type

- `spec`: Metadata list

## Details

In most cases, you should use the helper functions
[`prep_cas`](https://Songyosr.github.io/epidscan/reference/prep_cas.md),
[`prep_pop`](https://Songyosr.github.io/epidscan/reference/prep_pop.md),
[`prep_geo`](https://Songyosr.github.io/epidscan/reference/prep_geo.md),
[`prep_ctl`](https://Songyosr.github.io/epidscan/reference/prep_ctl.md),
or
[`prep_grd`](https://Songyosr.github.io/epidscan/reference/prep_grd.md)
to create satscan_table objects, as they handle data formatting and
validation.

This function is primarily for advanced users who need to create custom
table types or bypass the prep\_\* helpers.

## See also

[`prep_cas`](https://Songyosr.github.io/epidscan/reference/prep_cas.md),
[`prep_geo`](https://Songyosr.github.io/epidscan/reference/prep_geo.md),
[`satscanr`](https://Songyosr.github.io/epidscan/reference/satscanr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a simple case table manually
df <- data.frame(
    loc_id = c("A", "B"),
    cases = c(5, 10)
)
cas_table <- satscan_table(df, kind = "cas", spec = list(time_precision = "generic"))
print(cas_table)

# Preferred: use prep_cas() instead
cas_table2 <- prep_cas(df, loc_id = loc_id, cases = cases, style = "aggregated")
} # }
```
