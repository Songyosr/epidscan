# Write SaTScan File

Writes an ss_tbl to disk in SaTScan ASCII format. Handles column
ordering, time formatting, and whitespace.

## Usage

``` r
write_satscan(x, file, sep = " ", na = "", quote = FALSE)
```

## Arguments

- x:

  An ss_tbl object.

- file:

  Output file path.

- sep:

  Separator (default space).

- na:

  NA string (default "").

- quote:

  Logical (default FALSE).
