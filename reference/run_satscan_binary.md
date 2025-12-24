# Run SaTScan Binary

Executes the SaTScan binary directly using system commands. Replaces the
reliance on
[`rsatscan::satscan()`](https://rdrr.io/pkg/rsatscan/man/satscan.html).

## Usage

``` r
run_satscan_binary(prm_file, ss_path, verbose = FALSE)
```

## Arguments

- prm_file:

  Path to the input PRM file.

- ss_path:

  Path to the SaTScan executable.

- verbose:

  Logical. If TRUE, prints output to console.

## Value

Invisible TRUE on success. Stops on failure.
