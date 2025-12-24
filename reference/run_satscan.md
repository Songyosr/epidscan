# Run SatScan Analysis

Executes SatScan by calling the binary directly and parsing the output
files. Use
[`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md)
as the main entry point.

## Usage

``` r
run_satscan(
  work_dir,
  project_name = "epid",
  ss_location,
  ss_batch,
  verbose = FALSE
)
```

## Arguments

- work_dir:

  Working directory with input files.

- project_name:

  Project name (default: "epid"). This matches the inputs
  CaseFile=epid.cas etc.

- ss_location:

  Path to the folder containing the SaTScan executable (Backwards
  compatibility param).

- ss_batch:

  Name of the SaTScan executable (Backwards compatibility param).

- verbose:

  Print output?

## Value

satscan result object
