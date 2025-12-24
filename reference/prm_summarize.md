# Summarize Analysis Parameters

Extracts a human-readable summary of the analysis configuration from a
`prm_list`. useful for "pre-flight" checks and result summaries.

## Usage

``` r
prm_summarize(prm)
```

## Arguments

- prm:

  A `prm_list` or named list of parameters.

## Value

A list containing:

- `model`: Human-readable model type (e.g., "Discrete Poisson")

- `analysis_type`: Human-readable analysis type (e.g., "Retrospective
  Space-Time")

- `scan_areas`: Human-readable scan areas (e.g. "High Rates")

- `study_period`: Formatted string "StartDate to EndDate"

- `time_precision`: Time unit (e.g., "Month")

- `spatial_window`: Description of spatial window (e.g., "Circular (Max:
  50% pop)")

- `monte_carlo`: Number of replications
