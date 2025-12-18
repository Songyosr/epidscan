# epidscan

**Tidy Interface for SatScan Cluster Analysis**

`epidscan` provides a pipe-friendly R wrapper for running SatScan spatial and space-time cluster analyses directly on data frames or sf objects.

## Installation

```r
# Install from GitHub
devtools::install_github("Songyosr/epidscan")
```

**Requirements:** [SatScan](https://www.satscan.org/) must be installed on your system.

## Quick Start

```r
library(epidscan)

# 1. Set SatScan path (once per session)
set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")  # macOS
# set_satscan_path("C:/Program Files/SaTScan/SaTScan.exe")          # Windows

# 2. Run analysis
result <- my_data |>
  epid_satscan(
    obs_col = cases,
    pop_col = population,
    date_col = date,
    id_col = location_id,
    lat_col = latitude,
    long_col = longitude
  )

# 3. Filter significant clusters
clusters <- result |> 
  dplyr::filter(!is.na(CLUSTER), P_VALUE < 0.05)
```

## Features

- **Tidy interface** - Works with pipes and data frames
- **sf support** - Auto-extracts geometry from sf objects
- **Auto-detection** - Infers time precision from Date columns
- **Result joining** - Cluster info joined back to original data

## Parameters

| Parameter             | Description                                                    |
| --------------------- | -------------------------------------------------------------- |
| `obs_col`             | Observed case counts (required)                                |
| `pop_col`             | Population counts (optional, for Poisson)                      |
| `date_col`            | Date/time column (optional, for temporal)                      |
| `id_col`              | Location IDs (optional, auto-generated if missing)             |
| `lat_col`, `long_col` | Coordinates (required if not sf)                               |
| `type`                | `"space-time"`, `"purely-spatial"`, `"space-time-permutation"` |
| `model`               | `"poisson"`, `"bernoulli"`, `"space-time-permutation"`         |
| `time_precision`      | `"day"`, `"month"`, `"year"`, `"generic"`, or `NULL` (auto)    |
| `...`                 | Additional SaTScan arguments (e.g. `AnalysisType=1`)           |

## Output

Returns the original data with cluster columns added:

| Column     | Description                          |
| ---------- | ------------------------------------ |
| `CLUSTER`  | Cluster ID (1 = most significant)    |
| `P_VALUE`  | Statistical significance             |
| `REL_RISK` | Relative risk vs. rest of study area |

## Example: Leptospirosis Surveillance

```r
# Load case data with location and population
lepto_data <- readRDS("cases_with_pop.rds")

# Run space-time Poisson analysis
result <- lepto_data |>
  epid_satscan(
    obs_col = cases,
    pop_col = pop,
    date_col = date,
    id_col = tambon_code,
    lat_col = lat,
    long_col = long,
    type = "space-time",
    model = "poisson",
    MonteCarloReps = 999
  )

# View detected clusters
result |>
  dplyr::filter(!is.na(CLUSTER)) |>
  dplyr::distinct(CLUSTER, P_VALUE, REL_RISK)
```

## License

MIT
