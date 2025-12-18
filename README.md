# epidscan

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Modern, Stateless Interface for SaTScan Cluster Analysis**

`epidscan` is a robust R interface for the [SaTScan™](https://www.satscan.org/) spatial scan statistic. It is designed to be **stateless**, **type-safe**, and **pipe-friendly**, addressing the limitations of legacy wrappers like `rsatscan`.

## Why `epidscan`?

| Feature | `rsatscan` | `epidscan` |
| :--- | :--- | :--- |
| **Philosophy** | **Stateful**: Relies on hidden global environment (`ssenv`) | **Stateless**: Pure functions, no side-effects |
| **Data Input** | Manual file prep or loose data frames | **Type-Safe**: Validated `ss_tbl` objects |
| **Spatial** | Manual coordinates | **Native `sf`**: Auto-extracts centroids & CRS |
| **Parameters** | Fragile `ss.options()` list | **Smart Hierarchy**: Immutable Data > User Tweaks > Templates |
| **Reliability** | "Silent Failures" common | **Pre-flight Checks**: `prm_validate()` & Date Inference |

## Installation

```r
# Install from GitHub
devtools::install_github("Songyosr/epidscan")
```

**Requirements:** [SaTScan™](https://www.satscan.org/) must be installed on your system.

## Quick Start

The workflow follows a clean **Prep -> Run -> Analyze** pattern.

### 1. Setup (Once)

```r
library(epidscan)
library(dplyr)

# Set path to your SaTScan executable
set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")
```

### 2. Prepare Data (`ss_tbl`)

Use `as_satscan_*` functions to create strongly-typed inputs. These helpers validate your data and handle the complex formatting required by SaTScan (e.g., date formats, coordinate ordering).

```r
# Case File: Zero-case rows are automatically handled
cas <- as_satscan_case(
  cases_df, 
  loc_id = "zipcode", 
  cases = "cases", 
  time = "date"
) 

# Geometry: Works directly with sf objects!
# Auto-detects if you are using Lat/Long or Cartesian (Projected)
geo <- as_satscan_coordinates(
  shapefile_sf, 
  loc_id = "zipcode"
)

# Population (Optional)
pop <- as_satscan_population(
  pop_df, 
  loc_id = "zipcode", 
  time = "year", 
  population = "pop_count"
)
```

### 3. Run Analysis (`satscanr`)

The `satscanr` function is the pure engine. It takes your `ss_tbl` inputs and returns a structured result.

```r
res <- satscanr(
  cas = cas, 
  pop = pop, 
  geo = geo, 
  AnalysisType = 3,         # 3 = Space-Time
  ModelType = 0,            # 0 = Poisson
  MonteCarloReps = 999,
  verbose = TRUE
)
```

### 4. Explore Results

The result object contains everything you need, already parsed and joined.

```r
# Tidy summary of clusters
print(res$cluster_summary)

# Join results back to your map for visualization
library(ggplot2)
res$shapefile |>
  ggplot(aes(fill = factor(CLUSTER))) +
  geom_sf() +
  labs(title = "Detected Clusters")
```

## Advanced Features

### Parameter Templates (The "Smart Tweak" Model)
Instead of setting 50 parameters manually, use a template and tweak only what you need.

```r
# Load a template, but override the max cluster size
res <- satscanr(cas, geo,
  prm_path = "standard_analysis.prm",
  MaxSpatialSizeInPopulationAtRisk = 25
)
```

### Data Integrity
`epidscan` protects you from common errors:
*   **Time Precision**: If your data is daily but you asked for monthly aggregation, `epidscan` validates this before running.
*   **Coordinate Systems**: It prevents sending Lat/Long data to a Cartesian model.

## License

MIT
