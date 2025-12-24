
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epidscan

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A modern R interface for [SaTScan™](https://www.satscan.org/) spatial
scan statistics. Provides type-safe data structures, parameter
management, and native spatial data support for epidemiological cluster
detection.

## Design Principles

`epidscan` is built around a few core ideas:

**Stateless by design:** No hidden global state. Every function call is
independent and reproducible. Input validation happens upfront rather
than at runtime.

**Type-safe inputs:** The `ss_tbl` system validates data structure
before analysis runs. Location IDs, coordinates, and time formats are
checked at construction time.

**Spatial-first:** Native support for `sf` objects. Coordinate system
detection and ordering handled automatically.

**Parameter transparency:** Three-level parameter hierarchy (data
defaults → function overrides → template files) makes configuration
explicit and traceable.

## Installation

``` r
# Install from GitHub
pak::pak("Songyosr/epidscan")
```

**Requirements:** [SaTScan™](https://www.satscan.org/) must be installed
on your system.

## Example

Using the New Mexico brain cancer dataset (distributed with SaTScan):

``` r
library(epidscan)
library(dplyr)
library(broom)

# Configure SaTScan path (one time)
ss_path <- "/Applications/SaTScan.app/Contents/app/satscan" # Depend on yours location
set_satscan_path(ss_path)

# Load the classic NM brain cancer example data
# This dataset is distributed with SaTScan and included in the rsatscan package
data(NMcas, NMpop, NMgeo, package = "rsatscan")

# Check data
head(NMcas)
head(NMpop)
head(NMgeo) # The real data are actually X/Y not Lat/Long

# Prepare inputs
cas <- ss_cas(
  NMcas,
  loc_id = "county",
  cases = "cases",
  time = "year",
  time_precision = "year"
)

pop <- ss_pop(
  NMpop,
  loc_id = "county",
  time = "year",
  population = "population",
  time_precision = "year"
)

geo <- ss_geo(
  NMgeo,
  loc_id = "county",
  coord1 = "long",
  coord2 = "lat",
  coord_type = "cartesian"
)

# Run analysis
# Create parameter object (Auto-detects SaTScan version)
my_prm <- prm_options(
  AnalysisType = 3, # RetroSpective Space-time
  ModelType = 0, # Poisson
  TimeAggregationUnits = 4, # Year
  MonteCarloReps = 999
)

# Preview configuration
print(my_prm)
#> ── SaTScan Analysis Summary ──────────────────────────────────
#> Model: Discrete Poisson [High Rates]
#> Scan:  Retrospective Space-Time
#> Time:  ? to ? (Year)
#> Space: Circular (Max: 50% pop)
#> Sims:  999 Monte Carlo Reps
#> ──────────────────────────────────────────────────────────────

result <- satscanr(
  cas = cas,
  geo = geo,
  pop = pop,
  prm = my_prm,
  #verbose = TRUE,
  output_dir = "legacy/example/"
)

# Examine results
print(result)
#> SaTScan Analysis Results
#> ========================
#> Model: Poisson (Discrete)
#> Analysis Type: Space-Time Retrospective
#>
#> Clusters Detected: 5
#> Significant Clusters (p < 0.05): 2

library(broom)
tidy(result)
#> # A tibble: 5 × 11
#>   cluster observed expected relative_risk log_likelihood p_value radius
#>     <int>    <dbl>    <dbl>         <dbl>          <dbl>   <dbl>  <dbl>
#> 1       1      129     57.1          2.26           23.7   0.001   48.2
#> 2       2       46     19.0          2.42           10.4   0.021   39.1

glance(result)
#> # A tibble: 1 × 7
#>   n_clusters n_significant n_locations max_relative_risk min_p_value
#>        <int>         <int>       <int>             <dbl>       <dbl>
#> 1          5             2          32              2.42       0.001
```

## Core Workflow

### 1. Create validated inputs

The `ss_*()` functions construct type-safe input objects:

``` r
# Case file
cas <- ss_cas(
  data,
  loc_id = "location_id",
  cases = "case_count",
  time = "onset_date",
  time_precision = "day" # "day", "month", "year", or "generic"
)

# Coordinates file (works with sf objects or data frames)
geo <- ss_geo(
  shapefile_sf,
  loc_id = "location_id"
  # Centroids extracted automatically
  # CRS detected and coordinates ordered correctly for SaTScan
)

# Population file
pop <- ss_pop(
  population_data,
  loc_id = "location_id",
  time = "census_year",
  population = "pop_count",
  time_precision = "year"
)
```

### 2. Configure & Run

``` r
# New: Object-Oriented Configuration
opts <- prm_options(
  AnalysisType = 3, # Space-time
  MonteCarloReps = 999,
  MaxSpatialSizeInPopulation = 50
)

result <- satscanr(
  cas,
  geo = geo,
  pop = pop,
  prm = opts
)
```

The `satscanr()` function: - Writes SaTScan input files (`.cas`, `.geo`,
`.pop`) - Constructs parameter file using three-level hierarchy -
Executes SaTScan - Parses results into structured R objects

### 3. Work with results

Results implement broom methods for tidy analysis:

``` r
# Cluster-level details
tidy(result)

# Analysis-level summary
glance(result)

# Interactive map (requires leaflet)
map_clusters(result, geo_data = shapefile)
```

## Key Features

### The `ss_tbl` System

Input validation at construction time:

``` r
cas <- ss_cas(data, loc_id = "id", cases = "n", time = "date")
class(cas)
#> [1] "ss_tbl"      "data.frame"

print(cas)
#> <ss_tbl: cas>
#> Roles:
#>   - loc_id -> id
#>   - cases -> n
#>   - time -> date
#> Spec:
#>   - time_precision: day
#> Data: 1,234 rows x 3 cols
```

Features: - Zero-case rows automatically excluded - Time formatting
deferred to write-time based on precision - Column role mappings
explicit and inspectable

### Spatial Data Support

Native `sf` integration:

``` r
library(sf)

# From sf object
geo <- ss_geo(my_shapefile, loc_id = "GEOID")
# → Extracts centroids
# → Detects coordinate system (lat/long vs projected)
# → Orders coordinates correctly for SaTScan

# From data frame
geo <- ss_geo(
  coords_df,
  loc_id = "id",
  coord1 = "longitude",
  coord2 = "latitude",
  coord_type = "latlong" # or "cartesian", or "auto"
)
```

Coordinate ordering is handled automatically: - Lat/Long: SaTScan
expects Latitude, Longitude (Y, X) - Cartesian: SaTScan expects X, Y

### Parameter Management

### Parameter Management

Flexible configuration options:

``` r
# Method A: Object-Oriented (Recommended)
# - Auto-detects SaTScan version
# - Validates parameter names
# - Reusable across runs
my_opts <- prm_options(AnalysisType = 3, MonteCarloReps = 999)
satscanr(cas, geo, prm = my_opts)

# Method B: Quick Tweaks (Legacy/Interactive)
# - Good for single runs or quick checks
satscanr(cas, geo, AnalysisType = 3)

# Method C: Template Files (Replication)
# - Exact reproduction of GUI or previous runs
satscanr(cas, geo, prm_path = "space_time_template.prm")
```

Parameter validation:

``` r
# Validate against SaTScan's official templates
params <- list(
  AnalysisType = 3,
  ModelType = 1,
  MonteCarloReps = 999
)

prm_validate(params)
#> ✓ All parameters valid
#> ℹ Using template: Poisson space-time
```

## Available Input Types

| Function   | File Type | Purpose                                |
|------------|-----------|----------------------------------------|
| `ss_cas()` | `.cas`    | Case counts by location/time           |
| `ss_pop()` | `.pop`    | Population denominators                |
| `ss_geo()` | `.geo`    | Spatial coordinates                    |
| `ss_ctl()` | `.ctl`    | Controls (Bernoulli models)            |
| `ss_grd()` | `.grd`    | Custom scan centers                    |
| `ss_nwk()` | `.nwk`    | Network edges (road networks)          |
| `ss_nbr()` | `.nbr`    | Neighbor rankings (wide format)        |
| `ss_met()` | `.met`    | Meta-locations (hierarchical grouping) |

## Documentation

- [Getting Started](vignettes/getting-started.html) - Complete workflow
  examples
- [Parameter Reference](vignettes/satscan-parameters.html) - All SaTScan
  parameters
- [Spatial Data Guide](vignettes/spatial-data.html) - Working with sf
  and coordinates
- [Report Issues](https://github.com/Songyosr/epidscan/issues)

## Development Status

This package is under active development (lifecycle: experimental). The
API is stabilizing but may change in response to user feedback and
testing.

## Citation

``` r
citation("epidscan")

# Please also cite SaTScan:
# Kulldorff M. (1997). A spatial scan statistic. 
# Communications in Statistics: Theory and Methods, 26:1481-1496.
```

## License

MIT © Songyos Rajborirug
