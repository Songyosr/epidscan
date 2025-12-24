
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
Using the New Mexico Lung Cancer dataset (included in `epidscan`):

``` r
library(epidscan)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(broom)

# Configure SaTScan path (one time)
ss_path <- "/Applications/SaTScan.app/Contents/app/satscan" # Depend on yours location
set_satscan_path(ss_path)
#> SatScan path set to: /Applications/SaTScan.app/Contents/app/satscan

# Load the New Mexico Lung Cancer data
data(NMlung_cas)
data(NMlung_pop)
data(NMlung_geo)

# Check data
head(NMlung_cas)
#>       county cases       date age_group    sex
#> 1 Bernalillo     1 1973-01-01     40-44   Male
#> 2 Bernalillo     2 1973-01-01     45-49   Male
#> 3 Bernalillo     2 1973-01-01     50-54   Male
#> 4 Bernalillo     1 1973-01-01     50-54 Female
#> 5 Bernalillo     1 1973-01-01     65-69   Male
#> 6 Bernalillo     1 1973-01-01     70-74   Male
head(NMlung_pop)
#>       county year population age_group    sex
#> 1 Bernalillo 1973        469        <5   Male
#> 2 Bernalillo 1973        478        <5 Female
#> 3 Bernalillo 1973      14502        <5   Male
#> 4 Bernalillo 1973      13888        <5 Female
#> 5 Bernalillo 1973        566        <5   Male
#> 6 Bernalillo 1973        565        <5 Female
head(NMlung_geo)
#>       county x_km y_km
#> 1 Bernalillo  219  338
#> 2     Catron   27  189
#> 3     Chaves  418  156
#> 4     Colfax  408  538
#> 5      Curry  534  262
#> 6     DeBaca  438  272

# Prepare inputs
# Case File: Monthly data with Age/Sex covariates
cas <- ss_cas(
  NMlung_cas,
  loc_id = "county",
  cases = "cases",
  time = "date",
  time_precision = "month",
  covars = c("age_group", "sex")
)

# Population File: Yearly census data
pop <- ss_pop(
  NMlung_pop,
  loc_id = "county",
  time = "year",
  population = "population",
  time_precision = "year",
  covars = c("age_group", "sex")
)

# Coordinates File: Cartesian (km)
geo <- ss_geo(
  NMlung_geo,
  loc_id = "county",
  coord1 = "x_km",
  coord2 = "y_km",
  coord_type = "cartesian"
)

# Run analysis
# Create parameter object (Auto-detects SaTScan version)
my_prm <- prm_options(
  AnalysisType = 3, # RetroSpective Space-time
  ModelType = 0, # Poisson
  TimeAggregationUnits = 3, # Month
  MonteCarloReps = 999
)

# Preview configuration
print(my_prm)
#> ── SaTScan Analysis Summary ──────────────────────────────────
#> Model: Discrete Poisson [High Rates]
#> Scan:  Retrospective Space-Time
#> Time:  2000/1/1 to 2000/12/31 (Year)
#> Space: Circular (Max: 50% pop)
#> Sims:  999 Monte Carlo Reps
#> ──────────────────────────────────────────────────────────────
#> 
#> (plus 146 other parameters. Use as.list() to view all)

result <- satscanr(
  cas = cas,
  geo = geo,
  pop = pop,
  prm = my_prm,
  output_dir = "example_output"
)
#> Warning in value[[3L]](cond): SaTScan execution failed: SaTScan execution failed with exit code: 1
#> 
#> --- SaTScan Output (Tail) ---
#> The shapefiles option is not available for Cartesian coordinates.
#> The option was disabled.
#> Parameter Setting Warning:
#> The Google Maps option is not available for Cartesian coordinates.
#> The option was disabled.
#> Error: Unknown identifier in population file, record 66115. '' was not specified in the coordinates file.
#> Error: Record 66115 of the population file is missing the date.
#> Please see the 'population file' section in the user guide for help.
#> 
#> Problem encountered when reading the data from the input files.
#> 
#> Use '--help' to get help with program options.
#> You are running SaTScan v10.2.5.
#> 
#> SaTScan is free, available for download from https://www.satscan.org/.
#> It may be used free of charge as long as proper citations are given
#> to both the SaTScan software and the underlying statistical methodology.
#> 
#> Reading the coordinates file
#> Reading the population file
#> ------------------------------

# Examine results
summary(result)
#> SaTScan Results Summary
#> =======================
#> 
#> ── SaTScan Analysis Summary ──────────────────────────────────
#> Model: Discrete Poisson [High Rates]
#> Scan:  Retrospective Space-Time
#> Time:  2000/1/1 to 2000/12/31 (Month)
#> Space: Circular (Max: 50% pop)
#> Sims:  999 Monte Carlo Reps
#> ──────────────────────────────────────────────────────────────
#> 
#> Overview:
#>   Clusters detected:  0 (0 significant at p < 0.05)
#>   Locations analyzed: 32 (0 in clusters)
#> 
#> 
#> (Raw SaTScan text available. Use print(result, raw = TRUE) to view)

library(broom)
tidy(result)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: cluster <int>, p_value <dbl>

glance(result)
#> # A tibble: 1 × 13
#>   n_clusters n_significant n_locations n_in_clusters min_p_value
#>        <int>         <int>       <int>         <int>       <dbl>
#> 1          0             0          32             0          NA
#> # ℹ 8 more variables: max_relative_risk <dbl>, max_llr <dbl>,
#> #   prop_in_clusters <dbl>, total_observed <dbl>, total_population <dbl>,
#> #   model <chr>, analysis_type <chr>, monte_carlo_reps <int>
```

``` r
# simple = TRUE for Cartesian/blank canvas mapping
map_clusters(result, simple = TRUE)
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

# Visualize clusters
# The NM dataset uses arbitrary X/Y coordinates (named lat/long).
# We set simple = TRUE to plot them on a blank canvas (Simple CRS).
map_clusters(result, simple = TRUE)
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
