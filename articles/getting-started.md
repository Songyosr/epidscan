# Getting Started with epidscan

`epidscan` is a modern R interface for **Spatio-temporal scan
statistics**, designed to bring modern data science practices to
epidemiological cluster detection. While primarily built as a robust
front-end for [SaTScan™](https://www.satscan.org/), its architecture
allows for future expansion to other methods.

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

## Core Workflow

### 1. Create validated inputs

The `ss_*()` functions construct type-safe input objects. This step
ensures your data is correct before you ever run an analysis.

``` r
library(epidscan)
library(dplyr)

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

You can configure the analysis using a flexible object-oriented approach
or simple overrides.

``` r
# Object-Oriented Configuration (Recommended)
opts <- prm_options(
  AnalysisType = 3, # Space-time
  MonteCarloReps = 999,
  MaxSpatialSizeInPopulation = 50
)

# Run Analysis
result <- satscanr(
  cas,
  geo = geo,
  pop = pop,
  prm = opts
)
```

The
[`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md)
function handles the messy work: - Writes temporary SaTScan input files
(`.cas`, `.geo`, `.pop`). - Constructs a precise parameter file using
the three-level hierarchy. - Executes the SaTScan binary. - Parses the
results back into structured R objects.

### 3. Work with results

Results are returned as S3 objects with support for the `broom` package,
making it easy to harness the results for further analysis or
visualization.

``` r
library(broom)

# Cluster-level details (tidy data frame)
tidy(result)

# Analysis-level summary (metrics like max likelihood)
glance(result)

# Visualize clusters
# For Cartesian data or blank canvas mapping:
map_clusters(result, simple = TRUE)
```

## Key Features Deep Dive

### The `ss_tbl` System

We strictly validate inputs at construction time to prevent runtime
errors in the external SaTScan executable.

``` r
cas <- ss_cas(data, loc_id = "id", cases = "n", time = "date")
class(cas)
# "ss_tbl" "data.frame"
```

Features: - **Zero-case/population checks**: Rows with zero counts are
handled appropriately. - **Time formatting**: We store raw times and
format them correctly for SaTScan (YYYY/MM/DD) only when writing files,
respecting your specified `time_precision`. - **Role tracking**:
Metadata about which column represents `cases`, `time`, etc., is
preserved.

### Spatial Data Support

`epidscan` loves `sf`.

``` r
library(sf)

# From sf object
geo <- ss_geo(my_shapefile, loc_id = "GEOID")
```

- **Centroids**: Automatically extracts centroids from polygons.
- **Projections**: Detects if your data is Latitude/Longitude (Geodetic)
  or Projected (Cartesian).
- **Ordering**: SaTScan is picky (Lat/Long vs X/Y). We automatically
  order columns correctly based on the detected type.

### Parameter Management

We use a “Level 3 Hierarchy” to resolve parameters: 1. **Level 1 (Data
Defaults)**: Derived from your input data (e.g., file names, date ranges
inferred from data). 2. **Level 2 (Function Overrides)**: Arguments
passed directly to
[`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md)
or
[`prm_options()`](https://Songyosr.github.io/epidscan/reference/prm_options.md)
(e.g., `AnalysisType=3`). 3. **Level 3 (Templates)**: A `.prm` file you
provide (e.g., `prm_path = "template.prm"`).

Overrides take precedence! `AnalysisType` in
[`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md)
will overwrite `AnalysisType` in your template.

## Full Example: New Mexico Lung Cancer

This example uses the dataset distributed with this package.

``` r
library(epidscan)
library(dplyr)
library(broom)

# 1. Setup
# ss_path <- "/Applications/SaTScan.app/Contents/app/satscan" 
# set_satscan_path(ss_path)

# 2. Load Data
data(NMlung_cas)
data(NMlung_pop)
data(NMlung_geo)

# 3. Prepare Inputs
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

# 4. Configure & Run
my_prm <- prm_options(
  AnalysisType = 3, # Retrospective Space-time
  ModelType = 0,    # Poisson
  TimeAggregationUnits = 3, # Month
  MonteCarloReps = 999,
  ReportGiniClusters = "y"
)

# (This requires SaTScan to be installed and path set)
# result <- satscanr(
#   cas = cas,
#   geo = geo,
#   pop = pop,
#   prm = my_prm,
#   output_dir = tempdir()
# )

# 5. Inspect (Mock output)
# summary(result)
```
