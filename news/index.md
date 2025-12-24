# Changelog

## epidscan 0.1.0

**First release of epidscan** - A modern, stateless R interface for
SaTScan spatial scan statistics.

### Features

#### Core Analysis Engine

- **[`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md)** -
  Stateless analysis engine that accepts validated inputs and returns
  parsed results directly
- **Automatic parameter management** - Three-level hierarchy (data
  defaults → function overrides → template files) for flexible
  configuration
- **Clean workspace** - Results generated in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) by default; use
  `output_dir` to save explicitly

#### Type-Safe Input System

The `ss_tbl` system ensures data is correct before analysis runs:

- **[`ss_cas()`](https://Songyosr.github.io/epidscan/reference/ss_cas.md)** -
  Create validated case files with upfront error checking
  - Zero-case handling
  - Time formatting with precision control (day/month/year/generic)
  - Support for covariates (age groups, sex, etc.)
- **[`ss_geo()`](https://Songyosr.github.io/epidscan/reference/ss_geo.md)** -
  Spatial coordinates with native `sf` support
  - Automatic centroid extraction from polygons
  - CRS detection (geodetic vs. cartesian)
  - Correct coordinate ordering for SaTScan
- **[`ss_pop()`](https://Songyosr.github.io/epidscan/reference/ss_pop.md)** -
  Population denominators
  - Zero-population checks
  - Time-based population with precision control
  - Covariate support matching case files
- **[`ss_ctl()`](https://Songyosr.github.io/epidscan/reference/ss_ctl.md)** -
  Controls for Bernoulli models
- **[`ss_grd()`](https://Songyosr.github.io/epidscan/reference/ss_grd.md)** -
  Custom scan centers

#### Parameter Management

- **[`prm_options()`](https://Songyosr.github.io/epidscan/reference/prm_options.md)** -
  Object-oriented parameter configuration for reproducible analyses
- **[`prm_parse()`](https://Songyosr.github.io/epidscan/reference/prm_parse.md)
  /
  [`prm_write()`](https://Songyosr.github.io/epidscan/reference/prm_write.md)** -
  Robust parser that preserves comments and structure
- **[`prm_validate()`](https://Songyosr.github.io/epidscan/reference/prm_validate.md)** -
  Validate settings against official SaTScan templates before execution

#### Results & Visualization

- **S3 methods** following `broom` conventions:
  - `tidy()` - Cluster-level details as tidy data frames
  - `glance()` - Analysis-level summary metrics
  - [`summary()`](https://rdrr.io/r/base/summary.html) - Human-readable
    cluster summaries
- **[`map_clusters()`](https://Songyosr.github.io/epidscan/reference/map_clusters.md)** -
  Interactive visualization with leaflet
  - Significance-based color coding
  - Rich cluster popups with statistics
  - Support for both cartesian and lat/long data

#### Smart Defaults

- **Automatic date inference** - Time range detected from your data if
  not specified
- **Flexible column handling** - Works with your column names via role
  mapping
- **Spatial-first** - Seamless integration with `sf` simple features

### Included Data

- **New Mexico lung cancer dataset** - Complete example with cases,
  population, and coordinates
  - `NMlung_cas` - Monthly case counts by county, age, and sex
  - `NMlung_pop` - Annual population by county, age, and sex  
  - `NMlung_geo` - County centroids in kilometers

### Documentation

- **Getting Started vignette** - Complete workflow walkthrough
- **Parameter Reference** - Guide to SaTScan parameter configuration
- **Function documentation** - Comprehensive help pages for all
  functions

### Requirements

- R \>= 4.1.0
- SaTScan™ must be installed on your system (free download from
  <https://www.satscan.org/>)
