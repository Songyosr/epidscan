# epidscan

**Modular SatScan Pipeline for Epidemiological Analysis**

`epidscan` provides a structured, modular framework for preparing data and running SatScan analyses for disease surveillance in R.

## Installation

You can install the package directly from the source:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install epidscan
devtools::install()
```

## Setup

Before running an analysis, you must point `epidscan` to your SatScan executable:

```r
library(epidscan)

# macOS Example
set_satscan_path("/Applications/SaTScan.app/Contents/MacOS/SaTScan")

# Windows Example
# set_satscan_path("C:/Program Files/SaTScan/SaTScan.exe")
```

## Usage

### Run Full Pipeline
The `run_pipeline` function orchestrates the entire process:

```r
run_pipeline(
  mode = "simple",
  shapefile_path = "data/shapefiles/adm3.shp",
  case_file = "data/cases.rds",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  pop_years = 2024
)
```

### Use Individual Modules
You can also use specific modules for custom workflows:

```r
# Process case data only
cases <- process_cases(
    case_file = "cases.rds",
    output_dir = "data/derived",
    start_date = "2024-01-01", 
    end_date = "2024-12-31"
)

# Run SatScan on pre-prepared files
run_satscan_module(
    input_dir = "data/derived",
    start_date = "2024-01-01",
    end_date = "2024-12-31"
)
```
