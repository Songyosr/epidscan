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

### Tidy Workflow (Recommended)

The easiest way to run an analysis is using `epid_satscan` with your data frame or `sf` object:

```r
library(epidscan)

# Prepare your data (e.g., an sf object with cases)
# my_data <- ... 

# Run SatScan directly via pipe
results <- my_data |>
  epid_satscan(
    obs_col = case_count,    # Column with observed cases
    pop_col = population,    # Column with population
    date_col = date,         # Column with date (Date or numeric)
    id_col = district_id,    # Column with unique ID
    type = "space-time",
    time_precision = "Day",  # Optional: "Year", "Month", or "Generic"
    output_dir = "satscan_out" # Optional: Save intermediate files here
  )


# The result is an sf object joined with SatScan clusters
plot(results["recurrence_interval"])
```

### Advanced: Modular Pipeline
For complex data capabilities (like cleaning Thai addresses), use the modular functions:



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
