# Deployment Guide: Modular SaTScan Pipeline

This guide explains how to migrate the modular SaTScan pipeline to a new server.

## 1. File Manifest
Copy the following files and directories to your server, maintaining the structure:

### Core Scripts
- `run_final_analysis.R` (Main entry point)
- `run_pipeline.R` (Orchestrator)
- `visualize_results.Rmd` (Visualization)

### Modules (Required)
- `R/` (Copy the entire directory)
  - `R/modules/` (Core logic)
  - `R/case_processing/` (Helpers)
  - `R/population_processing/` (Helpers)
  - `R/satscan_config.R` (Configuration)

### Data (Required)
- `data/cases_prepared.rds` (Case data)
- `data/thai_bound/` (Shapefiles for visualization)
  - `tha_admbnda_adm3_rtsd_20220121.shp` (and associated .shx, .dbf, etc.)

### Data (Optional but Recommended)
- `data/population_cache/` (To skip re-downloading/processing population data)
- `data/population/` (Raw population files if cache is not copied)

## 2. Server Requirements

### Software
1.  **R** (Version 4.0+)
2.  **SaTScan** (Standalone software)
    - **Linux**: Install via Wine or native Linux binary (if available/supported).
    - **Windows**: Standard installer.
    - **Mac**: Standard installer.

### R Packages
Install the following packages on the server:
```r
install.packages(c("tidyverse", "sf", "rsatscan", "leaflet", "htmltools", "knitr", "rmarkdown", "kableExtra"))
```

## 3. Configuration

### SaTScan Path
**CRITICAL**: You must update the path to the SaTScan executable in `R/modules/run_satscan.R`.

Open `R/modules/run_satscan.R` and find the `satscan_exe_paths` argument (around line 19) or the `ss_paths` check (around line 144).

**For Windows:**
```r
satscan_exe_paths = c("C:/Program Files/SaTScan/SaTScan.exe")
```

**For Linux (if using Wine):**
You might need to wrap the execution command or point to a wrapper script.

**For Linux (Native):**
```r
satscan_exe_paths = c("/usr/bin/satscan", "/usr/local/bin/satscan")
```

## 4. Execution

1.  Navigate to the project directory.
2.  Run the analysis:
    ```bash
    Rscript run_final_analysis.R
    ```
3.  Generate the report:
    ```bash
    Rscript -e "rmarkdown::render('visualize_results.Rmd')"
    ```

## 5. Troubleshooting
- **"SatScan executable not found"**: Check the path in `R/modules/run_satscan.R`.
- **"Permission denied"**: Ensure the script has write permissions to the `output/` directory.
- **"Missing packages"**: Run the `install.packages` command above.
