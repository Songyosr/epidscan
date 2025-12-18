# epidscan 0.2.0

# epidscan 0.1.0

## New Features
* **Tidy API**: New `epid_satscan()` function provides a pipe-friendly interface for SatScan.
* **Simple Inputs**: Directly accepts data frames or `sf` objects. No manual file creation needed.
* **Auto-Configuration**: 
    * Automatically handles `CoordinatesType` (Lat/Long).
    * Auto-detects `StartDate` and `EndDate` from data.
    * Auto-infers `TimeAggregationUnits` from date column type.
* **Result Integration**: Returns the input data joined with SatScan cluster results (`CLUSTER`, `P_VALUE`, `REL_RISK`).

## Improvements
* **Modular Architecture**: Core logic split into `satscan_data_prep.R`, `satscan_options.R`, and `satscan_results.R`.
* **Robustness**: Improved error handling for missing columns and SatScan execution failures.
* **Documentation**: Comprehensive roxygen2 documentation with examples.

## Breaking Changes
* Legacy functions (e.g., `process_cases`, `run_pipeline`) have been moved to `legacy/` and are no longer exported.
