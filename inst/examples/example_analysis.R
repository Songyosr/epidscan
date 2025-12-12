#!/usr/bin/env Rscript
# Final Analysis Pipeline - Both Modes
library(tidyverse)
library(sf)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("  FINAL ANALYSIS PIPELINE\n")
cat("  Date Range: 2024-01-01 to 2025-11-30\n")
cat("  Max Cluster Size: 20%\n")
cat("═══════════════════════════════════════════════════════════\n\n")

source("run_pipeline.R")

# Common Parameters
start_date <- "2024-01-01"
end_date <- "2025-11-30"
pop_years <- c(2024, 2025)

# SatScan Configuration
satscan_config <- list(
    MaxSpatialSizeInPopulationAtRisk = 20 # 20%
)

# -----------------------------------------------------------------------------
# 1. Simple Mode
# -----------------------------------------------------------------------------
cat("\n[1/2] Running Simple Mode...\n")
run_pipeline(
    mode = "simple",
    shapefile_path = "data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp",
    province_code = "90",
    case_file = "data/cases_prepared.rds",
    start_date = start_date,
    end_date = end_date,
    pop_years = pop_years,
    update_geo = FALSE,
    update_pop = TRUE,
    update_cases = TRUE,
    run_analysis = TRUE,
    satscan_config = satscan_config
)

# Save to dedicated simple mode directory
cat("\nMoving results to simple mode directory...\n")
dir.create("output/simple_mode", showWarnings = FALSE, recursive = TRUE)
file.copy("output/satscan_results/satscan_results.rds",
    "output/simple_mode/satscan_results_simple.rds",
    overwrite = TRUE
)
file.copy("data/derived/results_sf.rds",
    "output/simple_mode/results_sf_simple.rds",
    overwrite = TRUE
)

# -----------------------------------------------------------------------------
# 2. Stratified Mode
# -----------------------------------------------------------------------------
cat("\n[2/2] Running Stratified Mode...\n")
run_pipeline(
    mode = "stratified",
    shapefile_path = "data/thai_bound/tha_admbnda_adm3_rtsd_20220121.shp",
    province_code = "90",
    case_file = "data/cases_prepared.rds",
    start_date = start_date,
    end_date = end_date,
    pop_years = pop_years,
    strata_vars = c("age_group", "sex"),
    update_geo = FALSE,
    update_pop = TRUE,
    update_cases = TRUE,
    run_analysis = TRUE,
    satscan_config = satscan_config
)

# Save to dedicated stratified mode directory
cat("\nMoving results to stratified mode directory...\n")
dir.create("output/stratified_mode", showWarnings = FALSE, recursive = TRUE)
file.copy("output/satscan_results/satscan_results.rds",
    "output/stratified_mode/satscan_results_stratified.rds",
    overwrite = TRUE
)
file.copy("data/derived/results_sf.rds",
    "output/stratified_mode/results_sf_stratified.rds",
    overwrite = TRUE
)

cat("\n══════════════════════════════════════════════════════════\n")
cat("  FINAL ANALYSIS COMPLETE\n")
cat("══════════════════════════════════════════════════════════\n")
