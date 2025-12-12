#' Post-Processing Module
#'
#' This script processes SatScan results by merging them with the geographic lookup.
#'
#' @param results_dir Directory containing SatScan output files
#' @param geo_lookup_path Path to the geographic lookup RDS file
#' @param output_dir Directory to save the final SF object
#'
#' @return The processed sf object (invisibly)
#'
#' @importFrom dplyr mutate select rename left_join %>%
#' @importFrom sf read_sf
#' @importFrom utils write.csv
#' @importFrom rsatscan satscan
#' @export

post_process <- function(results_dir = "output/satscan_results",
                         geo_lookup_path = "data/derived/geo_lookup.rds",
                         output_dir = "data/derived") {
    message("=== Post-Processing Results ===")

    # 1. Load Geo Lookup
    geo_sf <- readRDS(geo_lookup_path)

    # 2. Read SatScan Results from RDS file
    results_file <- file.path(results_dir, "satscan_results.rds")

    if (!file.exists(results_file)) {
        warning("No SatScan results found at: ", results_file)
        return(NULL)
    }

    satscan_res <- readRDS(results_file)

    # Extract tables from rsatscan object
    clusters <- satscan_res$col # Cluster summary
    gis <- satscan_res$gis # Location details (cluster membership)
    rr <- satscan_res$rr # Tambon-level relative risk

    if (is.null(clusters) || nrow(clusters) == 0) {
        message("No clusters detected in results.")
        # Still return all tambons with NA indicators
        map_data <- geo_sf %>%
            mutate(
                location_id = ADM3_PCODE,
                in_cluster = FALSE,
                CLUSTER = NA_integer_,
                CLUSTER_RR = NA_real_,
                LOC_RR = NA_real_,
                LOC_OBS = NA_integer_,
                LOC_EXP = NA_real_,
                P_VALUE = NA_real_,
                significant = FALSE
            )
        saveRDS(map_data, file.path(output_dir, "results_sf.rds"))
        message("Saved baseline map (no clusters) to: ", file.path(output_dir, "results_sf.rds"))
        return(map_data)
    }

    # 3. Prepare cluster data
    # Get tambon-level RR from rr table (baseline risk across full time period)
    tambon_rr <- rr %>%
        select(LOC_ID, OBSERVED, EXPECTED, REL_RISK) %>%
        rename(
            location_id = LOC_ID,
            TAMBON_OBS = OBSERVED,
            TAMBON_EXP = EXPECTED,
            TAMBON_RR = REL_RISK
        )

    # Get cluster-level info with significance flag
    cluster_info <- clusters %>%
        mutate(significant = P_VALUE < 0.05) %>%
        select(CLUSTER, REL_RISK, significant, SPAN, START_DATE, END_DATE, P_VALUE, RADIUS, EXPECTED, OBSERVED, POPULATION) %>%
        rename(CLUSTER_RR = REL_RISK, CLUSTER_EXP = EXPECTED, CLUSTER_OBS = OBSERVED, CLUSTER_POP = POPULATION)

    # Get cluster membership with location-specific stats
    cluster_locs <- gis %>%
        select(-any_of("P_VALUE")) %>%
        left_join(cluster_info, by = "CLUSTER") %>%
        rename(location_id = LOC_ID)

    # --- NEW: Create Tabular Summary (Non-Spatial) ---
    # Join tambon baseline with cluster info
    # Note: tambon_rr contains all locations analyzed by SatScan
    results_tab <- tambon_rr %>%
        left_join(cluster_locs, by = "location_id") %>%
        mutate(
            in_cluster = !is.na(CLUSTER),
            cluster_sig = ifelse(is.na(significant), FALSE, significant)
        )

    # Save Tabular Summary
    write.csv(results_tab, file.path(output_dir, "results_summary.csv"), row.names = FALSE)
    message("Saved tabular summary to: ", file.path(output_dir, "results_summary.csv"))

    # 4. Join with shapefile - START WITH ALL TAMBONS
    map_data <- geo_sf %>%
        mutate(location_id = gsub("^TH", "", ADM3_PCODE)) %>% # Strip TH prefix to match SatScan format
        left_join(results_tab, by = "location_id")

    # Save result
    saveRDS(map_data, file.path(output_dir, "results_sf.rds"))
    message("Saved results SF to: ", file.path(output_dir, "results_sf.rds"))
    message("  Total tambons: ", nrow(map_data))
    message("  In clusters: ", sum(map_data$in_cluster, na.rm = TRUE))
    message("  Significant clusters: ", length(unique(map_data$CLUSTER[map_data$cluster_sig & !is.na(map_data$CLUSTER)])))

    return(map_data)
}


