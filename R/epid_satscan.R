#' Tidy Interface for SatScan Analysis
#'
#' A pipe-friendly wrapper for running SatScan analyses directly on data frames or sf objects.
#' Abstracts away the file creation process.
#'
#' @param data Input data. Can be an \code{sf} object or a data.frame.
#' @param obs_col Column containing observed case counts (unquoted).
#' @param pop_col Column containing population counts (unquoted, optional).
#' @param date_col Column containing time/dates (unquoted, optional).
#' @param id_col Column containing unique location IDs (unquoted). If missing, will attempt to use row numbers or generate IDs.
#' @param lat_col Column containing latitude (unquoted, required if data is not sf).
#' @param long_col Column containing longitude (unquoted, required if data is not sf).
#' @param covariates List of columns to be used as covariates (unquoted).
#' @param type Analysis type: "space-time", "purely-spatial", "space-time-permutation", etc.
#' @param model Model type: "poisson", "bernoulli", "space-time-permutation", etc.
#' @param time_precision Unit of time: "Day", "Month", "Year", "Generic".
#' @param output_dir Directory to save SatScan input/output files. If NULL (default), uses a temporary directory.
#' @param base_prm Path to an existing .prm file to use as a template.
#' @param verbose Logical. Print SatScan output to console?
#' @param ... Additional arguments passed to \code{rsatscan::ss.options}.
#'
#' @return An sf object containing the results joined with the input geometry.
#' @importFrom rlang enquo eval_tidy quo_is_null as_name enquos
#' @importFrom sf st_centroid st_coordinates st_drop_geometry st_as_sf
#' @importFrom dplyr select rename mutate left_join distinct pull as_tibble group_by summarize ungroup all_of
#' @importFrom utils write.csv write.table modifyList read.table
#' @importFrom rsatscan write.ss.prm
#' @export
epid_satscan <- function(data,
                         obs_col,
                         pop_col = NULL,
                         date_col = NULL,
                         id_col = NULL,
                         lat_col = NULL,
                         long_col = NULL,
                         covariates = NULL,
                         type = "space-time",
                         model = "poisson",
                         time_precision = NULL,
                         output_dir = NULL,
                         base_prm = NULL,
                         verbose = FALSE,
                         ...) {

  # 1. Capture arguments (Tidy Evaluation)
  obs_quo <- rlang::enquo(obs_col)
  pop_quo <- rlang::enquo(pop_col)
  date_quo <- rlang::enquo(date_col)
  id_quo <- rlang::enquo(id_col)
  lat_quo <- rlang::enquo(lat_col)
  long_quo <- rlang::enquo(long_col)
  
  # 2. Geometry Prep
  is_sf <- inherits(data, "sf")

  if (is_sf) {
    # Extract coords from centroids
    centroids <- sf::st_centroid(data)
    coords <- sf::st_coordinates(centroids)

    geo_df <- data.frame(
      lat = coords[, 2], # Y
      long = coords[, 1] # X
    )

  } else {
    # Extract coords from columns
    if (rlang::quo_is_null(lat_quo) || rlang::quo_is_null(long_quo)) {
      stop("If data is not an sf object, lat_col and long_col must be provided.")
    }

    geo_df <- data |>
      dplyr::select(
        lat = !!lat_quo,
        long = !!long_quo
      )
  }

  # 3. ID Prep
  if (rlang::quo_is_null(id_quo)) {
    message("No id_col provided. Generating internal IDs.")
    data$epid_id <- seq_len(nrow(data))
    id_vec <- data$epid_id
  } else {
    id_vec <- data |> dplyr::pull(!!id_quo)
  }

  geo_df$id <- id_vec
  
  # Deduplicate Geo by ID
  geo_file_df <- geo_df |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::select(id, lat, long)

  # 4. Data Extraction for .cas / .pop
  export_df <- data.frame(id = id_vec)
  
  if (!rlang::quo_is_null(obs_quo)) {
     export_df$cases <- rlang::eval_tidy(obs_quo, data)
  } else {
     stop("obs_col is required.")
  }
  
  if (!rlang::quo_is_null(date_quo)) {
    export_df$date <- rlang::eval_tidy(date_quo, data)
  }
  
  if (!rlang::quo_is_null(pop_quo)) {
    export_df$pop <- rlang::eval_tidy(pop_quo, data)
  }
  
  # Handle Covariates
  cov_mat <- NULL
  if (!is.null(covariates)) {
     if (is.character(covariates)) {
         cov_mat <- data |> dplyr::select(dplyr::all_of(covariates))
     } else {
         try_cov <- tryCatch({
             data |> dplyr::select({{covariates}})
         }, error = function(e) NULL)
         
         if (!is.null(try_cov)) cov_mat <- try_cov
     }
  }
  
  # 5. File Writing
  # Determine Output Path
  if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
      }
      work_dir <- output_dir
  } else {
      work_dir <- tempdir()
  }
  
  cas_file <- file.path(work_dir, "epid.cas")
  geo_file <- file.path(work_dir, "epid.geo")
  pop_file <- file.path(work_dir, "epid.pop")
  out_file <- file.path(work_dir, "run_results") # avoid 'epid' prefix conflict if weird
  
  # Write Geo
  write.table(geo_file_df, geo_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Write Case
  cas_write_df <- export_df |> dplyr::select(id, cases)
  if ("date" %in% names(export_df)) cas_write_df$date <- export_df$date
  if (!is.null(cov_mat)) cas_write_df <- cbind(cas_write_df, cov_mat)
  
  write.table(cas_write_df, cas_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Write Pop (if exists)
  if ("pop" %in% names(export_df)) {
      pop_write_df <- export_df |> dplyr::select(id)
      if ("date" %in% names(export_df)) pop_write_df$date <- export_df$date
      pop_write_df$pop <- export_df$pop
      if (!is.null(cov_mat)) pop_write_df <- cbind(pop_write_df, cov_mat)
      
      write.table(pop_write_df, pop_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  
  # 6. Configure SatScan Options
  
  # Determine Time Precision
  if (is.null(time_precision)) {
      if ("date" %in% names(export_df)) {
          d <- export_df$date[1]
          if (inherits(d, "Date") || inherits(d, "POSIXt")) {
              time_precision <- 3 # Day
          } else {
              time_precision <- 0 # Generic (numeric/none)
          }
      } else {
          time_precision <- 0 # None
      }
  } else if (is.character(time_precision)) {
      time_precision <- switch(tolower(time_precision),
          "year" = 1,
          "month" = 2,
          "day" = 3,
          "generic" = 0,
          0
      )
  }
  
  clean_ss_options <- list(
    CaseFile = cas_file,
    CoordinatesFile = geo_file,
    CoordinatesType = 1, # Lat/Long (CRITICAL - must be 1!)
    PrecisionCaseTimes = time_precision,
    TimeAggregationUnits = time_precision,  # Must match case time precision
    AnalysisType = switch(type,
                          "space-time" = 3,
                          "purely-spatial" = 1,
                          "space-time-permutation" = 4,
                          1), 
    ModelType = switch(model,
                       "poisson" = 0,
                       "bernoulli" = 1,
                       "space-time-permutation" = 2,
                       0),
    OutputShapefiles = "n",
    ResultsFile = out_file
  )
  
  if ("pop" %in% names(export_df)) {
    clean_ss_options$PopulationFile <- pop_file
  }
  
  # Add date range from data if available
  if ("date" %in% names(export_df)) {
    dates <- export_df$date
    clean_ss_options$StartDate <- format(min(dates), "%Y/%m/%d")
    clean_ss_options$EndDate <- format(max(dates), "%Y/%m/%d")
  }
  
  # 7. Merge Options from Base and User
  # Order of precedence: User Args > Base PRM > Auto-Generated Defaults
  # Actually usually: Auto Defaults are base, overwritten by Base PRM? 
  # NO, our Auto-Generated params (File paths!) are Critical. They must override Base PRM paths.
  # So: Base PRM < Auto-Generated (Files) < User Args (...)
  
  final_opts <- clean_ss_options
  
  # If base_prm provided, read it and merge
  if (!is.null(base_prm) && file.exists(base_prm)) {
      message("Loading base parameters from: ", base_prm)
      # Simple PRM parser since rsatscan doesn't export a reader
      base_opts_list <- tryCatch({
        lines <- readLines(base_prm)
        # Parse lines: [Section]; Key=Value
        # rsatscan ss.options is a flat list of Key=Value
        
        # Filter comments and empty
        lines <- lines[!grepl("^#|^;", lines) & lines != ""]
        lines <- lines[!grepl("^\\[", lines)] # specific sections? rsatscan flattens them usually
        
        # Split by =
        parts <- strsplit(lines, "=")
        keys <- sapply(parts, `[`, 1)
        vals <- sapply(parts, `[`, 2)
        
        # Trim
        keys <- trimws(keys)
        vals <- trimws(vals)
        
        as.list(setNames(vals, keys))
      }, error = function(e) NULL)
      
      if (!is.null(base_opts_list)) {
          # We want to keep our Critical File Paths.
          # So we merge base_opts onto something, but ensure our paths stay.
          # modifyList(x, y) updates x with y.
          
          # Start with Base
          merged <- base_opts_list
          # Update with our critical config (Files, etc)
          merged <- modifyList(merged, clean_ss_options)
          final_opts <- merged
      }
  }
  
  # 8. Run SatScan using rsatscan's proper API
  ss_full_path <- get_satscan_path()
  if (is.null(ss_full_path)) stop("SatScan path not set. Use set_satscan_path().")
  
  ss_location <- dirname(ss_full_path)
  ss_batch <- basename(ss_full_path)
  
  # macOS App Bundle handling - use CLI binary, not GUI launcher
  if (Sys.info()["sysname"] == "Darwin" && ss_batch == "SaTScan") {
    if (basename(ss_location) == "MacOS") {
      alt_location <- file.path(dirname(ss_location), "app")
      alt_batch <- "satscan"
      if (file.exists(file.path(alt_location, alt_batch))) {
        ss_location <- alt_location
        ss_batch <- alt_batch
      }
    }
  }
  
  if (verbose) message("Running SatScan via rsatscan...")
  
  # Set options in rsatscan environment (required pattern)
  rsatscan::ss.options(reset = TRUE)
  rsatscan::ss.options(final_opts)
  
  # write.ss.prm takes (directory, project_name)
  rsatscan::write.ss.prm(work_dir, "epid")
  
  ss_results <- tryCatch({
    rsatscan::satscan(
      work_dir,
      "epid",
      sslocation = ss_location,
      ssbatchfilename = ss_batch,
      verbose = verbose,
      cleanup = FALSE
    )
  }, error = function(e) {
    warning("SatScan failed: ", e$message)
    return(data)
  })
  
  # 9. Parse Results
  if (verbose) {
    message("SatScan Result Debug:")
    message("  ss_results$col class: ", paste(class(ss_results$col), collapse = ", "))
    message("  ss_results$gis class: ", paste(class(ss_results$gis), collapse = ", "))
  }
  
  # Check if we have GIS output (location-cluster mapping)
  if (!is.null(ss_results) && !is.null(ss_results$gis) && is.data.frame(ss_results$gis)) {
    if (verbose) message("DEBUG: GIS data.frame found - extracting clusters")
    
    # GIS contains: LOC_ID, CLUSTER, P_VALUE, CLU_OBS, CLU_EXP, CLU_ODE, CLU_RR, CLU_POP, etc.
    # Extract what we need
    loc_cluster_map <- ss_results$gis |>
      dplyr::select(LOC_ID, CLUSTER) |>
      dplyr::mutate(LOC_ID = as.character(LOC_ID))
    
    # Get cluster stats - prefer $col if available, otherwise use $gis summary
    if (!is.null(ss_results$col) && is.data.frame(ss_results$col)) {
      cluster_info <- ss_results$col |>
        dplyr::select(CLUSTER, P_VALUE, REL_RISK)
    } else {
      # Extract unique cluster info from $gis (use CLU_RR for relative risk)
      cluster_info <- ss_results$gis |>
        dplyr::select(CLUSTER, P_VALUE, REL_RISK = CLU_RR) |>
        dplyr::distinct(CLUSTER, .keep_all = TRUE)
    }
    
    # Join cluster info to location map
    full_map <- loc_cluster_map |>
      dplyr::left_join(cluster_info, by = "CLUSTER")
    
    # Join to data
    if (!rlang::quo_is_null(id_quo)) {
      data$epid_link_id <- as.character(dplyr::pull(data, !!id_quo))
    } else {
      data$epid_link_id <- as.character(seq_len(nrow(data)))
    }
    
    final_df <- data |>
      dplyr::left_join(full_map, by = c("epid_link_id" = "LOC_ID")) |>
      dplyr::select(-epid_link_id)
    
    return(final_df)
  } else {
    if (verbose) message("DEBUG: No valid cluster data found")
  }
  
  return(data)
}

