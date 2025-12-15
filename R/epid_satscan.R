#' Tidy Interface for SatScan Analysis
#'
#' A pipe-friendly wrapper for running SatScan analyses directly on data frames or sf objects.
#' Abstracts away the file creation process.
#'
#' @param data Input data. Can be an \code{sf} object or a data.frame.
#' @param obs_col Column containing observed case counts (unquoted).
#' @param pop_col Column containing population counts (unquoted, optional).
#' @param date_col Column containing time/dates (unquoted, optional).
#' @param id_col Column containing unique location IDs (unquoted). If missing, row numbers used.
#' @param lat_col Column containing latitude (unquoted, required if data is not sf).
#' @param long_col Column containing longitude (unquoted, required if data is not sf).
#' @param type Analysis type: "space-time", "purely-spatial", "space-time-permutation".
#' @param model Model type: "poisson", "bernoulli", "space-time-permutation".
#' @param time_precision Unit of time: "day", "month", "year", "generic", or NULL for auto-detect.
#' @param output_dir Directory for output files. If NULL, uses temp directory.
#' @param verbose Logical. Print SatScan output?
#' @param ... Additional arguments passed to rsatscan::ss.options.
#'
#' @return A data.frame or sf object with CLUSTER, P_VALUE, REL_RISK columns added.
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @importFrom sf st_centroid st_coordinates st_geometry
#' @importFrom dplyr select mutate left_join distinct pull
#' @importFrom utils modifyList write.table
#' @importFrom rsatscan ss.options write.ss.prm satscan
#' @export
epid_satscan <- function(data,
                         obs_col,
                         pop_col = NULL,
                         date_col = NULL,
                         id_col = NULL,
                         lat_col = NULL,
                         long_col = NULL,
                         type = "space-time",
                         model = "poisson",
                         time_precision = NULL,
                         output_dir = NULL,
                         verbose = FALSE,
                         ...) {
  # 1. Capture quosures
  obs_quo <- rlang::enquo(obs_col)
  pop_quo <- rlang::enquo(pop_col)
  date_quo <- rlang::enquo(date_col)
  id_quo <- rlang::enquo(id_col)
  lat_quo <- rlang::enquo(lat_col)
  long_quo <- rlang::enquo(long_col)

  # 2. Extract geometry
  geo_df <- extract_geometry(data, lat_quo, long_quo)

  # 3. Extract/generate IDs
  if (rlang::quo_is_null(id_quo)) {
    message("No id_col provided. Generating internal IDs.")
    id_vec <- as.character(seq_len(nrow(data)))
  } else {
    id_vec <- as.character(dplyr::pull(data, !!id_quo))
  }
  geo_df$id <- id_vec

  # 4. Build export dataframe
  export_df <- data.frame(id = id_vec)

  if (!rlang::quo_is_null(obs_quo)) {
    export_df$cases <- rlang::eval_tidy(obs_quo, data)
  } else {
    stop("obs_col is required.")
  }

  if (!rlang::quo_is_null(pop_quo)) {
    export_df$pop <- rlang::eval_tidy(pop_quo, data)
  }

  if (!rlang::quo_is_null(date_quo)) {
    export_df$date <- rlang::eval_tidy(date_quo, data)
  }

  # 5. Setup work directory
  work_dir <- if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    output_dir
  } else {
    tempdir()
  }

  # 6. Write input files
  files <- write_satscan_files(geo_df, export_df, work_dir)

  # 7. Detect time precision
  date_values <- if ("date" %in% names(export_df)) export_df$date else NULL
  time_prec <- detect_time_precision(date_values, time_precision)

  # 8. Build options
  opts <- build_satscan_options(files, export_df, time_prec, type, model)
  opts <- apply_user_overrides(opts, list(...))

  # 9. Get SatScan path
  ss_full_path <- get_satscan_path()
  if (is.null(ss_full_path)) stop("SatScan path not set. Use set_satscan_path().")

  ss_paths <- get_macos_satscan_path(ss_full_path)

  # 10. Run SatScan
  ss_results <- run_satscan(
    work_dir = work_dir,
    project_name = "epid",
    ss_location = ss_paths$ss_location,
    ss_batch = ss_paths$ss_batch,
    final_opts = opts,
    verbose = verbose
  )

  # 11. Parse and return results
  if (is.null(ss_results)) {
    warning("SatScan returned NULL - returning original data")
    return(data)
  }

  parse_results(ss_results, data, id_quo, verbose)
}
