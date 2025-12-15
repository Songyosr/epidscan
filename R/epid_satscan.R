#' Tidy Interface for SatScan Analysis
#'
#' A pipe-friendly wrapper for running SatScan spatial and space-time cluster analyses
#' directly on data frames or sf objects. Handles file creation, parameter configuration,
#' and result parsing automatically.
#'
#' @param data Input data. Can be an \code{sf} object (geometry auto-extracted) or a data.frame
#'   (requires lat_col and long_col).
#' @param obs_col Column containing observed case counts (unquoted, required).
#' @param pop_col Column containing population counts (unquoted, optional for Poisson model).
#' @param date_col Column containing dates/times (unquoted, optional for temporal analysis).
#' @param id_col Column containing unique location IDs (unquoted). If missing, row numbers used.
#' @param lat_col Column containing latitude (unquoted, required if data is not sf).
#' @param long_col Column containing longitude (unquoted, required if data is not sf).
#' @param type Analysis type. One of:
#'   \itemize{
#'     \item "purely-spatial" (1)
#'     \item "purely-temporal" (2)
#'     \item "space-time" (3, default)
#'     \item "space-time-permutation" (4)
#'     \item "spatial-variation-in-temporal-trends" (5)
#'     \item "space-time-magnitude" (6)
#'   }
#'   Note: "bernoulli" is NOT a valid analysis type; use \code{model="bernoulli"} with an appropriate type (e.g. "purely-spatial").
#' @param model Probability model. One of:
#'   \itemize{
#'     \item "poisson" (0, default)
#'     \item "bernoulli" (1)
#'     \item "space-time-permutation" (2)
#'     \item "ordinal" (3)
#'     \item "exponential" (4)
#'     \item "normal" (5)
#'     \item "continuous-poisson" (6)
#'     \item "multinomial" (7)
#'     \item "rank" (8)
#'   }
#' @param time_precision Unit of time: "day", "month", "year", "generic", or NULL for auto-detect.
#'   Auto-detection uses Date objects for "day", numeric for "generic".
#' @param geo_type String. "latlong" (default) or "cartesian".
#'   If "latlong", `sf` data is transformed to WGS84.
#'   If "cartesian", raw coordinates are used (CoordinatesType=0).
#' @param start_date Optional start date (Date, POSIXt, or character).
#'   If NULL, defaults to minimum date in data.
#' @param end_date Optional end date (Date, POSIXt, or character).
#'   If NULL, defaults to maximum date in data.
#' @param monitor_mode String. "retrospective" (default) or "prospective".
#'   "retrospective" analyzes the entire dataset for past clusters.
#'   "prospective" scans for "active" clusters surviving to the end of the study period.
#' @param prospective_start_date Start date for the prospective surveillance period.
#'   Required if \code{monitor_mode = "prospective"}. Clusters must arguably be "active" after this date to be reported
#'   in prospective mode. Defined as Date, POSIXt, or character string.
#' @param output_dir Directory for SatScan output files. If NULL (default), uses temp directory.
#' @param verbose Logical. Print SatScan progress and debug info?
#' @param ... Additional arguments passed to \code{rsatscan::ss.options()}. Common options include:
#'   \itemize{
#'     \item MonteCarloReps - Number of Monte Carlo replications (default 999)
#'     \item MaxSpatialSizeInPopulationAtRisk - Max cluster size as percentage (default 50)
#'     \item MaxTemporalSize - Max temporal cluster size
#'   }
#'
#' @return A data.frame or sf object with cluster columns added:
#'   \itemize{
#'     \item CLUSTER - Cluster ID (1 = most significant)
#'     \item P_VALUE - Statistical significance
#'     \item REL_RISK - Relative risk compared to rest of study area
#'   }
#'
#' @details
#' This function requires SatScan to be installed on your system. Set the path using
#' \code{set_satscan_path()}.
#'
#' The function internally:
#' \enumerate{
#'   \item Extracts geometry from sf objects or lat/long columns
#'   \item Writes .cas, .pop, and .geo files for SatScan
#'   \item Configures parameters based on data and user options
#'   \item Executes SatScan via rsatscan
#'   \item Parses results and joins cluster info back to input data
#' }
#'
#' @examples
#' \dontrun{
#' # Basic space-time analysis
#' set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")
#'
#' result <- my_data |>
#'   epid_satscan(
#'     obs_col = cases,
#'     pop_col = population,
#'     date_col = date,
#'     id_col = location_id,
#'     lat_col = latitude,
#'     long_col = longitude
#'   )
#'
#' # Filter significant clusters
#' clusters <- result |> dplyr::filter(!is.na(CLUSTER), P_VALUE < 0.05)
#' }
#'
#' @seealso \code{\link{set_satscan_path}}, \code{\link{get_satscan_path}}
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
                         geo_type = "latlong",
                         start_date = NULL,
                         end_date = NULL,
                         monitor_mode = "retrospective",
                         prospective_start_date = NULL,
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
  geo_df <- extract_geometry(data, lat_quo, long_quo, geo_type)

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

  # 6. Detect time precision (MOVED UP)
  date_values <- if ("date" %in% names(export_df)) export_df$date else NULL
  time_prec <- detect_time_precision(date_values, time_precision)

  # Validate explicit start/end dates against precision
  # We basically just check if we can format them without error using the detected precision
  if (!is.null(start_date)) {
    # If character, trust user or try to parse?
    # SatScan expects string matching precision.
    # Only simple check if it's not a string
    if (inherits(start_date, "Date") || inherits(start_date, "POSIXt")) {
      # Good
    } else if (!is.character(start_date)) {
      stop("start_date must be Date, POSIXt or character string.")
    }
  }
  if (!is.null(end_date)) {
    if (inherits(end_date, "Date") || inherits(end_date, "POSIXt")) {
      # Good
    } else if (!is.character(end_date)) {
      stop("end_date must be Date, POSIXt or character string.")
    }
  }

  # Validate prospective settings
  if (monitor_mode == "prospective" && is.null(prospective_start_date)) {
    stop("prospective_start_date is required when monitor_mode is 'prospective'.")
  }

  # 7. Write input files (Now using time_prec)
  files <- write_satscan_files(geo_df, export_df, work_dir, time_precision = time_prec)

  # 8. Build options
  opts <- build_satscan_options(files, export_df, time_prec, type, model, geo_type, start_date, end_date, monitor_mode, prospective_start_date)
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
