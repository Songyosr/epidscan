#' Run SaTScan Analysis
#'
#' Orchestrates the SaTScan analysis by managing inputs, configuring parameters
#' via a strictly defined hierarchy, execution, and result parsing.
#'
#' @section Parameter Hierarchy (The "Smart Tweak" Model):
#' Parameters are resolved with the following precedence (Highest to Lowest):
#' \enumerate{
#'   \item \strong{Level 1: Data Integrity (Immutable)}
#'   Derived directly from the input objects (\code{cas}, \code{geo}, etc.).
#'   Includes filenames, time precision, and coordinate types.
#'   \emph{User cannot override these.}
#'
#'   \item \strong{Level 2: User Tweaks}
#'   Arguments explicitly passed to this function via \code{...}.
#'   \emph{Overrides Level 3 templates.}
#'
#'   \item \strong{Level 3: Template PRM}
#'   Settings loaded from \code{prm_path} if provided.
#'   \emph{Overrides Level 4 defaults.}
#'
#'   \item \strong{Level 4: Implicit Defaults}
#'   SaTScan's internal defaults (via \code{ss.options(reset=TRUE)}).
#' }
#'
#' @param cas Case table (\code{satscan_table}).
#' @param pop Population table (\code{satscan_table}, optional).
#' @param geo Geometry table (\code{satscan_table}).
#' @param ctl Control table (\code{satscan_table}, optional).
#' @param grd Grid table (\code{satscan_table}, optional).
#' @param prm_path Path to a template .prm file to load configuration from (Level 3).
#' @param output_dir Directory to copy final results to. If NULL, results remain in temp.
#' @param verbose Logical. Print progress and debug info.
#' @param ... Additional SaTScan parameters (Level 2 Tweaks).
#'   See \code{\link{satscan_options}} for a full list (e.g., \code{AnalysisType=1}, \code{ModelType=0}).
#' @return A \code{satscan_result} object containing:
#'   \item{main}{Main text output lines}
#'   \item{col}{Column definition data}
#'   \item{rr}{Relative Risk data}
#'   \item{gis}{GIS/Cluster information}
#'   \item{cluster_summary}{Tidy summary of clusters found}
#'   \item{location_summary}{Tidy summary of locations}
#'   \item{shapefile}{Result shapefile (sf) if available}
#'   \item{prm}{Final parameters used}
#'   \item{work_dir}{Path to working directory}
#' @importFrom rsatscan satscan ss.options
#' @importFrom utils write.table read.table modifyList file_test
#' @importFrom dplyr rename
#' @export
satscanr <- function(cas, pop = NULL, geo, ctl = NULL, grd = NULL,
                     prm_path = NULL,
                     output_dir = NULL,
                     verbose = FALSE, ...) {
    # 1. Setup Environment
    work_dir <- tempdir()
    if (verbose) message("Working directory: ", work_dir)

    # 2. Input Validation (Level 1 Check)
    if (!inherits(cas, "satscan_table") || cas$kind != "cas") stop("cas input must be a satscan_table of kind 'cas'")
    if (!inherits(geo, "satscan_table") || geo$kind != "geo") stop("geo input must be a satscan_table of kind 'geo'")
    if (!is.null(pop)) {
        if (!inherits(pop, "satscan_table") || pop$kind != "pop") stop("pop input must be class satscan_table kind 'pop'")
    }

    # 3. Write Files (Level 1 Physical Basis)
    f_cas <- file.path(work_dir, "epid.cas")
    f_geo <- file.path(work_dir, "epid.geo")
    f_pop <- if (!is.null(pop)) file.path(work_dir, "epid.pop") else NULL
    f_ctl <- if (!is.null(ctl)) file.path(work_dir, "epid.ctl") else NULL
    f_grd <- if (!is.null(grd)) file.path(work_dir, "epid.grd") else NULL

    write_ss_file <- function(df, path) {
        utils::write.table(df, path, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    }
    write_ss_file(cas$data, f_cas)
    write_ss_file(geo$data, f_geo)
    if (!is.null(f_pop)) write_ss_file(pop$data, f_pop)
    if (!is.null(f_ctl)) write_ss_file(ctl$data, f_ctl)
    if (!is.null(f_grd)) write_ss_file(grd$data, f_grd)

    # 4. Parameter Setup (The Hierarchy)

    # A. Base Template (Level 3)
    base_opts <- NULL
    if (!is.null(prm_path)) {
        if (verbose) message("Loading Level 3 Template: ", basename(prm_path))
        base_opts <- read_prm(prm_path)
    }

    # B. Set Options (Level 4 + Level 3 + Level 2)
    # - Reset to defaults (L4)
    # - Apply Base (L3)
    # - Apply Overrides (L2) from ...
    set_satscan_opts(.base = base_opts, .reset = TRUE, ...)

    # C. Level 1 Data Integrity (Immutable Overrides)

    # Determine Time Precision
    tp_map <- list(generic = 0, year = 1, month = 2, day = 3)
    tp_char <- cas$spec$time_precision
    tp_int <- if (is.null(tp_char)) 0 else tp_map[[tp_char]]

    # Critical Overrides
    critical_overrides <- list(
        CaseFile = basename(f_cas),
        CoordinatesFile = basename(f_geo),
        CoordinatesType = if (geo$spec$coord_type == "cartesian") 0 else 1,
        PrecisionCaseTimes = tp_int,
        TimeAggregationUnits = tp_int
    )
    if (!is.null(f_pop)) critical_overrides$PopulationFile <- basename(f_pop)
    if (!is.null(f_ctl)) critical_overrides$ControlFile <- basename(f_ctl)
    if (!is.null(f_grd)) {
        critical_overrides$GridFile <- basename(f_grd)
        # Ensure GridPosition is set if using grid file (default to LatLong/XY matching CoordsType)
        # We check if it's already set by user; if not, default it.
        curr <- rsatscan::ss.options()
        if (is.null(curr$GridPosition)) critical_overrides$GridPosition <- 2
    }

    # Apply Critical Overrides
    rsatscan::ss.options(critical_overrides)

    # D. Date Inference (Smart Defaults)
    # If StartDate/EndDate are NOT set by user/PRM, try to infer from data.
    # D. Date Inference (Smart Defaults)
    # If StartDate/EndDate are NOT set by user/PRM, try to infer from data.
    current_opts <- rsatscan::ss.options()

    inferred_dates <- infer_dates_from_data(
        current_opts = current_opts,
        cas_data = cas$data,
        time_precision_char = tp_char,
        verbose = verbose
    )

    if (!is.null(inferred_dates)) {
        rsatscan::ss.options(inferred_dates)
    }

    # 5. Execution
    # Fetch final state for logging
    final_opts <- rsatscan::ss.options()

    ss_full_path <- get_satscan_path()
    if (is.null(ss_full_path)) stop("SaTScan path not set")
    path_info <- get_macos_satscan_path(ss_full_path)

    ss_res <- run_satscan(
        work_dir = work_dir,
        project_name = "epid",
        ss_location = path_info$ss_location,
        ss_batch = path_info$ss_batch,
        final_opts = final_opts,
        verbose = verbose
    )

    # 6. Output Management (Optional Copy)
    if (!is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        # Copy result files
        res_files <- list.files(work_dir, pattern = "^epid\\.(txt|col|gis|sci|shp|dbf|shx)$", full.names = TRUE)
        file.copy(res_files, output_dir, overwrite = TRUE)
        if (verbose) message("Results copied to ", output_dir)
    }

    # 7. Parse & Return
    geo_for_parse <- geo$data |> dplyr::rename(id = loc_id)
    if (geo$spec$coord_type == "latlong") {
        geo_for_parse <- geo_for_parse |> dplyr::rename(lat = coord1, long = coord2)
    } else {
        geo_for_parse <- geo_for_parse |> dplyr::rename(x = coord1, y = coord2)
    }

    res <- parse_satscan_output(
        ss_results = ss_res,
        data = cas$data,
        geo_df = geo_for_parse,
        id_quo = rlang::quo(loc_id),
        output_dir = if (!is.null(output_dir)) output_dir else work_dir,
        verbose = verbose
    )

    res$work_dir <- if (!is.null(output_dir)) output_dir else work_dir
    # Ensure prm is a list for consistent downstream usage
    res$prm <- as.list(final_opts)

    return(res)
}
