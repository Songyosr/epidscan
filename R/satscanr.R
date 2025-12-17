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
#'   Arguments explicitly passed to this function (detected via \code{!missing()})
#'   and any raw parameters passed in \code{...}.
#'   \emph{Overrides Level 3 templates.}
#'
#'   \item \strong{Level 3: Template PRM}
#'   Settings loaded from \code{prm_path} if provided.
#'   \emph{Overrides Level 4 defaults.}
#'
#'   \item \strong{Level 4: Implicit Defaults}
#'   Default values of this function's arguments (e.g., \code{model = "poisson"}).
#'   Only used if not set in Level 2 or Level 3.
#' }
#'
#' @param cas Case table (\code{satscan_table}).
#' @param pop Population table (\code{satscan_table}, optional).
#' @param geo Geometry table (\code{satscan_table}).
#' @param ctl Control table (\code{satscan_table}, optional).
#' @param grd Grid table (\code{satscan_table}, optional).
#' @param type Analysis type ("space-time", "purely-spatial", etc.). Default is "space-time".
#' @param model Model type ("poisson", "bernoulli", etc.). Default is "poisson".
#' @param start_date Analysis start date (Date/String). Default NULL.
#' @param end_date Analysis end date (Date/String). Default NULL.
#' @param monitor_mode "retrospective" (default) or "prospective".
#' @param prospective_start_date Start date for prospective surveillance.
#' @param max_spatial_size Max spatial size (fraction 0-1). Default 0.50.
#' @param max_temporal_size Max temporal size (fraction 0-1). Default 0.50.
#' @param prm_path Path to a template .prm file to load configuration from (Level 3).
#' @param output_dir Directory to copy final results to. If NULL, results remain in temp.
#' @param verbose Logical. Print progress and debug info.
#' @param ... Additional raw SaTScan parameters (Level 2 Tweaks).
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
#' @importFrom utils write.table read.table modifyList
#' @importFrom dplyr rename
#' @export
satscanr <- function(cas, pop = NULL, geo, ctl = NULL, grd = NULL,
                     type = "space-time", model = "poisson",
                     start_date = NULL, end_date = NULL,
                     monitor_mode = "retrospective", prospective_start_date = NULL,
                     max_spatial_size = 0.50, max_temporal_size = 0.50,
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

    # 4. Parameter Construction (The Hierarchy)

    # helper to map signature args to SaTScan keys using the existing logic in build_satscan_options
    # We create a minimalist call to it just for mapping
    map_sig_args <- function(t_prec) {
        # We pass dummy files to satisfy the function, output is what matters
        files_dummy <- list(cas_file = "x", geo_file = "x")
        build_satscan_options(
            files = files_dummy,
            export_df = data.frame(), # No date inference here, strictly args
            time_precision = t_prec,
            type = type, model = model, geo_type = 0, # Defaults
            start_date = start_date, end_date = end_date,
            monitor_mode = monitor_mode,
            prospective_start_date = prospective_start_date
        )
    }

    # Determine Time Precision for Mapping
    tp_map <- list(generic = 0, year = 1, month = 2, day = 3)
    tp_char <- cas$spec$time_precision
    tp_int <- if (is.null(tp_char)) 0 else tp_map[[tp_char]]

    # Step A: Establish Base (Level 3 vs Level 4)
    current_opts <- list()

    if (!is.null(prm_path)) {
        # Level 3: Template PRM
        if (!file.exists(prm_path)) stop("PRM file not found: ", prm_path)
        if (verbose) message("Loading Level 3 Template: ", basename(prm_path))

        # Simple INI Parser
        lines <- readLines(prm_path)
        # Filter comments and empty lines
        lines <- lines[!grepl("^#", lines) & lines != ""]
        # Extract Key=Value
        matches <- regmatches(lines, regexec("^([^=]+)=(.*)$", lines))
        # Valid lines have length 3 (Full match, Key, Value)
        # Note: Section headers [Input] are ignored by regexec structure here or captured as garbage?
        # Standard PRM lines are like "CaseFile=epid.cas". Section headers like "[Input]" don't match.

        for (m in matches) {
            if (length(m) == 3) {
                key <- trimws(m[[2]])
                val <- trimws(m[[3]])
                # Try to convert numeric
                if (grepl("^[0-9.]+$", val)) val <- as.numeric(val)
                current_opts[[key]] <- val
            }
        }
    } else {
        # Level 4: Implicit Defaults
        # We start with the full set of defaults mapped
        current_opts <- map_sig_args(tp_int)

        # Add the manual defaults not in build helper
        current_opts$MaxSpatialSizeInPopulationAtRisk <- max_spatial_size * 100
        current_opts$MaxTemporalSize <- max_temporal_size * 100
    }

    # Step B: Apply Level 2 Tweaks (Explicit Args)
    tweaks <- list()

    # B1. Signature Arguments (if !missing)
    # If prm_path was provided, we ONLY apply these if user explicitly typed them.
    # If prm_path was NOT provided, current_opts ALREADY contains these (from Level 4 logic),
    # so re-applying them is harmless/redundant but correct.

    # We use the same mapper but need to only pick the ones !missing
    # Since map_sig_args uses the current function scope values, it uses what user passed.
    mapped_current <- map_sig_args(tp_int)

    # AnalysisType
    if (!missing(type)) tweaks$AnalysisType <- mapped_current$AnalysisType
    # ModelType
    if (!missing(model)) tweaks$ModelType <- mapped_current$ModelType
    # Dates
    if (!missing(start_date)) tweaks$StartDate <- mapped_current$StartDate
    if (!missing(end_date)) tweaks$EndDate <- mapped_current$EndDate
    # Monitor
    # Note: build_satscan_options doesn't strictly output 'ProspectiveStartDate' unless conditions met?
    # We rely on the mapper's logic. If it produced it, we take it.
    if (!missing(monitor_mode)) {
        # SaTScan 10 parameter: P 17: "AnalysisType=...", P 18: "ProspectiveStartDate=..."
        # If user sets monitor_mode='prospective', mapped_current might have ProspectiveStartDate.
        if (!is.null(mapped_current$ProspectiveStartDate)) tweaks$ProspectiveStartDate <- mapped_current$ProspectiveStartDate
    }
    if (!missing(prospective_start_date)) tweaks$ProspectiveStartDate <- mapped_current$ProspectiveStartDate

    # Sizes
    if (!missing(max_spatial_size)) tweaks$MaxSpatialSizeInPopulationAtRisk <- max_spatial_size * 100
    if (!missing(max_temporal_size)) tweaks$MaxTemporalSize <- max_temporal_size * 100

    # B2. Ellipsis (...) Tweaks
    dots <- list(...)
    tweaks <- utils::modifyList(tweaks, dots)

    # Merge L2 into Base
    combined_opts <- utils::modifyList(current_opts, tweaks)

    # Step C: Level 1 Data Integrity (Immutable)
    # We perform inference on dates from Data if Level 2 didn't set them explicitly?
    # Actually, L1 "Inferred Dates" is a tricky L1/L4 boundary.
    # If the user didn't set StartDate (L2), and PRM didn't set it (L3),
    # we usually want to infer it from Data (L1 behavior) rather than NULL (L4).
    # Logic:
    # If StartDate is missing in combined_opts, try to infer from data.

    # Date Inference Logic
    if (is.null(combined_opts$StartDate) || is.null(combined_opts$EndDate)) {
        # Construct dummy date df
        d_vals <- NULL
        if (tp_char == "day" && !is.null(cas$data$time)) {
            d_vals <- as.Date(cas$data$time, format = "%Y/%m/%d")
        } else if (tp_char == "month" && !is.null(cas$data$time)) {
            d_vals <- as.Date(paste0(cas$data$time, "/01"), format = "%Y/%m/%d")
        } else if (tp_char == "year" && !is.null(cas$data$time)) d_vals <- as.Date(paste0(cas$data$time, "/01/01"), format = "%Y/%m/%d")

        if (!is.null(d_vals)) {
            # Use internal formatter if possible, or just format
            # Re-use build_satscan_options but ONLY for dates
            date_opts <- build_satscan_options(
                files = list(cas_file = "x", geo_file = "x"),
                export_df = data.frame(date = d_vals),
                time_precision = tp_int,
                type = type, model = model
            )

            if (is.null(combined_opts$StartDate)) combined_opts$StartDate <- date_opts$StartDate
            if (is.null(combined_opts$EndDate)) combined_opts$EndDate <- date_opts$EndDate
        }
    }

    # Hard Overrides (Files & Precision)
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
        # Check standard default.
        if (is.null(combined_opts$GridPosition)) critical_overrides$GridPosition <- 2 # 2=Lat/Long or XY?
    }

    final_opts <- utils::modifyList(combined_opts, critical_overrides)

    # 5. Execution
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

    # Ensure work_dir in result points to    # Fix the work_dir in res for debugging/testing
    res$work_dir <- if (!is.null(output_dir)) output_dir else work_dir
    res$prm <- final_opts

    return(res)
}
