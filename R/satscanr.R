# SaTScan Analysis Runner
# Main entry point for the new table-based API

# -----------------------------------------------------------------------------
# Helper: Infer Dates from Data
# -----------------------------------------------------------------------------

#' Infer Dates from Data
#'
#' Helper to infer StartDate and EndDate from the case data if missing from options.
#'
#' @param current_opts List of current SaTScan options
#' @param cas_data Case data frame (must have a 'time' column)
#' @param time_precision_char Character string: "day", "month", or "year"
#' @param verbose Logical, print messages
#' @return Named list of inferred dates (StartDate, EndDate) or NULL if nothing inferred.
#' @keywords internal
infer_dates_from_data <- function(current_opts, cas_data, time_precision_char, verbose = FALSE) {
    # Check if dates are already present and NOT placeholder values
    # The default template has StartDate=2000/1/1 and EndDate=2000/12/31 as placeholders
    val_start <- current_opts[["StartDate"]]
    val_end <- current_opts[["EndDate"]]

    # Placeholder detection: default template values that should be overridden
    is_placeholder_start <- is.null(val_start) || is.na(val_start) || val_start == "" ||
        grepl("^2000/", val_start) || grepl("^2000-", val_start)
    is_placeholder_end <- is.null(val_end) || is.na(val_end) || val_end == "" ||
        grepl("^2000/", val_end) || grepl("^2000-", val_end)

    need_start <- is_placeholder_start
    need_end <- is_placeholder_end

    if (!need_start && !need_end) {
        return(NULL)
    }

    if (is.null(cas_data) || is.null(cas_data$time)) {
        return(NULL)
    }

    # Parse based on precision
    d_vals <- NULL
    if (time_precision_char == "day") {
        d_vals <- as.Date(cas_data$time, format = "%Y/%m/%d")
    } else if (time_precision_char == "month") {
        d_vals <- as.Date(paste0(cas_data$time, "/01"), format = "%Y/%m/%d")
    } else if (time_precision_char == "year") {
        d_vals <- as.Date(paste0(cas_data$time, "/01/01"), format = "%Y/%m/%d")
    }

    if (is.null(d_vals) || all(is.na(d_vals))) {
        return(NULL)
    }

    min_d <- min(d_vals, na.rm = TRUE)
    max_d <- max(d_vals, na.rm = TRUE)

    inferred <- list()
    if (need_start) {
        if (time_precision_char == "year") {
            inferred$StartDate <- format(min_d, "%Y/01/01")
        } else if (time_precision_char == "month") {
            inferred$StartDate <- format(min_d, "%Y/%m/01")
        } else {
            inferred$StartDate <- format(min_d, "%Y/%m/%d")
        }
    }
    if (need_end) {
        if (time_precision_char == "year") {
            inferred$EndDate <- format(max_d, "%Y/12/31")
        } else if (time_precision_char == "month") {
            # End of month
            d_next <- seq(max_d, by = "month", length.out = 2)[2]
            inferred$EndDate <- format(d_next - 1, "%Y/%m/%d")
        } else {
            inferred$EndDate <- format(max_d, "%Y/%m/%d")
        }
    }

    if (length(inferred) > 0) {
        if (verbose) message("Inferring missing dates from data: ", paste(names(inferred), collapse = ", "))
    }

    inferred
}

# -----------------------------------------------------------------------------
# satscanr: Main Analysis Function
# -----------------------------------------------------------------------------

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
#'   See \code{\link{satscan_parameters}} for a full list (e.g., \code{AnalysisType=1}, \code{ModelType=0}).
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

    # 4. Parameter Setup (The Hierarchy) - Using new prm_* system

    # A. Base Template (Level 3/4)
    if (!is.null(prm_path)) {
        if (verbose) message("Loading Template: ", basename(prm_path))
        prm <- prm_parse(prm_path)

        # Validate external PRM file
        validation <- prm_validate(prm)
        if (!validation$valid && verbose) {
            message(
                "  Warning: External PRM missing ", length(validation$missing),
                " parameters (compared to v", validation$ref_version, " template)"
            )
        }
    } else {
        if (verbose) message("Loading defaults from bundled templates")
        prm <- prm_defaults()
    }

    # B. Apply User Overrides (Level 2) from ...
    user_opts <- list(...)
    if (length(user_opts) > 0) {
        prm <- do.call(prm_set, c(list(prm, .strict = FALSE), user_opts))
    }

    # C. Level 1 Data Integrity (Immutable Overrides)

    # Determine Time Precision
    tp_map <- list(generic = 0, year = 1, month = 2, day = 3)
    tp_char <- cas$spec$time_precision
    tp_int <- if (is.null(tp_char)) 0 else tp_map[[tp_char]]

    # Critical Overrides - these always win (use .strict=FALSE for files missing some keys)
    prm <- prm_set(prm,
        CaseFile = basename(f_cas),
        CoordinatesFile = basename(f_geo),
        CoordinatesType = if (geo$spec$coord_type == "cartesian") 0 else 1,
        PrecisionCaseTimes = tp_int,
        TimeAggregationUnits = tp_int,
        ResultsFile = "epid.txt",
        .strict = FALSE
    )

    if (!is.null(f_pop)) prm <- prm_set(prm, PopulationFile = basename(f_pop), .strict = FALSE)
    if (!is.null(f_ctl)) prm <- prm_set(prm, ControlFile = basename(f_ctl), .strict = FALSE)
    if (!is.null(f_grd)) {
        prm <- prm_set(prm, GridFile = basename(f_grd), UseGridFile = "y", .strict = FALSE)
    }

    # D. Date Inference (Smart Defaults)
    # If StartDate/EndDate are NOT set by user/PRM, try to infer from data.
    inferred_dates <- infer_dates_from_data(
        current_opts = prm, # Pass prm_list instead of ss.options()
        cas_data = cas$data,
        time_precision_char = tp_char,
        verbose = verbose
    )

    if (!is.null(inferred_dates)) {
        prm <- do.call(prm_set, c(list(prm, .strict = FALSE), as.list(inferred_dates)))
    }

    # 5. Execution
    # Write PRM using skeleton injection
    prm_write(prm, file.path(work_dir, "epid.prm"))

    ss_full_path <- get_satscan_path()
    if (is.null(ss_full_path)) stop("SaTScan path not set")
    path_info <- get_macos_satscan_path(ss_full_path)

    ss_res <- run_satscan(
        work_dir = work_dir,
        project_name = "epid",
        ss_location = path_info$ss_location,
        ss_batch = path_info$ss_batch,
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

    # Return the prm_list for reference
    res$prm <- prm

    return(res)
}
