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
#' @section Statistical Methodology:
#' The scan statistic detects and evaluates clusters by gradually scanning a window
#' across space and/or time. At each location, the number of observed and expected
#' cases inside the window is compared using a likelihood ratio test (Kulldorff, 1997).
#'
#' \strong{Likelihood Ratio Test (Poisson Model):}
#' For a scanning window with \eqn{c} observed cases and \eqn{E[c]} expected cases
#' (given total cases \eqn{C}), the likelihood is proportional to:
#' \deqn{L = \left(\frac{c}{E[c]}\right)^c \left(\frac{C-c}{C-E[c]}\right)^{C-c} I()}
#' where \eqn{I()} is an indicator function (1 if scanning for high rates and \eqn{c > E[c]}).
#'
#' The window with maximum likelihood is the \strong{most likely cluster}. Statistical
#' significance is determined via Monte Carlo simulation: p-values are computed by
#' comparing the observed maximum likelihood to those from randomly simulated datasets
#' under the null hypothesis of spatial homogeneity.
#'
#' \strong{Probability Models:}
#' \itemize{
#'   \item \strong{Discrete Poisson}: Cases are Poisson-distributed proportional to
#'     population. Requires case and population files.
#'   \item \strong{Bernoulli}: Binary 0/1 data (cases/controls). Requires case and
#'     control files.
#'   \item \strong{Space-Time Permutation}: Uses only case data; compares observed
#'     space-time distribution against expected under independence of space and time.
#' }
#'
#' \strong{Scanning Windows:}
#' \itemize{
#'   \item \strong{Purely Spatial}: Circular or elliptic window scanning across space.
#'   \item \strong{Purely Temporal}: Interval scanning across time.
#'   \item \strong{Space-Time}: Cylindrical window (circular base in space, height in time).
#' }
#'
#' For full methodological details, see the SaTScan User Guide (Kulldorff, 2022)
#' and the original methodology paper: Kulldorff M. (1997). A spatial scan statistic.
#' Communications in Statistics: Theory and Methods, 26:1481-1496.
#'
#' @section Parameter Hierarchy (The "Smart Tweak" Model):
#' Parameters are resolved with the following precedence (highest to lowest):
#'
#' \enumerate{
#'   \item \strong{Level 1: Data Integrity (Immutable)}
#'
#'   Derived directly from the input table objects (\code{cas}, \code{geo}, etc.).
#'   These are \emph{automatically set} based on your data and cannot be overridden:
#'   \itemize{
#'     \item \code{CaseFile}, \code{CoordinatesFile}, \code{PopulationFile} (filenames)
#'     \item \code{CoordinatesType} (from \code{geo$spec$coord_type})
#'     \item \code{PrecisionCaseTimes}, \code{TimeAggregationUnits} (from \code{cas$spec$time_precision})
#'     \item \code{StartDate}, \code{EndDate} (inferred from case data if not specified)
#'   }
#'
#'   \item \strong{Level 2: User Tweaks}
#'
#'   Arguments passed explicitly via \code{...}. These override template settings.
#'   Example: \code{satscanr(..., AnalysisType = 3, MonteCarloReps = 999)}
#'
#'   \item \strong{Level 3: Template PRM}
#'
#'   Settings loaded from an external \code{.prm} file via \code{prm_path}.
#'   Use this to reuse configurations from previous SaTScan sessions.
#'
#'   \item \strong{Level 4: Package Defaults}
#'
#'   Bundled default templates (SaTScan v10.3 defaults from \code{prm_defaults()}).
#'   These provide sensible starting values when no template is specified.
#' }
#'
#' @section Parameter Management:
#' This function uses the native \code{prm_*} system for parameter management,
#' which directly manipulates PRM files without depending on external package internals.
#' Key functions include:
#' \itemize{
#'   \item \code{\link{prm_parse}}: Parse a \code{.prm} file into an R list
#'   \item \code{\link{prm_set}}: Modify parameter values
#'   \item \code{\link{prm_write}}: Write parameters back to a PRM file
#'   \item \code{\link{prm_defaults}}: Load bundled default templates
#'   \item \code{\link{prm_validate}}: Validate parameters against a reference version
#' }
#'
#' @param cas Case table (\code{satscan_table} of kind "cas") or \code{ss_tbl} of type "cas".

#' @param pop Population table (\code{satscan_table} of kind "pop", optional).
#'   Required for Poisson model. Created via \code{\link{prep_pop}}.
#' @param geo Geometry table (\code{satscan_table} of kind "geo"). Created via \code{\link{prep_geo}}.
#' @param ctl Control table (\code{satscan_table} of kind "ctl", optional).
#'   Required for Bernoulli model. Created via \code{\link{prep_ctl}}.
#' @param grd Grid table (\code{satscan_table} of kind "grd", optional).
#'   Custom scan centers. Created via \code{\link{prep_grd}}.
#' @param prm_path Path to a template \code{.prm} file to load configuration from (Level 3).
#'   If NULL, uses bundled defaults.
#' @param output_dir Directory to copy final results to. If NULL, results remain in temp.
#' @param verbose Logical. Print progress and debug info.
#' @param ... Additional SaTScan parameters (Level 2 Tweaks).
#'   See \code{\link{satscan_parameters}} for a full list. Common parameters include:
#'   \itemize{
#'     \item \code{AnalysisType}: 1=Purely Spatial, 2=Purely Temporal, 3=Retrospective Space-Time,
#'       4=Prospective Space-Time, 5=Spatial Variation in Temporal Trends
#'     \item \code{ModelType}: 0=Discrete Poisson, 1=Bernoulli, 2=Space-Time Permutation
#'     \item \code{ScanAreas}: 1=High Rates, 2=Low Rates, 3=Both
#'     \item \code{MonteCarloReps}: Number of Monte Carlo replications (e.g., 999, 9999)
#'   }
#' @return A \code{satscan_result} object containing:
#'   \itemize{
#'     \item \code{cluster_summary}: Data frame of detected clusters with p-values and relative risks
#'     \item \code{location_summary}: Data frame of all locations with cluster assignments
#'     \item \code{main_results}: Full time-series data merged with results (if \code{merge_time_series=TRUE})
#'     \item \code{raw_output}: Raw SaTScan output (col, gis, rr, shapefile, etc.)
#'     \item \code{prm}: Final \code{prm_list} object with all parameters used
#'     \item \code{work_dir}: Path to output directory
#'   }
#'
#' @references
#' Kulldorff M. (1997). A spatial scan statistic.
#' Communications in Statistics: Theory and Methods, 26:1481-1496.
#'
#' Kulldorff M. (2022). SaTScan User Guide for version 10.1.
#' \url{https://www.satscan.org/}
#'
#' @seealso
#' \code{\link{prep_cas}}, \code{\link{prep_geo}}, \code{\link{prep_pop}} for input preparation.
#' \code{\link{satscan_parameters}} for parameter reference.
#' \code{\link{prm_parse}}, \code{\link{prm_set}} for advanced parameter manipulation.
#'
#' @examples
#' \dontrun{
#' # Basic Poisson analysis
#' cas <- prep_cas(cases_df, loc_id = id, time = date, cases = n, time_precision = "day")
#' geo <- prep_geo(locations_sf, loc_id = id)
#' pop <- prep_pop(pop_df, loc_id = id, time = year, pop = population)
#'
#' result <- satscanr(cas, pop = pop, geo = geo, verbose = TRUE)
#'
#' # View clusters
#' print(result)
#' summary(result)
#'
#' # With custom parameters (Level 2 tweaks)
#' result2 <- satscanr(cas,
#'     pop = pop, geo = geo,
#'     AnalysisType = 3,
#'     MonteCarloReps = 9999,
#'     MaxTemporalSize = 50
#' )
#'
#' # Using a template PRM file (Level 3)
#' result3 <- satscanr(cas,
#'     pop = pop, geo = geo,
#'     prm_path = "my_template.prm",
#'     verbose = TRUE
#' )
#' }
#'
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
    # Using internal helper check_ss_input


    if (!check_ss_input(cas, "cas")) stop("cas input must be a satscan_table of kind 'cas' or ss_tbl of type 'cas'")
    if (!check_ss_input(geo, "geo")) stop("geo input must be a satscan_table of kind 'geo' or ss_tbl of type 'geo'")
    if (!is.null(pop)) {
        if (!check_ss_input(pop, "pop")) stop("pop input must be a satscan_table of kind 'pop' or ss_tbl of type 'pop'")
    }


    # 3. Write Files (Level 1 Physical Basis)
    f_cas <- file.path(work_dir, "epid.cas")
    f_geo <- file.path(work_dir, "epid.geo")
    f_pop <- if (!is.null(pop)) file.path(work_dir, "epid.pop") else NULL
    f_ctl <- if (!is.null(ctl)) file.path(work_dir, "epid.ctl") else NULL
    f_grd <- if (!is.null(grd)) file.path(work_dir, "epid.grd") else NULL

    write_ss_file_wrapper(cas, f_cas)
    write_ss_file_wrapper(geo, f_geo)
    if (!is.null(f_pop)) write_ss_file_wrapper(pop, f_pop)
    if (!is.null(f_ctl)) write_ss_file_wrapper(ctl, f_ctl)
    if (!is.null(f_grd)) write_ss_file_wrapper(grd, f_grd)


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
    # Using internal helpers get_ss_spec, get_ss_data


    # Determine Time Precision
    tp_map <- list(generic = 0, year = 1, month = 2, day = 3)
    tp_char <- get_ss_spec(cas, "time_precision")
    tp_int <- if (is.null(tp_char)) 0 else tp_map[[tp_char]]

    # Critical Overrides - these always win (use .strict=FALSE for files missing some keys)
    prm <- prm_set(prm,
        CaseFile = basename(f_cas),
        CoordinatesFile = basename(f_geo),
        CoordinatesType = if (get_ss_spec(geo, "coord_type") == "cartesian") 0 else 1,
        PrecisionCaseTimes = tp_int,
        TimeAggregationUnits = tp_int,
        ResultsFile = "epid.txt",
        # Ensure outputs are DBF/SHP for our parser
        MostLikelyClusterEachCentroidDBase = "y",
        MostLikelyClusterCaseInfoEachCentroidDBase = "y",
        CensusAreasReportedClustersDBase = "y",
        IncludeRelativeRisksCensusAreasDBase = "y",
        SaveSimLLRsDBase = "y",
        OutputShapefiles = "y",
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
        cas_data = get_ss_data(cas),
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
    if (is.null(ss_full_path)) stop("SaTScan path not set. Please use set_satscan_path().")
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
    geo_df <- get_ss_data(geo)
    geo_spec_coord <- get_ss_spec(geo, "coord_type")

    if (inherits(geo, "ss_tbl")) {
        # ss_tbl has user columns, map them using roles
        id_col <- ss_roles(geo)[["loc_id"]]
        c1_col <- ss_roles(geo)[["coord1"]]
        c2_col <- ss_roles(geo)[["coord2"]]

        geo_for_parse <- geo_df |> dplyr::rename(id = !!rlang::sym(id_col))

        if (geo_spec_coord == "latlong") {
            geo_for_parse <- geo_for_parse |> dplyr::rename(lat = !!rlang::sym(c1_col), long = !!rlang::sym(c2_col))
        } else {
            geo_for_parse <- geo_for_parse |> dplyr::rename(x = !!rlang::sym(c1_col), y = !!rlang::sym(c2_col))
        }
    } else {
        # Legacy satscan_table has normalized 'loc_id', 'coord1', 'coord2'
        geo_for_parse <- geo_df |> dplyr::rename(id = loc_id)
        if (geo_spec_coord == "latlong") {
            geo_for_parse <- geo_for_parse |> dplyr::rename(lat = coord1, long = coord2)
        } else {
            geo_for_parse <- geo_for_parse |> dplyr::rename(x = coord1, y = coord2)
        }
    }


    res <- parse_satscan_output(
        ss_results = ss_res,
        data = get_ss_data(cas),
        geo_df = geo_for_parse,
        id_quo = if (inherits(cas, "ss_tbl")) rlang::sym(ss_roles(cas)[["loc_id"]]) else rlang::quo(loc_id),
        output_dir = if (!is.null(output_dir)) output_dir else work_dir,
        verbose = verbose
    )

    res$work_dir <- if (!is.null(output_dir)) output_dir else work_dir

    # Return the prm_list for reference
    res$prm <- prm

    return(res)
}
