#' Prepare SaTScan Control File (Bernoulli)
#'
#' `r lifecycle::badge("superseded")`
#'
#' Prepares a control file for Bernoulli models.
#' Similar to \code{prep_cas}, but for controls (non-cases).
#'
#' @seealso [ss_ctl()] for the new `ss_tbl` interface.
#'
#' @section SaTScan File Specification:
#' The Control File has the following structure:
#' \code{<LocationID> <NoControls> <Date> <Covariate1> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Identifier matching Case/Geo files.
#'   \item \strong{NoControls}: Number of controls.
#'   \item \strong{Date}: Required for Space-Time Bernoulli. Same format rules as Case file.
#'   \item \strong{Covariates}: Optional.
#' }
#'
#' @param df Data frame containing control data.
#' @param loc_id Column name for location ID (unquoted).
#' @param time Column name for time/date (unquoted, optional). Required for Space-Time analyses.
#' @param cases Column name for control counts (unquoted, optional). If NULL, assumes 1 control per row.
#' @param covars Character vector of covariate column names.
#' @param style Input style: "casewise" or "aggregated".
#' @param time_precision Time resolution: "day", "month", "year", or "generic".
#' @return A \code{satscan_table} object of kind "ctl".
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @keywords internal
prep_ctl <- function(df, loc_id, time = NULL, cases = NULL, covars = NULL,
                     style = c("casewise", "aggregated"),
                     time_precision = c("day", "month", "year", "generic")) {
    style <- match.arg(style)
    time_precision <- match.arg(time_precision)

    id_quo <- rlang::enquo(loc_id)
    time_quo <- rlang::enquo(time)
    cases_quo <- rlang::enquo(cases)

    if (rlang::quo_is_null(id_quo)) stop("loc_id is required")

    # Extract ID
    raw_id <- as.character(rlang::eval_tidy(id_quo, df))

    # Extract Cases (Controls)
    if (rlang::quo_is_null(cases_quo)) {
        raw_cases <- rep(1L, nrow(df))
    } else {
        raw_cases <- rlang::eval_tidy(cases_quo, df)
    }

    tmp <- data.frame(
        loc_id = raw_id,
        cases = raw_cases,
        stringsAsFactors = FALSE
    )

    # Extract Time if present
    if (!rlang::quo_is_null(time_quo)) {
        raw_time <- rlang::eval_tidy(time_quo, df)
        tmp$time <- raw_time
    }

    # Add Covariates
    if (!is.null(covars)) {
        for (cov in covars) {
            if (!cov %in% names(df)) stop(sprintf("Covariate '%s' not found", cov))
            tmp[[cov]] <- df[[cov]]
        }
    }

    # SPARSITY: Remove zero-control rows
    if (any(tmp$cases == 0, na.rm = TRUE)) {
        tmp <- tmp[tmp$cases > 0 & !is.na(tmp$cases), , drop = FALSE]
    }

    # Format Time logic (same as prep_cas)
    if ("time" %in% names(tmp)) {
        if (time_precision == "day") {
            d <- tryCatch(as.Date(tmp$time), error = function(e) as.Date(paste0(tmp$time, "-01-01")))
            tmp$time <- format(d, "%Y/%m/%d")
        } else if (time_precision == "month") {
            d <- as.Date(paste0(as.character(tmp$time), "-01"), format = "%Y-%m-%d")
            if (all(is.na(d))) d <- as.Date(tmp$time)
            tmp$time <- format(d, "%Y/%m")
        } else if (time_precision == "year") {
            tmp$time <- format(as.Date(paste0(tmp$time, "-01-01")), "%Y")
            if (all(is.na(tmp$time))) tmp$time <- as.character(tmp$time)
        }
    }

    # Column selection
    desired_cols <- c("loc_id", "cases")
    if ("time" %in% names(tmp)) desired_cols <- c(desired_cols, "time")
    if (!is.null(covars)) desired_cols <- c(desired_cols, covars)

    out_df <- tmp[, desired_cols, drop = FALSE]

    satscan_table(
        data = out_df,
        kind = "ctl",
        spec = list(
            time_precision = time_precision,
            covars = covars
        )
    )
}
