#' Prepare SaTScan Case File
#'
#' `r lifecycle::badge("superseded")`
#'
#' Prepares a case file for SaTScan, enforcing strict sparsity (no zero-case rows)
#' and handling time precision formatting.
#'
#' @seealso [ss_cas()] for the new `ss_tbl` interface.
#'
#' @section SaTScan File Specification:
#' The Case File has the following structure:
#' \code{<LocationID> <NoCases> <Date> <Covariate1> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Character or numeric identifier. Matching ID must exist in Geo file.
#'   \item \strong{NoCases}: Number of cases. For "casewise" style without a count column, this is set to 1.
#'     Zero-case rows are removed (implicit zeros).
#'   \item \strong{Date}: Formatted according to \code{time_precision}:
#'     \itemize{
#'       \item "day": \code{YYYY/MM/DD}
#'       \item "month": \code{YYYY/MM}
#'       \item "year": \code{YYYY}
#'       \item "generic": Time string/number
#'     }
#'   \item \strong{Covariates}: Optional categorical variables.
#' }
#'
#' @param df Data frame containing case data.
#' @param loc_id Column name for location ID (unquoted).
#' @param time Column name for time/date (unquoted, optional).
#' @param cases Column name for case counts (unquoted, optional). If NULL, assumes 1 case per row (casewise).
#' @param covars Character vector of covariate column names.
#' @param style Case input style:
#'   \itemize{
#'     \item "casewise": Each row is a single case (cases=1).
#'     \item "aggregated": Rows represent counts.
#'   }
#' @param time_precision Time resolution for formatting: "day", "month", "year", or "generic".
#' @return A \code{satscan_table} object of kind "cas".
#'
#' @examples
#' \dontrun{
#' # aggregated daily
#' head(my_daily_data)
#' #   zipcode       date cases age_group
#' # 1   10001 2023-01-01     2     child
#' # 2   10002 2023-01-01     0     child  <-- Removed
#'
#' cas_obj <- prep_cas(my_daily_data,
#'     loc_id = zipcode, time = date, cases = cases,
#'     covars = "age_group", time_precision = "day"
#' )
#'
#' # casewise
#' head(my_cases)
#' #      id diagnosis_date
#' # 1 P001     2023-01-15
#'
#' cas_obj2 <- prep_cas(my_cases,
#'     loc_id = id, time = diagnosis_date,
#'     style = "casewise", time_precision = "day"
#' )
#' }
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @importFrom dplyr select mutate arrange filter
#' @export
prep_cas <- function(df, loc_id, time = NULL, cases = NULL, covars = NULL,
                     style = c("casewise", "aggregated"),
                     time_precision = c("day", "month", "year", "generic")) {
    style <- match.arg(style)
    time_precision <- match.arg(time_precision)

    id_quo <- rlang::enquo(loc_id)
    time_quo <- rlang::enquo(time)
    cases_quo <- rlang::enquo(cases)

    # validation
    if (rlang::quo_is_null(id_quo)) stop("loc_id is required")

    # New Logic: Always extract to tmp first

    raw_id <- rlang::eval_tidy(id_quo, df)

    if (rlang::quo_is_null(cases_quo)) {
        raw_cases <- rep(1L, nrow(df))
    } else {
        raw_cases <- rlang::eval_tidy(cases_quo, df)
    }

    # Initialize tmp
    tmp <- data.frame(
        loc_id = as.character(raw_id),
        cases = raw_cases,
        stringsAsFactors = FALSE
    )

    # Add time if exists
    if (!rlang::quo_is_null(time_quo)) {
        raw_time <- rlang::eval_tidy(time_quo, df)
        tmp$time <- raw_time
    }

    # Add covariates
    if (!is.null(covars)) {
        for (cov in covars) {
            if (!cov %in% names(df)) stop(sprintf("Covariate '%s' not found", cov))
            tmp[[cov]] <- df[[cov]]
        }
    }

    # SPARSITY ENFORCEMENT
    if (any(tmp$cases == 0, na.rm = TRUE)) {
        tmp <- tmp[tmp$cases > 0 & !is.na(tmp$cases), , drop = FALSE]
    }

    # Format Time if present
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

    # Select Columns
    desired_cols <- c("loc_id", "cases")
    if ("time" %in% names(tmp)) desired_cols <- c(desired_cols, "time")
    if (!is.null(covars)) desired_cols <- c(desired_cols, covars)

    out_df <- tmp[, desired_cols, drop = FALSE]

    satscan_table(
        data = out_df,
        kind = "cas",
        spec = list(
            time_precision = time_precision,
            case_style = style,
            covars = covars
        )
    )
}
