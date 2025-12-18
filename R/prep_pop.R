#' Prepare SaTScan Population File
#'
#' `r lifecycle::badge("superseded")`
#'
#' Prepares a population file for SaTScan, used as the denominator for Poisson models.
#' Population data is treated as "census anchors" - SaTScan interpolates between these time points.
#'
#' @seealso [ss_pop()] for the new `ss_tbl` interface.
#'
#' @section SaTScan File Specification:
#' The Population File has the following structure:
#' \code{<LocationID> <Year/Time> <Population> <Covariate1> ...}
#'
#' \itemize{
#'   \item \strong{LocationID}: Character or numeric identifier. Match cases and geo.
#'   \item \strong{Year/Time}: The "census year" or time point. For daily analysis, you can still use
#'     yearly population anchors (e.g. 2023, 2024). SaTScan interpolates linearly.
#'   \item \strong{Population}: The count of people at risk.
#'   \item \strong{Covariates}: Optional. If used, population must be stratified by these covariates
#'     (i.e., you need a row for every combination of Location, Time, and Covariate levels).
#' }
#'
#' @param df Data frame containing population data.
#' @param loc_id Column name for location ID (unquoted).
#' @param time Column name for time/year (unquoted). Population times are anchors.
#' @param pop Column name for population count (unquoted).
#' @param covars Character vector of covariate column names.
#' @return A \code{satscan_table} object of kind "pop".
#'
#' @examples
#' \dontrun{
#' # Basic yearly population
#' head(pop_data)
#' #   county year  pop
#' # 1    001 2020 5000
#' # 2    001 2021 5100
#'
#' pop_obj <- prep_pop(pop_data, loc_id = county, time = year, pop = pop)
#'
#' # Stratified by sex
#' head(pop_strat)
#' #   county year  pop sex
#' # 1    001 2020 2500   M
#' # 2    001 2020 2500   F
#'
#' pop_obj2 <- prep_pop(pop_strat, loc_id = county, time = year, pop = pop, covars = "sex")
#' }
#' @importFrom rlang enquo eval_tidy quo_is_null
#' @export
prep_pop <- function(df, loc_id, time, pop, covars = NULL) {
    id_quo <- rlang::enquo(loc_id)
    time_quo <- rlang::enquo(time)
    pop_quo <- rlang::enquo(pop)

    if (rlang::quo_is_null(id_quo)) stop("loc_id is required")
    if (rlang::quo_is_null(time_quo)) stop("time is required for population anchors")
    if (rlang::quo_is_null(pop_quo)) stop("pop is required")

    # Build base df
    out_df <- data.frame(
        loc_id = as.character(rlang::eval_tidy(id_quo, df)),
        stringsAsFactors = FALSE
    )

    # Time (Year or other)
    # SaTScan population files usually use "Year" or "Time"
    # We assume the user provides a format that matches their case file Precision,
    # OR it's just a year string for yearly population.
    # We don't force formatting here as strictly as cases, but simple string conversion is safe.
    raw_time <- rlang::eval_tidy(time_quo, df)
    out_df$time <- as.character(raw_time)

    # Population
    out_df$pop <- rlang::eval_tidy(pop_quo, df)

    # Covariates
    if (!is.null(covars)) {
        for (cov in covars) {
            if (!cov %in% names(df)) stop(sprintf("Covariate '%s' not found", cov))
            out_df[[cov]] <- df[[cov]]
        }
    }

    # Column order: LocationID, Time, Population, [Covariates]
    desired_cols <- c("loc_id", "time", "pop")
    if (!is.null(covars)) desired_cols <- c(desired_cols, covars)

    out_df <- out_df[, desired_cols, drop = FALSE]

    satscan_table(
        data = out_df,
        kind = "pop",
        spec = list(covars = covars)
    )
}
