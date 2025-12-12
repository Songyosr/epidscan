# Population Model Functions
#
# This module provides functions for model-based population estimation
# as an alternative to raw population counts.

#' Estimate Expected Cases from Historical Data
#'
#' Calculates expected cases using a Poisson regression model based on historical trends.
#' This can be used as an alternative to raw population in SatScan analysis.
#'
#' @param case_data Data frame with columns: location_id, date, cases
#' @param pop_data Data frame with columns: location_id, year, population
#' @param method Character. Method for estimation: "poisson", "historical_rate", "smoothed"
#' @param baseline_years Numeric vector. Years to use for baseline calculation
#'
#' @return Data frame with columns: location_id, year, expected_cases
#'
#' @examples
#' expected <- estimate_expected_cases(
#'     case_data = cases,
#'     pop_data = population,
#'     method = "historical_rate",
#'     baseline_years = 2021:2023
#' )
estimate_expected_cases <- function(
    case_data,
    pop_data,
    method = c("historical_rate", "poisson", "smoothed"),
    baseline_years = NULL) {
    method <- match.arg(method)

    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    # Convert date to year if needed
    if ("date" %in% names(case_data)) {
        case_data <- case_data %>%
            dplyr::mutate(year = as.numeric(format(as.Date(date, "%Y/%m/%d"), "%Y")))
    }

    if (method == "historical_rate") {
        expected <- estimate_from_historical_rate(case_data, pop_data, baseline_years)
    } else if (method == "poisson") {
        expected <- estimate_from_poisson_model(case_data, pop_data)
    } else if (method == "smoothed") {
        expected <- estimate_from_smoothed_rates(case_data, pop_data, baseline_years)
    }

    return(expected)
}

#' Estimate Expected Cases from Historical Rate
#'
#' Calculates expected cases based on historical incidence rates.
#'
#' @param case_data Data frame with: location_id, year, cases
#' @param pop_data Data frame with: location_id, year, population
#' @param baseline_years Numeric vector. Years for baseline (if NULL, uses all available)
#'
#' @return Data frame with: location_id, year, expected_cases
estimate_from_historical_rate <- function(case_data, pop_data, baseline_years = NULL) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    # Aggregate cases by location and year
    case_summary <- case_data %>%
        dplyr::group_by(location_id, year) %>%
        dplyr::summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")

    # Join with population
    data <- case_summary %>%
        dplyr::left_join(pop_data, by = c("location_id", "year")) %>%
        dplyr::mutate(rate = total_cases / population * 100000) # per 100,000

    # Filter to baseline years if specified
    if (!is.null(baseline_years)) {
        baseline_data <- data %>% dplyr::filter(year %in% baseline_years)
    } else {
        baseline_data <- data
    }

    # Calculate average rate per location
    avg_rates <- baseline_data %>%
        dplyr::group_by(location_id) %>%
        dplyr::summarise(avg_rate = mean(rate, na.rm = TRUE), .groups = "drop")

    # Apply average rate to population to get expected cases
    expected <- pop_data %>%
        dplyr::left_join(avg_rates, by = "location_id") %>%
        dplyr::mutate(
            avg_rate = ifelse(is.na(avg_rate), 0, avg_rate),
            expected_cases = (avg_rate / 100000) * population
        ) %>%
        dplyr::select(location_id, year, expected_cases)

    message("Estimated expected cases using historical rates")
    if (!is.null(baseline_years)) {
        message("  Baseline years: ", paste(baseline_years, collapse = ", "))
    }

    return(expected)
}

#' Estimate Expected Cases from Poisson Regression
#'
#' Uses Poisson GLM to model case counts with temporal trends.
#'
#' @param case_data Data frame with: location_id, year, cases
#' @param pop_data Data frame with: location_id, year, population
#'
#' @return Data frame with: location_id, year, expected_cases
estimate_from_poisson_model <- function(case_data, pop_data) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    # Aggregate cases
    case_summary <- case_data %>%
        dplyr::group_by(location_id, year) %>%
        dplyr::summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")

    # Join with population
    data <- case_summary %>%
        dplyr::left_join(pop_data, by = c("location_id", "year"))

    # Fit Poisson GLM with year as predictor and log(population) as offset
    # Note: This is a simplified model; you may want to add more covariates
    tryCatch(
        {
            model <- glm(
                total_cases ~ year + offset(log(population)),
                data = data,
                family = poisson(link = "log")
            )

            # Predict expected cases
            expected <- pop_data %>%
                dplyr::mutate(expected_cases = predict(model, newdata = ., type = "response")) %>%
                dplyr::select(location_id, year, expected_cases)

            message("Estimated expected cases using Poisson regression")
            message("  Model AIC: ", round(AIC(model), 2))

            return(expected)
        },
        error = function(e) {
            warning("Poisson model failed: ", e$message)
            warning("Falling back to historical rate method")
            return(estimate_from_historical_rate(case_data, pop_data))
        }
    )
}

#' Estimate Expected Cases with Temporal Smoothing
#'
#' Smooths historical rates using moving average before projection.
#'
#' @param case_data Data frame with: location_id, year, cases
#' @param pop_data Data frame with: location_id, year, population
#' @param baseline_years Numeric vector. Years for baseline
#'
#' @return Data frame with: location_id, year, expected_cases
estimate_from_smoothed_rates <- function(case_data, pop_data, baseline_years = NULL) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    # For now, use historical rate method
    # You can enhance this with actual temporal smoothing (e.g., LOESS, splines)
    message("Using historical rate as smoothed estimate (enhance with LOESS if needed)")

    expected <- estimate_from_historical_rate(case_data, pop_data, baseline_years)

    return(expected)
}

#' Create Population File from Expected Cases
#'
#' Converts expected cases into .pop file format for SatScan.
#' Instead of using raw population, this uses expected cases as the "population" baseline.
#'
#' @param expected_cases Data frame with: location_id, year, expected_cases
#' @param output_file Character. Path to output .pop file
#'
#' @return Data frame in SatScan population format
#'
#' @examples
#' pop_expected <- create_pop_from_expected(
#'     expected_cases = expected,
#'     output_file = "data/satscan_pop_expected.pop"
#' )
create_pop_from_expected <- function(expected_cases, output_file = NULL) {
    # Format as SatScan population file: location_id, year, population
    pop_formatted <- expected_cases %>%
        dplyr::select(location_id, year, expected_cases) %>%
        dplyr::rename(population = expected_cases)

    # Write to file if specified
    if (!is.null(output_file)) {
        write.table(
            pop_formatted,
            output_file,
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE
        )
        message("Expected cases population file written to: ", output_file)
    }

    return(pop_formatted)
}

#' Compare Population Methods
#'
#' Compares raw population vs expected cases for each location.
#'
#' @param pop_raw Data frame with raw population
#' @param pop_expected Data frame with expected cases
#'
#' @return Data frame with comparison
compare_population_methods <- function(pop_raw, pop_expected) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package is required")
    }

    comparison <- pop_raw %>%
        dplyr::rename(raw_pop = population) %>%
        dplyr::left_join(
            pop_expected %>% dplyr::rename(expected_pop = population),
            by = c("location_id", "year")
        ) %>%
        dplyr::mutate(
            ratio = expected_pop / raw_pop,
            diff = expected_pop - raw_pop
        )

    message("\n=== Population Method Comparison ===")
    message("Mean ratio (expected/raw): ", round(mean(comparison$ratio, na.rm = TRUE), 3))
    message("Locations with ratio > 2: ", sum(comparison$ratio > 2, na.rm = TRUE))
    message("Locations with ratio < 0.5: ", sum(comparison$ratio < 0.5, na.rm = TRUE))

    return(comparison)
}
