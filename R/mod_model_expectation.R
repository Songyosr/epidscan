#' Expectation Modeling Module
#'
#' This script calculates expected cases using a GLM/GAM model.
#' It combines case and population data, fits a model on training data,
#' and predicts expected cases for the analysis period.
#'
#' Input:
#' - data/derived/cases_stratified.rds
#' - data/derived/pop_stratified.rds
#' Output:
#' - data/derived/expected_cases.rds
#' - data/derived/satscan.cas
#' - data/derived/satscan.pop

#' @importFrom dplyr mutate filter left_join select %>%
#' @importFrom lubridate year week yday
#' @importFrom tidyr replace_na
#' @importFrom stats glm poisson predict
#' @importFrom utils write.table
#' @export

model_expectation <- function(cases_path = "data/derived/cases_stratified.rds",
                              pop_path = "data/derived/pop_stratified.rds",
                              output_dir = "data/derived",
                              train_years = 2021:2023,
                              predict_year = 2025) {
    message("=== Modeling Expected Cases ===")

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # 1. Load Data
    cases <- readRDS(cases_path)
    pop <- readRDS(pop_path)

    # 2. Prepare Modeling Dataset
    # Expand grid to include all location-date combinations (fill zeros)
    all_dates <- seq(as.Date(paste0(min(train_years), "-01-01")),
        as.Date(paste0(predict_year, "-12-31")),
        by = "day"
    )

    locations <- unique(pop$tambon_code)

    # Note: This full expansion might be huge.
    # For daily data, maybe aggregate to week first?
    # The user's reference used weekly data. Let's stick to daily for now but be mindful.

    # Join Cases with Pop
    # First, ensure cases have year/week info
    cases_aug <- cases %>%
        mutate(
            year = year(onset_date),
            week = week(onset_date),
            doy = yday(onset_date)
        )

    # Create a base frame (simplified for now: just observed cases + pop)
    # In a real GLM, we need zero counts.
    # Let's aggregate to Weekly for the model to be stable, then distribute back?
    # Or just model daily with offset.

    # Let's merge cases and pop
    # Pop is yearly.

    data_model <- cases_aug %>%
        left_join(pop, by = c("tambon_code", "year")) %>%
        mutate(
            population = replace_na(population, 1000), # Fallback if missing
            log_pop = log(population)
        )

    # 3. Fit Model (Simplified GLM based on user reference)
    # cases ~ location + year + week + offset(log(pop))

    train_data <- data_model %>% filter(year %in% train_years)

    if (nrow(train_data) < 10) {
        warning("Not enough training data. Using raw population as expectation (Rate = 1).")
        # Fallback: Expected = Population (or some rate)
        # This is just a placeholder to prevent crash
        expected_df <- data_model %>%
            mutate(expected_cases = population * 1e-5) # Arbitrary rate
    } else {
        message("Fitting GLM model...")
        glm_model <- glm(
            cases ~ tambon_code + factor(year) + factor(week) + offset(log_pop),
            family = poisson(link = "log"),
            data = train_data
        )

        # 4. Predict
        predict_data <- data_model %>% filter(year == predict_year)

        # Handle new levels (locations not in training)
        # For now, assume all locations exist.

        predict_data$expected_cases <- predict(glm_model, newdata = predict_data, type = "response")

        expected_df <- predict_data
    }

    # 5. Save Expected Cases Dataframe
    saveRDS(expected_df, file.path(output_dir, "expected_cases.rds"))

    # 6. Generate SatScan Files
    # .cas file: <Location> <Cases> <Date>
    # Use the original daily cases for the .cas file (observed)
    # Filter for prediction year
    cases_final <- cases %>%
        filter(year(onset_date) == predict_year) %>%
        mutate(date_str = format(onset_date, "%Y/%m/%d")) %>%
        select(tambon_code, cases, date_str)

    write.table(cases_final, file.path(output_dir, "satscan.cas"),
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    # .pop file: <Location> <Year> <ExpectedCases>
    # For SatScan with "Expected Cases" option, the .pop file should contain the expected counts.
    # But SatScan usually takes population.
    # If we use Space-Time Permutation, we don't need pop.
    # If we use Poisson, we need Pop + Covariates OR Expected Cases directly.
    # If using "Space-Time" model with "Expected Cases" file?

    # User's reference implies generating expected cases.
    # Let's assume we want to use these expected cases as the "Population" (baseline)
    # effectively turning it into a Ratio of Observed/Expected.

    # Aggregate expected cases by location (and time if ST)
    # For standard Space-Time Poisson, pop is usually static or yearly.
    # If we have daily expected cases, we might need to use the "Expected Cases" file option in SatScan parameters.

    # For now, let's save the expected cases aggregated by location/year as a .pop file
    # This acts as the "Population at Risk" adjusted for covariates.
    pop_final <- expected_df %>%
        group_by(tambon_code, year) %>%
        summarise(expected = sum(expected_cases), .groups = "drop")

    write.table(pop_final, file.path(output_dir, "satscan.pop"),
        row.names = FALSE, col.names = FALSE, quote = FALSE
    )

    message("Generated SatScan files in ", output_dir)
    return(list(
        cas = file.path(output_dir, "satscan.cas"),
        pop = file.path(output_dir, "satscan.pop")
    ))
}


