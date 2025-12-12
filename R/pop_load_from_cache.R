#' Load Population Data from Cache
#'
#' Loads cached population data for specified years.
#'
#' @param years Numeric vector of years to load
#' @param cache_dir Cache directory path
#'
#' @return Data frame with combined population data from all years
#'
#' @examples
#' pop_data <- load_from_cache(c(2023, 2025))
#'
#' @export
load_from_cache <- function(years, cache_dir = "data/population_cache") {
    data_list <- list()

    for (year in years) {
        cache_file <- file.path(cache_dir, paste0("thailand_pop_", year, ".rds"))

        if (!file.exists(cache_file)) {
            stop("Cache file not found: ", cache_file, "\nRun download_population_cache() first")
        }

        data <- readRDS(cache_file)
        data$year <- year # Ensure year column exists
        data_list[[as.character(year)]] <- data

        message("Loaded ", year, ": ", nrow(data), " rows")
    }

    # Combine all years
    combined <- bind_rows(data_list)

    message("Total loaded: ", nrow(combined), " rows across ", length(years), " year(s)")

    return(combined)
}
