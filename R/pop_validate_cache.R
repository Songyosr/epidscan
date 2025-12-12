#' Validate Population Cache
#'
#' Checks if required cache files exist and warns if outdated.
#'
#' @param years Numeric vector of required years
#' @param cache_dir Cache directory path
#' @param current_year Current year for staleness check (default: current system year)
#'
#' @return Logical. TRUE if all required files exist, FALSE otherwise
#'
#' @examples
#' validate_cache(c(2023, 2025), "data/population_cache")
#'
#' @export
validate_cache <- function(years,
                           cache_dir = "data/population_cache",
                           current_year = as.numeric(format(Sys.Date(), "%Y"))) {
    all_exist <- TRUE

    for (year in years) {
        cache_file <- file.path(cache_dir, paste0("thailand_pop_", year, ".rds"))

        if (!file.exists(cache_file)) {
            warning(
                "Cache file missing for year ", year, "\n",
                "Run this command to download:\n",
                "  source('scripts/download_population_cache.R')\n",
                "  download_population_cache(years = ", year, ")",
                call. = FALSE
            )
            all_exist <- FALSE
        } else {
            # Check if cache is stale (using old data for newer analysis)
            if (year < current_year - 1) {
                warning(
                    "Using cached data from ", year, " for ", current_year, " analysis\n",
                    "Consider downloading more recent data if available:\n",
                    "  download_population_cache(years = ", current_year - 1, ":", current_year, ")",
                    call. = FALSE
                )
            }
        }
    }

    return(all_exist)
}
