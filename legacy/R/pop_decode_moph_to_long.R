#' Decode MOPH Population Data to Long Format with Custom Age Groups
#'
#' Converts MOPH wide-format population data (male_g1, female_g1, etc.) to
#' long format with custom age groupings suitable for stratified SatScan analysis.
#'
#' @param data Data frame with MOPH population columns (male_g1, female_g1, ...)
#' @param age_breaks Numeric vector of age break points. Default: c(0, 15, 60, Inf)
#' @param age_labels Character vector of age group labels. Default: c("0-14", "15-59", "60+")
#'
#' @return Data frame with columns: areacode, year, age_group, sex, population
#'
#' @details
#' MOPH Age Group Mapping (22 groups):
#' - g1: <1 year, g2: 1-4, g3: 5-9, g4: 10-14, g5: 15-19, g6: 20-24
#' - g7: 25-29, g8: 30-34, g9: 35-39, g10: 40-44, g11: 45-49, g12: 50-54
#' - g13: 55-59, g14: 60-64, g15: 65-69, g16: 70-74, g17: 75-79, g18: 80-84
#' - g19: 85-89, g20: 90-94, g21: 95-99, g22: 100+
#'
#' @examples
#' \dontrun{
#' pop_data <- readRDS("data/population_cache/thailand_pop_2024.rds")
#' long_data <- decode_moph_to_long(pop_data)
#' }
decode_moph_to_long <- function(data,
                                age_breaks = c(0, 15, 60, Inf),
                                age_labels = c("0-14", "15-59", "60+")) {
    # MOPH age group to age midpoint mapping
    age_map <- data.frame(
        age_group_num = 1:22,
        age_midpoint = c(
            0, # g1: <1 year
            2.5, # g2: 1-4 years
            7, # g3: 5-9 years
            12, # g4: 10-14 years
            17, # g5: 15-19 years
            22, # g6: 20-24 years
            27, # g7: 25-29 years
            32, # g8: 30-34 years
            37, # g9: 35-39 years
            42, # g10: 40-44 years
            47, # g11: 45-49 years
            52, # g12: 50-54 years
            57, # g13: 55-59 years
            62, # g14: 60-64 years
            67, # g15: 65-69 years
            72, # g16: 70-74 years
            77, # g17: 75-79 years
            82, # g18: 80-84 years
            87, # g19: 85-89 years
            92, # g20: 90-94 years
            97, # g21: 95-99 years
            102 # g22: 100+ years
        ),
        stringsAsFactors = FALSE
    )

    # 1. Pivot longer from wide format
    long_data <- data %>%
        tidyr::pivot_longer(
            cols = matches("(male|female)_g[0-9]+"),
            names_to = c("sex", "age_group"),
            names_pattern = "(male|female)_g([0-9]+)",
            values_to = "population"
        ) %>%
        dplyr::mutate(
            age_group_num = as.integer(age_group),
            sex = stringr::str_to_title(sex) # "Male"/"Female"
        ) %>%
        dplyr::select(-age_group) # Remove original character age_group

    # 2. Add age midpoints
    long_data <- long_data %>%
        dplyr::left_join(age_map, by = "age_group_num")

    # 3. Apply custom age breaks
    result <- long_data %>%
        dplyr::mutate(
            age_group = cut(
                age_midpoint,
                breaks = age_breaks,
                labels = age_labels,
                right = FALSE,
                include.lowest = TRUE
            )
        ) %>%
        dplyr::group_by(areacode, year, age_group, sex) %>%
        dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(location_id = areacode) # Rename for consistency with pipeline

    return(result)
}
