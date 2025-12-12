#' Write SatScan Population File
#'
#' Writes population data to SatScan .pop file format.
#'
#' @param data Processed population data
#' @param output_path Path to output file
#' @param mode "simple" or "stratified"
#'
#' @return Output file path (invisibly)
#'
#' @details
#' File formats per SatScan manual:
#' - Simple: location_id, year, population
#' - Stratified: location_id, year, population, age_group, sex
#'
#' @examples
#' write_satscan_pop(pop_data, "output/satscan.pop", mode = "simple")
#'
#' @export
write_satscan_pop <- function(data, output_path, mode = "simple") {
  
  if (mode == "simple") {
    # Simple: location_id, year, population
    required_cols <- c("location_id", "year", "population")
    output_data <- data[, required_cols]
  } else if (mode == "stratified") {
    # Stratified: location_id, year, population, age_group, sex
    required_cols <- c("location_id", "year", "population", "age_group", "sex")
    output_data <- data[, required_cols]
  } else {
    stop("mode must be 'simple' or 'stratified'")
  }
  
  # Validate required columns exist
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Write without headers or quotes
  write.table(
    output_data,
    output_path,
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  
  message("Wrote SatScan population file: ", output_path)
  message("  Mode: ", mode)
  message("  Rows: ", nrow(output_data))
  message("  Total population: ", format(sum(data$population), big.mark = ","))
  
  invisible(output_path)
}
