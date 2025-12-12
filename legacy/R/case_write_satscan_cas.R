#' Write SatScan Case File
#'
#' Writes aggregated case data to SatScan-compatible .cas file format.
#'
#' @param data Aggregated data frame (from aggregate_simple or aggregate_stratified)
#' @param output_path Character. Path to output .cas file
#' @param mode Character. Either "simple" or "stratified"
#'
#' @details
#' Simple mode format: location_id, cases, date
#' Stratified mode format: location_id, cases, date, age_group, sex
#'
#' @return Path to created file (invisibly)
#'
#' @examples
#' data <- data.frame(
#'     location_id = "900101",
#'     date = as.Date("2025-01-01"),
#'     cases = 5
#' )
#' write_satscan_cas(data, "output/satscan.cas", mode = "simple")
#'
#' @export
write_satscan_cas <- function(data, output_path, mode = "simple") {
    # Format date as YYYY/MM/DD
    data$date_str <- format(data$date, "%Y/%m/%d")

    if (mode == "simple") {
        # Simple: location_id, cases, date
        output_data <- data[, c("location_id", "cases", "date_str")]
    } else if (mode == "stratified") {
        # Stratified: location_id, cases, date, age_group, sex
        output_data <- data[, c("location_id", "cases", "date_str", "age_group", "sex")]
    } else {
        stop("mode must be 'simple' or 'stratified'")
    }

    # Write without headers or quotes
    write.table(
        output_data,
        output_path,
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE
    )

    message("Wrote SatScan case file: ", output_path)
    message("  Mode: ", mode)
    message("  Rows: ", nrow(output_data))
    message("  Total cases: ", sum(data$cases))

    invisible(output_path)
}
