# Debug script to inspect rsatscan output
library(rsatscan)
devtools::load_all(".")

set_satscan_path("/Applications/SaTScan.app/Contents/app/satscan")

df <- data.frame(
  id = rep(letters[1:5], each=30),
  date = rep(seq.Date(as.Date("2024-01-01"), by="day", length.out=30), 5),
  cases = c(rep(0, 120), rep(3, 30)),
  pop = 1000,
  lat = c(rep(1:4, each=30), rep(5, 30)),
  long = 100
)

# Manually call helpers
prep <- prepare_satscan_data(df, rlang::enquo(cases), rlang::enquo(pop), 
                              rlang::enquo(date), rlang::enquo(id), 
                              rlang::enquo(lat), rlang::enquo(long))
files <- write_satscan_files(prep, tempdir())
opts <- build_satscan_options(files, prep$export_df, 3, "space-time", "poisson")
opts$MonteCarloReps <- 9
ss_result <- run_satscan(tempdir(), opts, FALSE)

cat("=== DEBUG OUTPUT ===\n")
cat("class(ss_result):", class(ss_result), "\n")
cat("names(ss_result):", names(ss_result), "\n")

if (!is.null(ss_result$col)) {
  cat("class(ss_result$col):", class(ss_result$col), "\n")
  cat("length:", length(ss_result$col), "\n")
  if (is.data.frame(ss_result$col)) {
    print(head(ss_result$col))
  } else {
    cat("first 3 elements:", head(ss_result$col, 3), "\n")
  }
} else {
  cat("ss_result$col is NULL\n")
}

cat("\nFiles in tempdir:\n")
print(list.files(tempdir(), pattern="epid|run"))
