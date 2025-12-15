# Script to verify which SatScan call works (CLI vs GUI)

library(rsatscan)

working_dir <- tempdir()
project_name <- "test_satscan"

# Create dummy files
# Case
cases <- data.frame(
    id = 1:5,
    cases = c(100, 0, 0, 0, 0), # Force cluster at id 1
    date = "2024/01/01"
)
write.cas(cases, working_dir, project_name)

# Pop
pop <- data.frame(
    id = 1:5,
    year = 2024,
    pop = 100
)
write.pop(pop, working_dir, project_name)

# Geo
geo <- data.frame(
    id = 1:5,
    lat = 1:5,
    long = 1:5
)
write.geo(geo, working_dir, project_name)

# Create minimal param file options
ss.options(reset = TRUE)
ss.options(list(
    CaseFile = paste0(project_name, ".cas"),
    PopulationFile = paste0(project_name, ".pop"),
    CoordinatesFile = paste0(project_name, ".geo"),
    PrecisionCaseTimes = 3,
    StartDate = "2024/01/01",
    EndDate = "2024/01/01",
    AnalysisType = 1,
    ModelType = 0,
    MonteCarloReps = 0,
    ResultsFile = paste0(project_name, ".txt")
))

write.ss.prm(working_dir, project_name)

# print("=== TEST 1: Using MacOS/SaTScan (What I implemented) ===")
# tryCatch(
#     {
#         rsatscan::satscan(
#             working_dir,
#             project_name,
#             sslocation = "/Applications/SaTScan.app/Contents/MacOS",
#             ssbatchfilename = "SaTScan",
#             verbose = TRUE,
#             cleanup = FALSE
#         )
#     },
#     error = function(e) print(e)
# )

print("\n\n=== TEST 2: Using app/satscan (Legacy style) ===")
res <- tryCatch(
    {
        prm_path <- file.path(working_dir, paste0(project_name, ".prm"))
        if (file.exists(prm_path)) {
            message("\n--- Content of ", prm_path, " ---")
            cat(readLines(prm_path), sep = "\n")
            message("--- End of prm ---\n")
        }
        rsatscan::satscan(
            working_dir,
            project_name,
            sslocation = "/Applications/SaTScan.app/Contents/app",
            ssbatchfilename = "satscan",
            verbose = FALSE,
            cleanup = FALSE
        )
    },
    error = function(e) {
        print(paste("Error running SatScan:", e$message))
        return(NULL)
    }
)

if (!is.null(res)) {
    print("=== RESULT ANALYSIS ===")
    print(paste("Class of res:", class(res)))
    print(paste("Names of res:", paste(names(res), collapse = ", ")))
    if (!is.null(res$col)) {
        print("Class of $col:")
        print(class(res$col))
        print("Column names of $col:")
        print(colnames(res$col))
    }
    if (!is.null(res$gis)) {
        print("Class of $gis:")
        print(class(res$gis))
        print("Column names of $gis:")
        print(colnames(res$gis))
    }
}
```
