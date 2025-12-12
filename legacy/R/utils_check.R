#' Check Package Installation
#'
#' Verifies that all required packages for epidscan are installed.
#'
#' @return Logical. TRUE if all packages are found.
#' @export
check_installation <- function() {
    required_packages <- c(
        "tidyverse",
        "sf",
        "terra",
        "rsatscan",
        "tidygeocoder",
        "spatstat"
    )

    missing_packages <- c()

    for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            missing_packages <- c(missing_packages, pkg)
        }
    }

    if (length(missing_packages) > 0) {
        warning("Missing packages: ", paste(missing_packages, collapse = ", "))
        return(FALSE)
    }

    message("All required packages are installed.")
    return(TRUE)
}
