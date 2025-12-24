library(epidscan)
library(dplyr)
# Try to load rsatscan if available, else skip
if (requireNamespace("rsatscan", quietly=TRUE)) {
    data(NMgeo, package="rsatscan")
    message("NMgeo loaded. Head:")
    print(head(NMgeo))
    
    message("Detecting coords:")
    print(epidscan:::detect_coordinates(NMgeo))
} else {
    message("rsatscan not installed.")
}
