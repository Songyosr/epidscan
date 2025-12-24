# Process NMlung Dataset from inst/extdata to data/

# Paths
raw_dir <- "data-raw/NMlung"
cas_file <- file.path(raw_dir, "lung.cas")
pop_file <- file.path(raw_dir, "nm-m.pop")
geo_file <- file.path(raw_dir, "nm-km.geo")

# ---- Helper: Decode Factors ----
# Sex: 1=Male, 2=Female
decode_sex <- function(x) {
    factor(x, levels = c(1, 2), labels = c("Male", "Female"))
}

# Age: 1..18
# 1 = <5, 2=5-9, ..., 17=80-84, 18=85+
# Logic:
# 1 -> 0
# 2 -> 5
# ...
# i -> (i-1)*5
age_labels <- c(
    "<5",
    paste(seq(5, 80, by = 5), seq(9, 84, by = 5), sep = "-"),
    "85+"
)
decode_age <- function(x) {
    factor(x, levels = 1:18, labels = age_labels)
}

# ---- 1. Process Case File (lung.cas) ----
# Cols: County, Cases, MonthIdx, Age, Sex
d_cas <- read.table(cas_file, header = FALSE, col.names = c("county", "cases", "time_idx", "age_group", "sex"))

# Decode Time (Index 2001 = Jan 1973)
# Year = 1973 + (Idx - 2001) %/% 12
# Month = (Idx - 2001) %% 12 + 1
# Date = YYYY-MM-01
years <- 1973 + (d_cas$time_idx - 2001) %/% 12
months <- (d_cas$time_idx - 2001) %% 12 + 1
d_cas$date <- as.Date(sprintf("%04d-%02d-01", years, months))

# Decode Factors
d_cas$sex <- decode_sex(d_cas$sex)
d_cas$age_group <- decode_age(d_cas$age_group)

# Select and Reorder
NMlung_cas <- d_cas[, c("county", "cases", "date", "age_group", "sex")]
attr(NMlung_cas, "source") <- "NMlung/lung.cas"

# ---- 2. Process Population File (nm-m.pop) ----
# Cols: County, TimeIdx, Population, Age, Sex
# Note: TimeIdx is monthly index centered on July of each year (e.g. 2007 = July 1973).
d_pop <- read.table(pop_file, header = FALSE, col.names = c("county", "time_idx", "population", "age_group", "sex"), fill = TRUE)

# Filter invalid rows (trailing empty lines or EOF chars)
# Row 66115 has county="\032" and NA population
d_pop <- d_pop[!is.na(d_pop$county) & d_pop$county != "" & d_pop$county != "\032" & !is.na(d_pop$population), ]

# Convert to Year
# Verification: 2007 (July 1973) -> 1973 + (2007-2001)%/%12 = 1973 + 6%/%12 = 1973. Correct.
d_pop$year <- 1973 + (d_pop$time_idx - 2001) %/% 12

# Decode Factors
d_pop$sex <- decode_sex(d_pop$sex)
d_pop$age_group <- decode_age(d_pop$age_group)

# Select and Reorder
NMlung_pop <- d_pop[, c("county", "year", "population", "age_group", "sex")]
attr(NMlung_pop, "source") <- "NMlung/nm-m.pop"

# ---- 3. Process Geo File (nm-km.geo) ----
# Cols: County, X, Y (in KM)
d_geo <- read.table(geo_file, header = FALSE, col.names = c("county", "x_km", "y_km"))

NMlung_geo <- d_geo
attr(NMlung_geo, "source") <- "NMlung/nm-km.geo"
attr(NMlung_geo, "units") <- "km"

# ---- Save Data ----
save(NMlung_cas, file = "data/NMlung_cas.rda", compress = "xz", version = 2)
save(NMlung_pop, file = "data/NMlung_pop.rda", compress = "xz", version = 2)
save(NMlung_geo, file = "data/NMlung_geo.rda", compress = "xz", version = 2)

message("Datasets saved to data/")
str(NMlung_cas)
str(NMlung_pop)
str(NMlung_geo)
