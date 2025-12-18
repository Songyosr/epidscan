library(testthat)

test_that("new_ss_tbl validation works", {
    df <- data.frame(id = 1:3, vals = c(10, 20, 30))

    # Success
    expect_error(new_ss_tbl(df, "cas", roles = c(loc_id = "id", cases = "vals")), NA)

    # Invalid type
    expect_error(new_ss_tbl(df, "invalid_type", roles = c(loc_id = "id")), "should be one of")

    # Missing required roles
    expect_error(new_ss_tbl(df, "cas", roles = c(loc_id = "id")), "Missing required roles")

    # Missing columns in data
    expect_error(new_ss_tbl(df, "cas", roles = c(loc_id = "id", cases = "missing_col")), "Missing columns in `data`")
})

test_that("as_satscan_case creates valid objects", {
    df <- data.frame(
        county = c("A", "B"),
        cases = c(10, 20),
        date = as.Date(c("2021-01-01", "2021-01-02")),
        age = c(1, 2)
    )

    ss <- as_satscan_case(df, loc_id = "county", cases = "cases", time = "date", covars = "age")

    expect_s3_class(ss, "ss_tbl")
    expect_equal(ss_type(ss), "cas")
    expect_equal(ss_roles(ss)[["loc_id"]], "county")
    expect_equal(ss_roles(ss)[["time"]], "date")
    expect_equal(ss_spec(ss)$covars, "age")
})

test_that("as_satscan_neighbors handles wide columns correctly", {
    df <- data.frame(
        id = c(1, 2),
        n1 = c(2, 1),
        n2 = c(3, 3)
    )

    ss <- as_satscan_neighbors(df, loc_id = "id", neighbor_cols = c("n1", "n2"))

    expect_equal(ss_type(ss), "nbr")
    expect_equal(ss_spec(ss)$neighbor_cols, c("n1", "n2"))

    # Verify output order includes neighbors
    expect_equal(ss_order(ss), c("id", "n1", "n2"))
})

test_that("format_satscan_time works correctly", {
    dates <- as.Date(c("2021-01-01", "2021-12-31"))

    expect_equal(format_satscan_time(dates, "day"), c("2021/01/01", "2021/12/31"))
    expect_equal(format_satscan_time(dates, "month"), c("2021/01", "2021/12"))
    expect_equal(format_satscan_time(dates, "year"), c("2021", "2021"))

    # Character inputs
    expect_equal(format_satscan_time(c("2021-01", "2021-02"), "month"), c("2021/01", "2021/02"))

    # Generic pass-through
    expect_equal(format_satscan_time(c(1, 2, 3), "generic"), c("1", "2", "3"))
})

test_that("write_satscan outputs correct content and order", {
    tmp <- tempfile(fileext = ".cas")
    df <- data.frame(
        id = c("A", "B"),
        cases = c(10, 20),
        extra = c("ignor", "ed"),
        date = as.Date("2021-01-01")
    )

    # Basic case file: id, cases. Extra columns passed but NOT mapped.
    ss <- as_satscan_case(df, loc_id = "id", cases = "cases")
    write_satscan(ss, tmp)

    res <- read.table(tmp)
    expect_equal(ncol(res), 2) # Should only write mapped columns
    expect_equal(res[, 1], c("A", "B"))
    expect_equal(res[, 2], c(10, 20))

    # Case file with Time + mapped optional column (attribute)
    # We trick it by mapping 'extra' to 'attribute' role
    ss2 <- new_ss_tbl(df, "cas", roles = c(loc_id = "id", cases = "cases", attribute = "extra"))
    write_satscan(ss2, tmp)

    res2 <- read.table(tmp)
    expect_equal(ncol(res2), 3)
    # Order should be ID, Cases, Attribute (based on schema optional_roles order)
    expect_equal(res2[, 3], c("ignor", "ed"))
})

test_that("write_satscan handles neighbors wide format", {
    tmp <- tempfile(fileext = ".nbr")
    df <- data.frame(
        id = c(1, 2),
        n1 = c(2, 1),
        n2 = c(3, 4)
    )

    ss <- as_satscan_neighbors(df, loc_id = "id", neighbor_cols = c("n1", "n2"))
    write_satscan(ss, tmp)

    res <- read.table(tmp)
    expect_equal(ncol(res), 3) # id, n1, n2
    expect_equal(res[, 1], c(1, 2))
    expect_equal(res[, 2], c(2, 1))
    expect_equal(res[, 3], c(3, 4))
})

test_that("write_satscan formats time at write-time", {
    tmp <- tempfile(fileext = ".cas")
    df <- data.frame(
        id = "A",
        cases = 1,
        date = as.Date("2021-01-01")
    )

    # Specifying month precision
    ss <- as_satscan_case(df, loc_id = "id", cases = "cases", time = "date", time_precision = "month")
    write_satscan(ss, tmp)

    res <- read.table(tmp)
    # ID, Cases, Time
    expect_equal(names(res), c("V1", "V2", "V3"))
    expect_equal(res$V3, "2021/01")
})

test_that("as_satscan_coordinates handles sf objects", {
    skip_if_not_installed("sf")

    # Create a simple SF object
    df <- data.frame(id = "A", lat = 10, long = 20)
    # Mocking SF structure if sf not available (tough), or just use sf functions
    # Assuming test environment has sf if it installed the package

    # Create simple polygon or point
    pt <- sf::st_sfc(sf::st_point(c(20, 10)), crs = 4326) # Long=20, Lat=10
    sf_obj <- sf::st_sf(id = "A", geometry = pt)

    # Convert
    ss <- as_satscan_coordinates(sf_obj, loc_id = "id", coord_type = "latlong")

    # Check structure
    expect_equal(ss_type(ss), "geo")
    # Should have extracted coords. Lat(10) -> coord1, Long(20) -> coord2 for 'latlong' type?
    # Wait, my logic:
    # if final_type == "latlong": roles = c(coord1 = "__ss_c2", coord2 = "__ss_c1")
    # __ss_c2 is Lat (Y), __ss_c1 is Long (X)
    # So coord1 (1st col in file) = Lat = 10
    # coord2 (2nd col in file) = Long = 20

    # Verify values by peeking at data
    expect_equal(ss[["__ss_c2"]], 10)
    expect_equal(ss[["__ss_c1"]], 20)

    # Check spec
    expect_equal(ss_spec(ss)$coord_type, "latlong")
})
