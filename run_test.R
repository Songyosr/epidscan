library(testthat)
devtools::load_all(".")
test_file("tests/testthat/test-workflow-lepto.R", reporter = "progress")
