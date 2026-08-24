# Test runner.
#
#   Rscript tests/testthat.R
#
# The analytics library is plain sourced files rather than a package, so the
# tests source it directly instead of calling library().

library(testthat)

source(file.path("R", "utils_io.R"))
source_lib()

test_dir("tests/testthat", stop_on_failure = TRUE)
