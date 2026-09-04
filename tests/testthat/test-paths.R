# Regression tests for project-root discovery.
#
# The bug these guard against: find_project_root() looked only for a .Rproj
# file, which .gitignore deliberately excludes because RStudio rewrites it on
# every open. A fresh clone therefore has no .Rproj, so every CI run of this
# suite died before the first test with "Could not locate the project root",
# while the same command passed on a developer machine.

test_that("a checkout without a .Rproj file still resolves", {
  parent <- tempfile("root")
  root <- file.path(parent, "checkout")
  dir.create(file.path(root, "tests"), recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  file.create(file.path(root, "renv.lock"))

  expected <- normalizePath(root, winslash = "/", mustWork = TRUE)

  expect_equal(find_project_root(root), expected)
  # And from a subdirectory, which is how the report and ingest scripts call it.
  expect_equal(find_project_root(file.path(root, "tests")), expected)
})

test_that("a .Rproj file is still accepted on its own", {
  parent <- tempfile("root")
  root <- file.path(parent, "checkout")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  file.create(file.path(root, "Whatever.Rproj"))

  expect_equal(
    find_project_root(root),
    normalizePath(root, winslash = "/", mustWork = TRUE)
  )
})
