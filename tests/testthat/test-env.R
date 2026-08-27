# Regression tests for credential path resolution.
#
# The bug these guard against: on Windows, R expands "~" through R_USER, which
# the installer often points at the user's Documents folder. When that folder is
# redirected to OneDrive, "~/.secrets/api.env" resolves to a cloud-synced
# directory -- so the real credential file is never found, and any secret
# written to the resolved path is uploaded. Both failure modes are silent.

test_that("user_home prefers USERPROFILE over the tilde", {
  withr_profile <- Sys.getenv("USERPROFILE", unset = NA)
  on.exit(
    if (is.na(withr_profile)) {
      Sys.unsetenv("USERPROFILE")
    } else {
      Sys.setenv(USERPROFILE = withr_profile)
    },
    add = TRUE
  )

  temp <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  Sys.setenv(USERPROFILE = temp)

  expect_equal(user_home(), temp)
})

test_that("user_home ignores a variable pointing at a directory that does not exist", {
  original <- Sys.getenv("USERPROFILE", unset = NA)
  on.exit(
    if (is.na(original)) Sys.unsetenv("USERPROFILE") else Sys.setenv(USERPROFILE = original),
    add = TRUE
  )

  Sys.setenv(USERPROFILE = file.path(tempdir(), "definitely-not-a-real-directory"))

  # Falls through to HOME or the tilde rather than returning a broken path.
  expect_true(dir.exists(user_home()))
})

test_that("the resolved home is not a cloud-synced directory", {
  # This is the assertion that fails if the tilde bug is ever reintroduced.
  expect_false(
    grepl("OneDrive|Dropbox|Google ?Drive|iCloud", user_home(), ignore.case = TRUE),
    info = paste0(
      "Resolved home is inside a cloud-synced folder: ", user_home(),
      ". Credentials must not live there."
    )
  )
})

test_that("the machine credential path sits under the resolved home", {
  path <- user_env_file()

  expect_true(startsWith(path, user_home()))
  expect_true(grepl("\\.secrets", path))
  expect_true(endsWith(path, "api.env"))
})

test_that("warn_if_synced flags a synced path and passes a local one", {
  expect_warning(
    warn_if_synced("C:/Users/someone/OneDrive/Documents/.secrets/api.env"),
    "cloud-synced"
  )
  expect_silent(warn_if_synced("C:/Users/someone/.secrets/api.env"))
})

test_that("env_paths lists both scopes without reading any value", {
  paths <- env_paths()

  expect_equal(nrow(paths), 2)
  expect_equal(paths$scope, c("machine", "project"))
  expect_true(is.logical(paths$exists))
  # Paths and existence flags only -- never a credential.
  expect_equal(names(paths), c("scope", "path", "exists"))
})

test_that("env_status reports presence and length but never a value", {
  original <- Sys.getenv("TEST_FAKE_CREDENTIAL", unset = NA)
  on.exit(
    if (is.na(original)) {
      Sys.unsetenv("TEST_FAKE_CREDENTIAL")
    } else {
      Sys.setenv(TEST_FAKE_CREDENTIAL = original)
    },
    add = TRUE
  )

  secret <- "abc123-not-a-real-key"
  Sys.setenv(TEST_FAKE_CREDENTIAL = secret)

  status <- env_status("TEST_FAKE_CREDENTIAL")

  expect_true(status$set)
  expect_equal(status$length, nchar(secret))
  expect_false(any(vapply(status, function(column) {
    any(as.character(column) == secret)
  }, logical(1))))
})

test_that("require_env names the missing variable and the files it searched", {
  Sys.unsetenv("TEST_ABSENT_CREDENTIAL")

  expect_error(
    require_env("TEST_ABSENT_CREDENTIAL"),
    "TEST_ABSENT_CREDENTIAL"
  )
  expect_error(require_env("TEST_ABSENT_CREDENTIAL"), "\\.secrets")
})

test_that("has_env treats whitespace as absent", {
  on.exit(Sys.unsetenv("TEST_BLANK_CREDENTIAL"), add = TRUE)

  Sys.setenv(TEST_BLANK_CREDENTIAL = "   ")
  expect_false(has_env("TEST_BLANK_CREDENTIAL"))

  Sys.setenv(TEST_BLANK_CREDENTIAL = "value")
  expect_true(has_env("TEST_BLANK_CREDENTIAL"))
})
