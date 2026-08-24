# Credential resolution.
#
# Secrets live in two places and never in the repository: a machine-wide file at
# ~/.secrets/api.env, and an optional project-local .env for per-project
# overrides. The machine-wide file is read first so that the project file wins
# on any key defined in both.
#
# Nothing here ever prints a secret value. env_status() reports whether a key is
# present and how long it is, which is enough to diagnose a bad paste without
# writing the key into a terminal transcript.

USER_ENV_FILE <- "~/.secrets/api.env"
PROJECT_ENV_FILE <- ".env"

#' Load credentials into the R session environment.
#'
#' Safe to call repeatedly; later calls simply re-read the files.
#'
#' @param user_file Machine-wide credential file. Read first.
#' @param project_file Project-local credential file. Read second, so it wins.
#' @return Invisibly, a character vector of the files that were actually read.
load_env <- function(user_file = USER_ENV_FILE,
                     project_file = PROJECT_ENV_FILE) {
  loaded <- character(0)
  for (path in c(user_file, project_file)) {
    expanded <- path.expand(path)
    if (file.exists(expanded)) {
      readRenviron(expanded)
      loaded <- c(loaded, expanded)
    }
  }
  invisible(loaded)
}

#' Is a credential available?
#'
#' @param name Environment variable name.
#' @return TRUE when the variable is set and not blank.
has_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  nzchar(trimws(value))
}

#' Fetch a required credential, or fail with an actionable message.
#'
#' The error names the missing variable and where to get it, and never echoes
#' any value.
#'
#' @param name Environment variable name.
#' @param hint Short instruction on how to obtain the credential.
#' @return The credential value.
require_env <- function(name, hint = NULL) {
  load_env()
  if (!has_env(name)) {
    message <- paste0(
      "Missing credential: ", name, ".\n",
      "  Set it in ", PROJECT_ENV_FILE, " or ", USER_ENV_FILE,
      " (copy .env.example to get started)."
    )
    if (!is.null(hint)) {
      message <- paste0(message, "\n  ", hint)
    }
    stop(message, call. = FALSE)
  }
  Sys.getenv(name)
}

# Every credential the project can use. Names only -- this list is safe to
# print, and printing it is how a teammate finds out what to put in .env.
DEFAULT_CREDENTIALS <- c(
  "FACTSET_USERNAME_SERIAL",
  "FACTSET_API_KEY",
  "BANXICO_SIE_TOKEN",
  "INEGI_API_TOKEN"
)

#' Report credential availability without disclosing values.
#'
#' @param names Environment variable names to check.
#' @return A data frame with one row per variable: name, whether it is set, and
#'   its character length. Values are never included.
env_status <- function(names = DEFAULT_CREDENTIALS) {
  load_env()
  data.frame(
    variable = names,
    set = vapply(names, has_env, logical(1), USE.NAMES = FALSE),
    length = vapply(
      names,
      function(n) nchar(Sys.getenv(n, unset = "")),
      integer(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}
