# Credential resolution.
#
# Secrets live in two places and never in the repository: a machine-wide file at
# <home>/.secrets/api.env, and an optional project-local .env for per-project
# overrides. The machine-wide file is read first so that the project file wins
# on any key defined in both.
#
# Nothing here ever prints a secret value. env_status() reports whether a key is
# present and how long it is, which is enough to diagnose a bad paste without
# writing the key into a terminal transcript.
#
# DO NOT use "~" to find the home directory. On Windows, R expands it via the
# R_USER variable, which the installer commonly points at the user's Documents
# folder -- and when that folder is redirected to OneDrive, "~" resolves to a
# cloud-synced directory. Measured on this machine:
#
#   path.expand("~")  ->  C:/Users/franc/OneDrive/Documentos
#   USERPROFILE       ->  C:/Users/franc          <- where .secrets actually is
#
# So "~/.secrets/api.env" silently looks in the wrong place, and any secret
# written there would be uploaded to a cloud account. user_home() below resolves
# the real profile directory instead.

PROJECT_ENV_FILE <- ".env"

#' The user's home directory, resolved without relying on "~".
#'
#' USERPROFILE first (Windows, unset elsewhere), then HOME (Unix, and usually
#' correct on Windows too), and only then the tilde as a last resort.
#'
#' @return An absolute path.
user_home <- function() {
  for (variable in c("USERPROFILE", "HOME")) {
    value <- Sys.getenv(variable, unset = "")
    if (nzchar(value) && dir.exists(value)) {
      return(normalizePath(value, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
}

#' Path to the machine-wide credential file.
#'
#' A function rather than a constant, because the home directory has to be
#' resolved at call time rather than baked in when the file is sourced.
#'
#' @return An absolute path.
user_env_file <- function() {
  file.path(user_home(), ".secrets", "api.env")
}

#' Warn if a credential path sits inside a cloud-synced folder.
#'
#' A secret in OneDrive, Dropbox or iCloud is a secret on someone else's
#' infrastructure. This check would have caught the tilde bug above immediately,
#' which is the argument for keeping it.
#'
#' @param path Path to check.
#' @return Invisibly, TRUE when the path looks synced.
warn_if_synced <- function(path) {
  synced <- grepl("OneDrive|Dropbox|Google ?Drive|iCloud", path, ignore.case = TRUE)
  if (synced) {
    warning(
      "Credential path is inside a cloud-synced folder:\n  ", path,
      "\n  Move it to a local directory. Secrets should not be uploaded.",
      call. = FALSE
    )
  }
  invisible(synced)
}

#' Load credentials into the R session environment.
#'
#' Safe to call repeatedly; later calls simply re-read the files.
#'
#' @param user_file Machine-wide credential file. Read first.
#' @param project_file Project-local credential file. Read second, so it wins.
#' @return Invisibly, a character vector of the files that were actually read.
load_env <- function(user_file = user_env_file(),
                     project_file = PROJECT_ENV_FILE) {
  loaded <- character(0)
  for (path in c(user_file, project_file)) {
    if (file.exists(path)) {
      readRenviron(path)
      loaded <- c(loaded, path)
    }
  }
  warn_if_synced(user_file)
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
      "  Set it in ", PROJECT_ENV_FILE, " or ", user_env_file(),
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

#' Report which credential files were looked for, and whether they were found.
#'
#' Print this before diagnosing a missing credential. A file that is never read
#' because the path resolved somewhere unexpected looks identical, from the
#' error message alone, to a file with the key missing from it.
#'
#' @return A data frame of candidate paths and whether each exists.
env_paths <- function() {
  candidates <- c(machine = user_env_file(), project = PROJECT_ENV_FILE)
  data.frame(
    scope = names(candidates),
    path = unname(candidates),
    exists = file.exists(unname(candidates)),
    stringsAsFactors = FALSE
  )
}

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
