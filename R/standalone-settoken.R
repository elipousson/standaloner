# ---
# repo: elipousson/settoken
# file: standalone-settoken.R
# last-updated: 2023-08-12
# license: https://opensource.org/license/mit/
# imports: [rlang (>= 1.0.0), cli (>= 2.5.0), glue]
# ---
#
# ## Changelog
#
# 2023-08-13:
# * Create file with `set_envvar_token()` and `get_envvar_token()`
#
# nocov start
#
# set_envvar_token is based on the MIT-licensed tidycensus::census_api_key()
# function.
#
# <https://github.com/walkerke/tidycensus/blob/master/LICENSE>
#
# YEAR: 2017
# COPYRIGHT HOLDER: Kyle Walker
#
#' Set or get a token from your  `.Renviron` file
#'
#' [set_envvar_token()] can set an API key or personal access token (PAT) as a
#' named environment variable temporarily for the current session or saved for
#' future sessions. This function is based on [tidycensus::census_api_key()] by
#' Kyle Walker with updates for more verbose messages using `{cli}` functions
#' and improved handling of edge cases, e.g. partial matching for token names.
#'
#' [get_envvar_token()] can return an environment variable or error if the token
#' is missing or does not match a supplied pattern.
#'
#' @param token A personal access token, API key, or other environmental
#'   variable.
#' @param install Add your token to your `.Renviron` for use in future sessions.
#' @param overwrite If `TRUE`, overwrite any existing token in `.Renviron`,
#' @param default Default name used for environmental variable where the token
#'   is stored.
#' @param quiet If `TRUE`, suppress messages by setting the
#'   `cli.default_handler` option to [suppressMessages()].
#' @inheritParams rlang::args_error_context
#' @importFrom rlang caller_env is_true local_options current_env is_null
#'   caller_call call_name
#' @importFrom cli cli_bullets cli_alert_success
#' @importFrom glue glue
#' @importFrom utils read.table write.table
set_envvar_token <- function(token,
                             install = FALSE,
                             overwrite = FALSE,
                             default = "TOKEN",
                             quiet = FALSE,
                             call = caller_env()) {
  if (is_true(quiet)) {
    local_options(
      "cli.default_handler" = suppressMessages,
      .frame = current_env()
    )
  }

  if (is_false(install)) {
    caller_name <- "set_renv_token"
    caller <- caller_call()
    if (!is_null(caller)) {
      caller_name <- call_name(caller)
    }

    cli_bullets(
      c(
        "v" = "{.envvar {default}} set to {.val {token}} with {.fn Sys.setenv}.",
        "*" = "To use this token in future sessions, call
        {.fn {caller_name}} using {.arg install = TRUE}."
      )
    )
    Sys.setenv(default = token)
    return(invisible(token))
  }

  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (file.exists(renv)) {
    default_match <- grepl(paste0("^", default, "(?=\\=)"),
      readLines(renv),
      perl = TRUE
    )

    has_default <- any(default_match)

    if (has_default && !overwrite) {
      cli_abort(
        c("{.envvar {default}} already exists in your {.file .Renviron}.",
          "*" = "Set {.arg overwrite = TRUE} to replace this token."
        ),
        call = call
      )
    }
    backup <- file.path(home, ".Renviron_backup")
    file.copy(renv, backup)
    cli_alert_success("{.file .Renviron} backed up to {.path {backup}}.")

    if (has_default) {
      oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
      newenv <- oldenv[!default_match, ]
      utils::write.table(
        newenv, renv,
        quote = FALSE,
        sep = "\n", col.names = FALSE, row.names = FALSE
      )
    }
  } else {
    file.create(renv)
  }

  write(glue('{default}="{token}"'), renv, sep = "\n", append = TRUE)

  cli_bullets(
    c(
      "v" = "{.val {token}} saved to {.file .Renviron} variable {.envvar {default}}.",
      "*" = "Restart R or run {.code readRenviron(\"~/.Renviron\")} then use
      {.code Sys.getenv(\"{default}\")} to access the token."
    )
  )

  invisible(token)
}

#' @rdname set_envvar_token
#' @name get_envvar_token
#' @param message Error message passed to [cli::cli_abort()] if token can't be
#'   found.
#' @param pattern Optional pattern passed to [grepl()] and used to validate the
#'   stored token.
#' @param perl Passed to [grepl()] if pattern is supplied. Defaults to `TRUE`.
#' @importFrom rlang caller_arg `%||%`
#' @importFrom cli cli_abort
get_envvar_token <- function(token = NULL,
                             default = "TOKEN",
                             message = NULL,
                             pattern = NULL,
                             perl = TRUE,
                             call = caller_env(),
                             ...) {
  if (!is_string(default)) {
    cli_abort(
      "{.arg default} must be a string not {.obj_type_friendly {default}}",
      call = call
    )
  }

  token <- token %||% Sys.getenv(default)

  if (!is_empty(token) && !identical(token, "")) {
    if (!is_null(pattern) && !grepl(pattern, token, perl = perl)) {
      message <- "{.arg token} must match the supplied pattern: {pattern}"
    } else {
      return(token)
    }
  }

  message <- message %||% "{.arg token} is empty and {.arg default} variable
  {.val {default}} can't be found in {.file .Renviron}"

  cli_abort(
    message = message,
    ...,
    call = call
  )
}
