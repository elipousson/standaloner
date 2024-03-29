# ---
# repo: elipousson/standaloner
# file: standalone-extra-checks.R
# last-updated: 2024-02-06
# license: https://opensource.org/license/mit/
# imports: [rlang (>= 1.0.0), cli (>= 2.5.0)]
# ---
#
# Note: the functions in this script depend on functions from
# `standalone-types-check.R` but does not import the script directly. If you
# this script, you must run `usethis::use_standalone("r-lin/rlang",
# "types-check")` first for these additional checks to work as expected.
#
# ## Changelog
#
# 2024-02-06:
# * Fix issue with passing unavailable dots. Add dots more consistently.
#
# 2023-10-10:
# * Rename package from settoken to more general name: standaloner.
#
# 2023-09-29:
# * Create file with `check_list()`, `is_url()`, `check_url()`,
# `check_inherits_any()`, `check_inherits_all()`, and `check_exclusive_args()`
#
# nocov start

#' Check if object is a list
#'
#' @noRd
check_list <- function(x,
                       allow_na = FALSE,
                       allow_empty = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (allow_na && is.na(x)) {
    return(invisible(NULL))
  }

  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  if (is_empty(x)) {
    if (allow_empty) {
      return(invisible(NULL))
    }

    cli_abort(
      "{.arg {arg}} can't be empty.",
      call = call
    )
  }

  if (is_list(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    what = "a list",
    arg = arg,
    call = call
  )
}

#' Does x match the pattern of a URL?
#' @noRd
is_url <- function(x, pattern = NULL, ...) {
  if (!is_vector(x) || is_empty(x)) {
    return(FALSE)
  }

  url_pattern <- "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  if (is.null(pattern)) return(grepl(url_pattern, x, ...))

  grepl(url_pattern, x, ...) & grepl(pattern, x, ...)
}


#' Check is a URL is valid
#' @noRd
check_url <- function(url,
                      pattern = NULL,
                      ...,
                      allow_null = FALSE,
                      arg = caller_arg(url),
                      call = caller_env()) {
  if (allow_null && is_null(url)) {
    return(invisible(NULL))
  }

  if (is_url(url, pattern = pattern)) {
    return(invisible(NULL))
  }

  check_string(
    url,
    allow_empty = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  stop_input_type(
    url,
    what = "a valid url",
    ...,
    arg = arg,
    call = call
  )
}

#' Check if x inherits any of the supplied class values and error if not
#' @inheritParams rlang::inherits_any
#' @noRd
check_inherits_any <- function(x,
                               class,
                               ...,
                               allow_null = FALSE,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (rlang::inherits_any(x, class)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    what = class,
    ...,
    arg = arg,
    call = call
  )
}

#' Check if x inherits all of the supplied class values and error if not
#' @inheritParams rlang::inherits_any
#' @noRd
check_inherits_all <- function(x,
                               class,
                               ...,
                               allow_null = FALSE,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (rlang::inherits_all(x, class)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    what = class,
    ...,
    arg = arg,
    call = call
  )
}

#' Check if x or y is not `NULL` and error if neither or both are supplied
#' @inheritParams rlang::check_exclusive
#' @noRd
check_exclusive_args <- function(x = NULL,
                                 y = NULL,
                                 require = TRUE,
                                 ...,
                                 x_arg = caller_arg(x),
                                 y_arg = caller_arg(y),
                                 call = caller_env()) {
  if (is_empty(c(x, y))) {
    if (!require) {
      return(invisible(NULL))
    }

    cli_abort(
      "One of {.arg {x_arg}} or {.arg {y_arg}} must be supplied.",
      call = call,
      ...
    )
  }

  if (!has_length(c(x, y), 2)) {
    return(invisible(NULL))
  }

  cli_abort(
    "Exactly one of {.arg {x_arg}} or {.arg {y_arg}} must be supplied.",
    call = call,
    ...
  )
}

#' Check if x has names
#' @noRd
check_has_name <- function(x,
                           nm,
                           ...,
                           allow_any = FALSE,
                           allow_null = FALSE,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  rlang::check_required(x, arg = arg, call = call)

  if (allow_null && is.null(x)) return(invisible(NULL))

  check_character(nm, call = call)

  has_nm <- rlang::has_name(x, nm)

  if (all(has_nm)) return(invisible(NULL))

  if (allow_any && any(has_nm)) return(invisible(NULL))

  n <- length(nm)
  what <- " all of"
  add_msg <- c("i" = "{.arg {arg}} is missing {.val {nm[!has_nm]}}")

  if (n == 1) {
    what <- ""
    add_msg <- NULL
  } else if (allow_any) {
    what <- " any of"
    add_msg <- NULL
  }

  cli_abort(
    message = c(
      "{.arg {arg}} must have{what} the name{cli::qty(n)}{?s} {.val {nm}}.",
      add_msg
      ),
    ...,
    .envir = current_env(),
    call = call
  )
}
