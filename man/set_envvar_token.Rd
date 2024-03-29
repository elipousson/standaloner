% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set.R
\name{set_envvar_token}
\alias{set_envvar_token}
\alias{get_envvar_token}
\title{Set or get a token from your  \code{.Renviron} file}
\usage{
set_envvar_token(
  token,
  install = FALSE,
  overwrite = FALSE,
  default = "TOKEN",
  quiet = FALSE,
  call = caller_env()
)

get_envvar_token(
  token = NULL,
  default = "TOKEN",
  message = NULL,
  call = caller_env(),
  ...
)
}
\arguments{
\item{token}{A personal access token, API key, or other environmental
variable.}

\item{install}{Add your token to your \code{.Renviron} for use in future sessions.}

\item{overwrite}{If \code{TRUE}, overwrite any existing token in \code{.Renviron},}

\item{default}{Default name used for environmental variable where the token
is stored.}

\item{quiet}{If \code{TRUE}, suppress messages by setting the cli.default_handler
option to \code{suppressMessages()}.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}

\item{message}{Error message passed to \code{\link[cli:cli_abort]{cli::cli_abort()}} if token can't be
found.}

\item{...}{
  Arguments passed on to \code{\link[cli:cli_abort]{cli::cli_abort}}
  \describe{
    \item{\code{.envir}}{Environment to evaluate the glue expressions in.}
    \item{\code{.frame}}{The throwing context. Used as default for
\code{.trace_bottom}, and to determine the internal package to mention
in internal errors when \code{.internal} is \code{TRUE}.}
  }}
}
\description{
This function is based on \code{\link[tidycensus:census_api_key]{tidycensus::census_api_key()}} by Kyle Walker.
}
