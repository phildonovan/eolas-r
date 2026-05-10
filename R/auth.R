# Package-level environment to hold the API key
.eolas_env <- new.env(parent = emptyenv())

#' Set your eolas API key
#'
#' Stores the key for the duration of the R session. Alternatively, set the
#' `EOLAS_API_KEY` environment variable and omit this call entirely.
#'
#' @param key An API key from <https://eolas.fyi/signup>.
#' @return The key, invisibly.
#' @export
#' @examples
#' \dontrun{
#' eolas_key("your_key_here")
#' }
eolas_key <- function(key) {
  .eolas_env$key <- key
  invisible(key)
}

eolas_get_key_internal <- function() {
  # EOLAS_API_KEY is the canonical env var. VS_API_KEY is honoured for back-compat
  # with the legacy vswarehouse package.
  key <- .eolas_env$key %||%
         Sys.getenv("EOLAS_API_KEY", unset = "") %|""|%
         Sys.getenv("VS_API_KEY", unset = "")
  if (nchar(key) == 0) {
    stop(
      "No API key found. Call eolas_key(\"...\") or set the EOLAS_API_KEY ",
      "environment variable. Get a free key at https://eolas.fyi/signup",
      call. = FALSE
    )
  }
  key
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Like `%||%` but treats empty strings as missing. Used for env-var chaining
# where Sys.getenv() returns "" when unset.
`%|""|%` <- function(x, y) if (nzchar(x %||% "")) x else y
