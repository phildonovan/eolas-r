# Default API base. Read from the EOLAS_BASE_URL env var so a dev/staging host
# can be selected with `EOLAS_BASE_URL=... R` or an .Renviron entry (previously
# this was a hardcoded literal, so the env var was silently inert). Read at load;
# a per-call override is still the explicit `base_url=` argument.
.eolas_default_base_url <- function() {
  Sys.getenv("EOLAS_BASE_URL", unset = "https://api.eolas.nz")
}
EOLAS_BASE_URL <- .eolas_default_base_url()

# Per-session runtime memo (R has no client object):
#   $arrow_supported  NULL = unknown (try it), TRUE = server speaks Arrow,
#                      FALSE = server ignored format=arrow (old; skip retry)
#   $arrow_nagged      TRUE once we've told a no-arrow user about the speedup
.eolas_runtime <- new.env(parent = emptyenv())

.eolas_user_agent <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("eolas")),
    error = function(e) "1.0.0"
  )
  # Explicit UA: good API-client hygiene + insulation against the Cloudflare
  # edge tightening bot rules (raw default UAs can be 403'd; custom always OK).
  paste0("eolas-r/", ver, " (r; +https://eolas.nz)")
}

eolas_http_perform <- function(req) {
  httr2::req_perform(req)
}

# Sanitise an error `detail` before it reaches cli_abort. A CF 5xx / origin error
# often delivers a multi-KB HTML page as the body; resp_body_string() then puts the
# whole page into `detail`, and cli_abort dumps thousands of chars of HTML at the
# user. Detect an HTML/Cloudflare body and replace it with a short message (+ cf-ray
# when present); cap any other runaway detail. Mirrors the Python client's
# _sanitize_error_detail. (2026-07-05 client audit EH-8.)
.eolas_sanitize_detail <- function(detail, resp) {
  if (!is.character(detail) || length(detail) != 1L || is.na(detail)) {
    return("Unknown error")
  }
  looks_html <- grepl("<html|<!doctype|<head|cloudflare|just a moment|_incapsula",
    detail,
    ignore.case = TRUE
  )
  if (looks_html) {
    cfray <- httr2::resp_header(resp, "cf-ray")
    msg <- "upstream returned an HTML error page (likely a Cloudflare or gateway error) -- retry"
    if (!is.null(cfray)) msg <- paste0(msg, " (cf-ray ", cfray, ")")
    return(msg)
  }
  if (nchar(detail) > 500L) {
    return(paste0(substr(detail, 1L, 500L), "... (truncated)"))
  }
  detail
}

eolas_check_status <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status == 200L) {
    return(invisible(resp))
  }

  # Double-tryCatch: first try JSON, then plain string, then synthesise a
  # message from the status code alone. The innermost fallback is critical for
  # CF 504/521/522 and origin-timeout responses that deliver an empty body --
  # resp_body_string() calls resp_body_raw() which cli_abort()s on 0-byte bodies,
  # producing a confusing internal traceback instead of a clear "retry" message.
  body <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(e) {
      tryCatch(
        list(detail = httr2::resp_body_string(resp)),
        error = function(e2) {
          list(detail = sprintf(
            "Empty response body (status %d). Likely CF gateway or origin timeout -- retry.",
            httr2::resp_status(resp)
          ))
        }
      )
    }
  )
  detail <- .eolas_sanitize_detail(body$detail %||% "Unknown error", resp)

  if (status == 401L) {
    cli::cli_abort(c(
      "Authentication error: invalid or missing API key.",
      "i" = "Check the key, or set a new one with {.fn eolas_key_save} or the {.envvar EOLAS_API_KEY} environment variable.",
      "i" = "Get a free key at {.url https://eolas.nz/signup}"
    ), call. = FALSE)
  }
  # 403 detail is passed through verbatim. Used for Enterprise-only endpoints
  # (e.g. `eolas_integration()`) where the server's message tells the caller
  # exactly which upgrade they need.
  if (status == 403L) {
    cli::cli_abort("Authentication error: {detail}", call. = FALSE)
  }
  if (status == 429L) {
    retry <- httr2::resp_header(resp, "Retry-After")
    limit <- httr2::resp_header(resp, "X-RateLimit-Limit")
    reset <- httr2::resp_header(resp, "X-RateLimit-Reset")
    cfray <- httr2::resp_header(resp, "cf-ray")
    msg <- "Rate limit reached."
    if (!is.null(limit)) msg <- paste0(msg, " Plan limit: ", limit, " requests.")
    if (!is.null(retry)) {
      msg <- paste0(msg, " Retry after ", retry, "s.")
    } else if (!is.null(reset)) {
      msg <- paste0(msg, " Resets at ", reset, ".")
    }
    # A 429 with our X-RateLimit-* headers came from the API; one with only a
    # cf-ray was thrown at the Cloudflare edge before reaching the origin.
    if (!is.null(cfray) && is.null(limit)) {
      msg <- paste0(msg, " (Blocked at the Cloudflare edge -- cf-ray ", cfray, ".)")
    }
    cli::cli_abort(paste0(msg, " Upgrade for higher limits: https://eolas.nz/pricing"),
      call. = FALSE
    )
  }
  if (status == 404L) cli::cli_abort("Not found: {detail}", call. = FALSE)
  cli::cli_abort("API error (HTTP {status}): {detail}", call. = FALSE)
}

eolas_http_get <- function(path, ..., base_url = EOLAS_BASE_URL) {
  key <- eolas_get_key_internal()
  url <- paste0(base_url, path)
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    # Total-request timeout so a black-holed connection can't hang the caller forever
    # (2026-07-05 client audit EH-1). 120s is ample for JSON metadata/data calls; the
    # bulk streaming builders set their own, larger budget.
    httr2::req_timeout(120) |>
    httr2::req_url_query(...) |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- eolas_http_perform(req)
  eolas_check_status(resp)
  resp
}
