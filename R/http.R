EOLAS_BASE_URL <- "https://api.eolas.fyi"

# Per-session runtime memo (R has no client object):
#   $arrow_supported  NULL = unknown (try it), TRUE = server speaks Arrow,
#                      FALSE = server ignored format=arrow (old; skip retry)
#   $arrow_nagged      TRUE once we've told a no-arrow user about the speedup
.eolas_runtime <- new.env(parent = emptyenv())

.eolas_user_agent <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("eolas")),
                  error = function(e) "1.0.0")
  # Explicit UA: good API-client hygiene + insulation against the Cloudflare
  # edge tightening bot rules (raw default UAs can be 403'd; custom always OK).
  paste0("eolas-r/", ver, " (r; +https://eolas.fyi)")
}

eolas_http_perform <- function(req) {
  httr2::req_perform(req)
}

eolas_check_status <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status == 200L) return(invisible(resp))

  # Double-tryCatch: first try JSON, then plain string, then synthesise a
  # message from the status code alone. The innermost fallback is critical for
  # CF 504/521/522 and origin-timeout responses that deliver an empty body —
  # resp_body_string() calls resp_body_raw() which cli_abort()s on 0-byte bodies,
  # producing a confusing internal traceback instead of a clear "retry" message.
  body <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(e) tryCatch(
      list(detail = httr2::resp_body_string(resp)),
      error = function(e2) list(detail = sprintf(
        "Empty response body (status %d). Likely CF gateway or origin timeout — retry.",
        httr2::resp_status(resp)
      ))
    )
  )
  detail <- body$detail %||% "Unknown error"

  if (status == 401L) stop(
    "Authentication error: invalid or missing API key. ",
    "Check the key, or set a new one with eolas_key_save() or the ",
    "EOLAS_API_KEY environment variable. Get a free key at https://eolas.fyi/signup",
    call. = FALSE
  )
  # 403 detail is passed through verbatim. Used for Enterprise-only endpoints
  # (e.g. `eolas_integration()`) where the server's message tells the caller
  # exactly which upgrade they need.
  if (status == 403L) stop(paste0("Authentication error: ", detail), call. = FALSE)
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
      msg <- paste0(msg, " (Blocked at the Cloudflare edge — cf-ray ", cfray, ".)")
    }
    stop(paste0(msg, " Upgrade for higher limits: https://eolas.fyi/pricing"), call. = FALSE)
  }
  if (status == 404L) stop(paste0("Not found: ", detail), call. = FALSE)
  stop(paste0("API error (HTTP ", status, "): ", detail), call. = FALSE)
}

eolas_http_get <- function(path, ..., base_url = EOLAS_BASE_URL) {
  key <- eolas_get_key_internal()
  url <- paste0(base_url, path)
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_user_agent(.eolas_user_agent()) |>
    httr2::req_url_query(...) |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- eolas_http_perform(req)
  eolas_check_status(resp)
  resp
}
