EOLAS_BASE_URL <- "https://api.eolas.fyi"

eolas_http_perform <- function(req) {
  httr2::req_perform(req)
}

eolas_check_status <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status == 200L) return(invisible(resp))

  body <- tryCatch(
    httr2::resp_body_json(resp),
    error = \(e) list(detail = httr2::resp_body_string(resp))
  )
  detail <- body$detail %||% "Unknown error"

  if (status == 401L) stop("Authentication error: invalid or missing API key.", call. = FALSE)
  # 403 detail is passed through verbatim. Used for Enterprise-only endpoints
  # (e.g. `eolas_integration()`) where the server's message tells the caller
  # exactly which upgrade they need.
  if (status == 403L) stop(paste0("Authentication error: ", detail), call. = FALSE)
  if (status == 429L) stop("Rate limit reached. Upgrade to Pro for unlimited access.", call. = FALSE)
  if (status == 404L) stop(paste0("Not found: ", detail), call. = FALSE)
  stop(paste0("API error (HTTP ", status, "): ", detail), call. = FALSE)
}

eolas_http_get <- function(path, ..., base_url = EOLAS_BASE_URL) {
  key <- eolas_get_key_internal()
  url <- paste0(base_url, path)
  req <- httr2::request(url) |>
    httr2::req_headers("X-API-Key" = key) |>
    httr2::req_url_query(...) |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- eolas_http_perform(req)
  eolas_check_status(resp)
  resp
}
