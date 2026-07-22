# eolas 1.9.1

* **`geometry = FALSE` now skips the column at READ time, not after.** Previously
  the whole Parquet was read -- including the WKT -- and the column was dropped
  afterwards, paying the full parse and peak memory for data immediately
  discarded. `eolas_get_local()` now passes a `col_select` projection to
  `arrow::read_parquet()`, so those column chunks are never decoded, and the
  entire sf/sfarrow/WKB conversion path is skipped. Geometry is typically ~95% of
  a spatial layer's bytes.
* The cached file is unchanged -- one artifact still serves both variants, so
  upgrading a non-spatial read back to spatial re-reads the same local file with
  no re-download.
* `eolas_get_local()` gains a `geometry` argument, mirroring `eolas_get()`.
* New dependency: `tidyselect` (already an indirect `arrow` dependency).

# eolas 1.9.0

Version jumps 1.4.0 -> 1.9.0 to clear a band of PyPI versions (1.5.0-1.8.0)
burned by yanked May-2026 uploads of the sibling `eolas-data` package. Yanked
filenames are permanently reserved, so those numbers can never be published
again. Both clients move together to keep their versions aligned.


* **`eolas_get(geometry = FALSE)` omits the `geometry_wkt` column.** Two-thirds
  of eolas datasets (1017/1536) carry geometry, and on TA/RC boundary tables the
  WKT dwarfs the attributes you actually wanted. The column is now projected away
  at the API's storage layer, so it is never read from S3 or transferred -- this
  cuts I/O, not just payload. Responses carry `X-Eolas-Geometry-Omitted: true`.
* **Whole-dataset pulls of small spatial tables now work.** Geometry was one of
  the two triggers for the API's large-dataset guard, so
  `eolas_get("some_boundary_table")` was diverted to a bulk download. With
  `geometry = FALSE` the client keeps such calls on the live path, matching the
  server's relaxed guard. The row-count trigger is unchanged -- dropping a column
  does not reduce row count.
* **`geometry = FALSE` is honoured on the bulk-routed path too.** A spatial table
  over the 100k-row threshold stays blocked even with `geometry = FALSE`, so the
  call routes to the bulk cache -- which has no server-side projection. The flag
  was dropped at that hand-off, so the caller silently received the full
  geometry-bearing file and, with `as_sf = NULL`, an auto-converted `sf` object.
  Found in peer review.
* `geometry = FALSE` with `as_sf = TRUE` now errors: there would be no geometry
  to convert.

# eolas 1.3.22

* **Faster failure against an unreachable API host.** `eolas_get()` first tries
  the Arrow wire format, then falls back to JSON. A transport failure (timeout /
  connection / DNS) on the Arrow attempt was swallowed and retried as JSON, so an
  unreachable host paid the request timeout **twice** (~2x latency, ~240s at the
  default budget). Transport failures are now re-raised immediately after the
  first attempt; the JSON fallback is still used when a server simply does not
  speak Arrow.

# eolas 1.3.21

* **`EOLAS_BASE_URL` environment variable is now honoured.** The default API
  base was previously a hardcoded literal, so setting `EOLAS_BASE_URL` (e.g. to
  point at a dev/staging host) had no effect. It is now read at load, so
  `EOLAS_BASE_URL=... R` or an `.Renviron` entry selects the host. A per-call
  override is still the explicit `base_url=` argument.

# eolas 1.3.20

Hotfix release addressing issues found in the 2026-07-05 client-library audit.

* **Fix broken install on distro-R (httr2 floor).** The package uses
  `httr2::req_perform_connection()` (added in httr2 1.1.0) for all streaming
  downloads, but declared only `httr2 (>= 1.0.0)`. On systems with a pre-existing
  httr2 1.0.x the install "succeeded" and then every bulk / large / geospatial
  download crashed with `'req_perform_connection' is not an exported object`. The
  dependency floor is now `httr2 (>= 1.1.0)`.
* **Request timeouts everywhere.** Added `httr2::req_timeout()` to every request
  builder (data, bulk, changelog). A black-holed connection can no longer hang the
  caller indefinitely.
* **Cleaner errors on gateway failures.** A Cloudflare / origin 5xx that returns an
  HTML error page is now reported as a short message (with the `cf-ray` when present)
  instead of dumping the multi-kilobyte HTML body into the error.
* **`eolas_download_bulk()` rejects unknown arguments.** A misspelled argument (e.g.
  `dest_dir=` instead of `path=`) now errors instead of being silently ignored.
* **Corrected bulk-download docs.** The `freshness` help and the 402 error message
  no longer claim Free plans get a monthly bulk snapshot — bulk download is a
  Pro/Enterprise feature and Free keys receive HTTP 402 (query datasets with
  `eolas_get()` instead).
* **Honest install note.** The README no longer promises "no compiler needed" on
  Linux — `arrow`/`sf` may compile from source; documents the Posit Package
  Manager / r2u binary path.
