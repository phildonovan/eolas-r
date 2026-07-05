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
