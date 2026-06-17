# eolas <img src="https://img.shields.io/badge/R-package-blue" align="right"/>

R client for the [eolas.fyi](https://eolas.fyi) statistical data API — 1,400+ official New Zealand statistical & geospatial datasets, plus OECD data for international comparisons, returned as tidy data frames (or `sf` objects for geospatial layers).

_Coverage is New Zealand + OECD today. Australian sources are on the roadmap — not yet available; OECD data already includes Australia (and other OECD members) for cross-country comparisons._

## Installation

```r
remotes::install_github("phildonovan/eolas-r")
```

## Quick setup (workstation)

Two one-off steps make every future R session frictionless:

**1. Save your API key** to the OS keyring (macOS Keychain / Windows Credential Manager / Linux Secret Service) so `eolas_get_*()` finds it automatically — no `eolas_key()` call needed:

```r
install.packages("keyring")   # one-off; on Linux: sudo apt install libsecret-1-dev first
eolas_key_save()              # interactive masked prompt
```

Other key-management helpers: `eolas_key_status()`, `eolas_key_clear()`.

**2. Set a library directory** so downloaded bulk files land somewhere permanent instead of the transient `~/.cache/eolas/` OS cache:

```r
eolas_library_set("~/eolas-library")    # writes to ~/.eolas/config.json
eolas_library_status()                  # show resolved path + source
eolas_library_clear()                   # revert to ~/.cache/eolas/ fallback
```

On first interactive use without a library configured, the package prompts you to choose one (skip with `EOLAS_LIBRARY=…` or by pre-calling `eolas_library_set()`).

Or use an env var (useful for CI, Shiny Server, systemd):

```r
Sys.setenv(EOLAS_LIBRARY = "~/eolas-library")
```

After setting the library, `eolas_get_local("nz_parcels")` and the smart-routing in `eolas_get("nz_parcels")` will use `~/eolas-library/` automatically.

The OS keyring slot and config file (`~/.eolas/config.json`) are shared with the Python `eolas-data` client, so a key or library path set from R is immediately usable in Python and vice versa.

## Quickstart

```r
library(eolas)
library(ggplot2)

eolas_key("your_api_key")   # or: eolas_key_save() once, or set EOLAS_API_KEY in .Renviron

# Generic
df <- eolas_get("nz_cpi", start = "2020-01-01")

# Source-tagged (sets the `eolas_source` attr for downstream display)
df <- eolas_get_statsnz("nz_cpi")
df <- eolas_get_oecd("nz_gdp_growth")

# Discovery
all_datasets <- eolas_list()
nz_only      <- eolas_list("Stats NZ")
meta         <- eolas_info("nz_cpi")

# Plot — use ggplot2 directly. eolas_get_* returns tidy data frames so
# `aes(date, value)` is usually all you need:
ggplot(df, aes(date, value)) + geom_line()
```

Get an API key at <https://eolas.fyi/signup>. Free plan is 10 requests/month; Pro ($49/month) is unlimited.

## Source-specific helpers

One `eolas_get_*()` and `eolas_list_*()` per source:

Core: `statsnz`, `statsnz_geo`, `oecd`, `rbnz`, `treasury`, `linz`, `lris`, `mbie`, `nzta`, `msd`, `police`, `acc`, `edcounts`, `worksafe`, `doc`, `geonet`, `pharmac`, `eeca`, `immigration`, `charities`. Auckland: `akl_council`, `akl_transport`. Regional-council bundles: `northland`, `bay_of_plenty`, `hawkes_bay`, `taranaki`, `manawatu_whanganui`, `wellington`, `top_of_south`, `west_coast`, `otago`, `southland`, `napier_whanganui`, `colab_waikato`, `ecan_canterbury`.

Run `eolas_list()` for the full live catalogue — the set above is generated and grows; `eolas_list()` is always authoritative.

## Integrations (Enterprise plan)

Generate ready-to-run connector configs for popular data-pipeline tools (Meltano, Fivetran, Azure Data Factory) directly from R:

```r
# In-memory: inspect what the server would generate
result <- eolas_integration("meltano", c("nz_cpi", "nz_gdp_growth"))
names(result$files)            # "meltano.yml", "README.md", ".env.example"
cat(result$files$meltano.yml)

# Or write straight to a directory ready for `meltano install`
eolas_integration(
  "meltano",
  c("nz_cpi", "nz_gdp_growth"),
  output_dir = "./my-pipeline"
)
```

This is an Enterprise-plan feature. Non-Enterprise keys see the server's upgrade-pointer error message; the gating lives server-side so it's bypass-proof. See <https://eolas.fyi/#pricing>.

## Geospatial

Datasets with a `geometry_wkt` column auto-convert to `sf` objects (CRS WGS84) when the `sf` package is available:

```r
install.packages("sf")
gdf <- eolas_get("nz_addresses")          # sf object
df  <- eolas_get("nz_addresses", as_sf = FALSE)  # plain df, WKT preserved
```

## Working with large geo datasets

The 5.4M-row `linz.nz_parcels` table allocates ~10 GB when materialised as an `sf` object. Pass `as_arrow = TRUE` to skip all geometry materialisation and get a zero-copy `arrow::Table` instead — geometry stays as character WKT until you need it:

```r
# Zero-copy Arrow table — no sf allocation
tbl <- eolas_get_linz("nz_parcels", as_arrow = TRUE)

# Filter before materialising — dramatically cheaper than loading the full sf object
library(duckdb)
con <- dbConnect(duckdb())
result <- dbGetQuery(con, "
  SELECT parcel_id, geometry_wkt
  FROM tbl
  WHERE ST_Within(ST_GeomFromText(geometry_wkt),
                  ST_GeomFromText('POLYGON((174.7 -41.3, 174.8 -41.3, 174.8 -41.4, 174.7 -41.4, 174.7 -41.3))'))
")
```

`as_arrow = TRUE` works on all datasets (geo or non-geo), all routing modes (`mode = "live"`, `"cached"`, `"auto"`), and all `eolas_get_*()` source helpers. It cannot be combined with `as_sf = TRUE`.

## Faster transport (Arrow)

`eolas_get()` (and every `eolas_get_*()`) automatically uses Apache Arrow as
the wire format when the `arrow` package is installed — typically **5–10×
faster end-to-end** on large pulls, with no code change. It falls back to
JSON transparently if `arrow` isn't installed or the server doesn't support
it (you'll get a one-time hint about the speed-up).

```r
install.packages("arrow")          # one-off; everything else is unchanged
df <- eolas_get("nz_addresses")    # now streamed via Arrow, same data frame
```

For a columnar file straight from the REST API:

```bash
curl -H "X-API-Key: $EOLAS_API_KEY" \
  "https://api.eolas.fyi/v1/datasets/nz_cpi/data?format=parquet" -o nz_cpi.parquet
```

See the [R reference](https://docs.eolas.fyi/r/reference/) for the format benchmark.

## Bulk downloads — `eolas_get()` is now smart

`eolas_get()` auto-routes large or geospatial datasets through the cache+sync path — no code change needed. `eolas_get_linz("nz_parcels")` used to take 15 minutes (live Iceberg scan); it now returns an `sf` object in seconds.

```r
# Smart default: nz_parcels auto-routes to CDN-cached GeoParquet, no limit needed
gdf <- eolas_get_linz("nz_parcels")   # sf object in seconds
df  <- eolas_get("nz_cpi")            # small dataset -> stays on live path

# Escape hatches when you need explicit control:
gdf <- eolas_get("nz_parcels", mode = "live")      # force live Iceberg scan (server returns 413
                                                    # if dataset is large/geo and no filter is set
                                                    # — apply limit/start/end or use mode="cached")
gdf <- eolas_get("nz_parcels", mode = "cached")    # force cache+sync (= eolas_get_local)
```

**Routing rules (`mode = "auto"`, the default):**
1. If `start`, `end`, or `limit` is set -> always live (slice queries can't use a whole-file cache).
2. If the dataset is licence-restricted (`bulk_export_class = "none"`, e.g. OECD) -> always live.
3. If bulk-eligible AND (has geometry OR >100k rows) -> cache+sync path.
4. Otherwise -> live.

`eolas_get_local()` is the explicit alias for `mode = "cached"` — use it when you need to control `cache_dir`, `format`, or `freshness`:

```r
# Explicit cache+sync with extra options
gdf <- eolas_get_local("nz_parcels")
df  <- eolas_get_local("nz_cpi", cache_dir = "/data/eolas", format = "csv_gz")
df  <- eolas_get_local("nz_parcels", as_sf = FALSE)   # plain data.frame, no sf conversion
```

For advanced control over the sync lifecycle, use `eolas_sync_bulk()` directly. For one-shot downloads to a raw vector or file path, use `eolas_download_bulk()`:

```r
r <- eolas_sync_bulk("nz_cpi", path = "nz_cpi.parquet")
# r$status: "downloaded" | "unchanged" | "updated"; r$bytes_downloaded == 0 when unchanged
eolas_download_bulk("treasury_fiscal_spending", path = "t.parquet")
```

**Progress bars:** `eolas_download_bulk`, `eolas_sync_bulk`, and `eolas_get_local` all show a `cli` progress bar automatically in interactive R sessions, so 1+ GB files are never silent. Pass `progress = FALSE` to suppress in scripts, or set `EOLAS_NO_PROGRESS=1` in the environment for a batch/CI-wide escape hatch. The bar falls back gracefully to an indeterminate spinner when the server doesn't send a `Content-Length` header.

Full docs: [docs.eolas.fyi/bulk-downloads/](https://docs.eolas.fyi/bulk-downloads/).

## Sync — always-fresh local copy

`eolas_sync(name, path)` keeps a local file current, automatically choosing *how* based on the dataset's CDC serving tier — you make the same call either way:

- **snapshot-tier** datasets → full-snapshot download, re-fetched only when the server snapshot changes (`eolas_sync_bulk()`).
- **changelog-tier** datasets (e.g. the LINZ SCD2 layers) → incremental: the first call downloads a baseline, then later calls fetch *only what changed* from the `/changes` feed and pk-merge it into your file (`eolas_sync_changes()`).

```r
# Same call regardless of tier:
r <- eolas_sync("nz_building_outlines", path = "buildings.parquet")
r$status        # "downloaded" (baseline) | "updated" | "unchanged"
r$sync_mode     # "changelog" for changelog-tier datasets
r$ops_applied   # number of change rows applied this run
r$current_seq   # feed watermark after this sync

# First call baselines; subsequent calls apply only new changes:
r <- eolas_sync("nz_building_outlines", path = "buildings.parquet")
r$ops_applied   # e.g. 1240
```

A sidecar at `paste0(path, ".eolas-meta.json")` records the snapshot id / feed watermark so the next call fetches only new data. For SCD2 datasets the merge keeps only the current rows (`is_current = true`), so `buildings.parquet` is always a clean current-state snapshot — the SCD2 history is handled for you. A `410` (watermark expired) self-heals by re-baselining.

The changelog sidecar (`schema_version` 2) is byte-compatible with the Python `eolas-data` client: a file synced from Python can be resumed from R and vice versa.

## License

MIT
