# eolas <img src="https://img.shields.io/badge/R-package-blue" align="right"/>

R client for the [eolas.fyi](https://eolas.fyi) statistical data API — 1,400+ official New Zealand statistical & geospatial datasets, plus OECD data for international comparisons, returned as tidy data frames (or `sf` objects for geospatial layers).

_Coverage is New Zealand + OECD today. Australian sources are on the roadmap — not yet available; OECD data already includes Australia (and other OECD members) for cross-country comparisons._

## Installation

```r
remotes::install_github("phildonovan/eolas-r")
```

## Save your API key (workstation)

Save your key to the OS keyring once and never paste it again:

```r
install.packages("keyring")   # one-off; on Linux: sudo apt install libsecret-1-dev first
eolas_key_save()              # interactive masked prompt
```

After that, `eolas_get_*()` finds the key automatically in every future R session — no `eolas_key()` call needed. The same OS keyring slot (`service = "eolas"`) is read by the Python `eolas-data` client, so a key saved from R is immediately usable in Python and vice versa.

Other key-management helpers:

```r
eolas_key_status()    # show which source is supplying the key + masked first 8 chars
eolas_key_clear()     # remove from OS keyring
```

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

## Bulk downloads

For whole-dataset downloads (Parquet, gzipped CSV, or GeoParquet — no row caps):

```r
eolas_download_bulk("treasury_fiscal_spending", path = "t.parquet")
# Pro/Enterprise → current Iceberg snapshot; Free → latest monthly snapshot.
# Licence-restricted datasets (OECD) raise a 403 error with the licence reason.
```

To keep a local file in sync with upstream refreshes without re-downloading unchanged data, use `eolas_sync_bulk()` — a cheap HEAD request checks the server's snapshot id and only transfers data if it changed:

```r
r <- eolas_sync_bulk("nz_cpi", path = "nz_cpi.parquet")
# r$status: "downloaded" | "unchanged" | "updated"
# r$bytes_downloaded == 0 when unchanged
```

Full docs: [docs.eolas.fyi/bulk-downloads/](https://docs.eolas.fyi/bulk-downloads/).

## License

MIT
