# eolas <img src="https://img.shields.io/badge/R-package-blue" align="right"/>

R client for the [eolas.fyi](https://eolas.fyi) statistical data API — 700+ datasets across NZ, Australia, OECD, and more, returned as tidy data frames (or `sf` objects for geospatial layers).

## Installation

```r
remotes::install_github("phildonovan/eolas-r")
```

## Quickstart

```r
library(eolas)

eolas_key("your_api_key")   # or set EOLAS_API_KEY in .Renviron

# Generic
df <- eolas_get("nz_cpi", start = "2020-01-01")

# Source-tagged (sets the `eolas_source` attr, used by eolas_plot caption)
df <- eolas_get_statsnz("nz_cpi")
df <- eolas_get_oecd("nz_gdp_production_annual")

# Discovery
all_datasets <- eolas_list()
nz_only      <- eolas_list("Stats NZ")
meta         <- eolas_info("nz_cpi")

# Quick plot
eolas_plot(df)
```

Get an API key at <https://eolas.fyi/signup>. Free plan is 10 requests/month; Starter is 100; Pro is unlimited.

## Source-specific helpers

One `eolas_get_*()` and `eolas_list_*()` per source:

`statsnz`, `statsnz_geo`, `oecd`, `rbnz`, `treasury`, `linz`, `mbie`, `nzta`, `msd`, `police`, `acc`, `edcounts`, `worksafe`.

## Integrations (Enterprise plan)

Generate ready-to-run connector configs for popular data-pipeline tools (Meltano, Fivetran, Azure Data Factory) directly from R:

```r
# In-memory: inspect what the server would generate
result <- eolas_integration("meltano", c("nz_cpi", "nz_gdp"))
names(result$files)            # "meltano.yml", "README.md", ".env.example"
cat(result$files$meltano.yml)

# Or write straight to a directory ready for `meltano install`
eolas_integration(
  "meltano",
  c("nz_cpi", "nz_gdp"),
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

## Migrating from `vswarehouse`

The previous package name was `vswarehouse` (`library(vswarehouse)`). Direct equivalents:

| `vswarehouse` | `eolas` |
|---|---|
| `library(vswarehouse)` | `library(eolas)` |
| `vs_key(key)` | `eolas_key(key)` |
| `vs_list()`, `vs_info()`, `vs_get()` | `eolas_list()`, `eolas_info()`, `eolas_get()` |
| `vs_get_statsnz()`, `vs_get_oecd()`, ... | `eolas_get_statsnz()`, `eolas_get_oecd()`, ... |
| `vs_plot()` | `eolas_plot()` |
| `vs_series` class | `eolas_dataset` class |
| `attr(df, "vs_name")` / `vs_source` | `attr(df, "eolas_name")` / `eolas_source` |
| `VS_API_KEY` env var | `EOLAS_API_KEY` (legacy `VS_API_KEY` still honoured) |

The default base URL is now `https://api.eolas.fyi` (was `https://api.virtus-solutions.io`, which 301-redirects).

## License

MIT
