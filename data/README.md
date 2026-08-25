# Data

Three directories. The tier is decided by the **licence of the source**, not by how far
the data has been processed.

| Directory | Contents | Tracked | Written by |
|---|---|---|---|
| `raw/` | Untouched vendor extracts | **No** | `analysis/01_…`, `03_…` |
| `private/` | Analysis-ready datasets **derived from FactSet** | **No** | `analysis/01_…`, `04_…` |
| `processed/` | Analysis-ready datasets derived from public sources | **Yes** | `analysis/04_…`, `05_…` |

## The source ladder

**FactSet is the primary source.** `analysis/01_ingest_prices.R` requests every price
series from FactSet first and falls back to Yahoo Finance / BMV only when the key is
absent, the account is not entitled to the endpoint, or the vendor is unreachable. The
fallback is always announced in the log, every row carries a `source` column, and a
provenance manifest records which vendor served which ticker.

A run that quietly degraded to the public source and a run that used FactSet throughout
must never look alike. If they did, nobody would ever notice the difference — which is
exactly the circumstance in which a report gets published on the wrong data.

## Why FactSet output is not committed

FactSet's terms restrict redistribution. That restriction attaches to the content, not
to who holds a key, so a public repository cannot carry FactSet-derived series even when
every reader of it is separately licensed. Vendor data therefore lands in `private/`,
which is excluded from version control, and the reports cite FactSet as a source the way
any research note cites a data vendor.

The tracked snapshot in `processed/` comes from public sources alone. It is the floor
that keeps the repository renderable, and the baseline the vendor series can be checked
against. `read_dataset()` prefers `private/` when it exists and falls back to
`processed/`, so a teammate with a key sees vendor numbers and a reader without one
still sees a rendered report — neither configures anything.

---

## Datasets

| File | Tier | Grain | Key columns |
|---|---|---|---|
| `prices_daily.csv` | private or processed | One row per ticker per trading day | `date`, `ticker`, `open`, `high`, `low`, `close`, `adjusted`, `volume`, `source` |
| `returns_daily.csv` | private or processed | One row per ticker per trading day | `date`, `ticker`, `log_return`, `source` |
| `prices_provenance.csv` | follows the prices | One row per ticker and source | `ticker`, `source`, `rows`, `first`, `last` |
| `gdp_mx.csv` | processed | One row per year | `date`, `year`, `gdp_growth` |
| `rates_mx.csv` | processed | One row per date per series | `date`, `series`, `value` |

`adjusted` is the series every return calculation uses. `close` is retained unadjusted so
price-level narratives can reference the figure a market participant actually saw.

> **Open question on the vendor series.** FactSet's `adjust` parameter controls split and
> spin-off adjustment, not dividend adjustment. Until `analysis/00_probe_factset.R` has
> been run against an entitled key, `adjusted` mirrors the close on the FactSet path.
> If the endpoint does not return a total-return series, this understates long-horizon
> returns while barely moving volatility — which makes it hard to spot and worth settling
> before stage 03.

## Fundamentals

Quarterly EPS, margins and leverage ratios come from FactSet and land in
`raw/factset/`. `analysis/03_ingest_fundamentals.R` requires an entitled key.

## Adding a source

1. Fetch it in a numbered script under `analysis/`, never inside a report.
2. Write the untouched response to `raw/`.
3. Reshape into a tidy, documented dataset.
4. Route it by licence: restricted to `private/`, public to `processed/`.
5. Add a row to the table above and to the source table in the project README.
