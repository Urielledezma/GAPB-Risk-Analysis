# Data

Three directories. The tier is decided by the **licence of the source**, not by how far
the data has been processed.

| Directory | Contents | Tracked | Written by |
|---|---|---|---|
| `raw/` | Untouched vendor extracts and cached filings | **No** | `analysis/01_…`, `03_…`, `06_…` |
| `private/` | Analysis-ready datasets **derived from FactSet** | **No** | `analysis/01_…`, `04_…` |
| `processed/` | Analysis-ready datasets derived from public sources | **Yes** | `analysis/04_…`, `05_…`, `06_…` |

`raw/sec/` holds the SEC documents `analysis/06_ingest_gap_filings.R` downloads, one
directory per accession. It is a cache rather than an extract: filings are immutable once
accepted, so a second run reads from disk and touches no network. Deleting it costs one
re-download and nothing else.

## The source ladder

FactSet remains the preferred licensed source, but the current ITESO licence is
workstation-only. The operational public path uses Yahoo Finance / BMV and records that
choice in every row and in the provenance manifest. A normalized manual FactSet export
in `private/` takes precedence when available.

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
`processed/`, so a teammate with a manual export sees vendor numbers and a reader
without one still sees a rendered report — neither changes report code.

### Current state of the FactSet path, 2026-09-01

ITESO confirmed that the institutional licence does not include API access. This closes
the 401 investigation as a licensing boundary rather than a client defect. FactSet is
therefore a manual licensed override: exports go to `raw/factset/manual/`,
`analysis/03_ingest_fundamentals.R` validates them, and normalized datasets go to
untracked `private/`. In their absence, reports use the tracked public snapshot.

---

## Datasets

| File | Tier | Grain | Key columns |
|---|---|---|---|
| `prices_daily.csv` | private or processed | One row per ticker per trading day | `date`, `ticker`, `open`, `high`, `low`, `close`, `adjusted`, `volume`, `source` |
| `returns_daily.csv` | private or processed | One row per ticker per trading day | `date`, `ticker`, `log_return`, `source` |
| `prices_provenance.csv` | follows the prices | One row per ticker and source | `ticker`, `source`, `rows`, `first`, `last` |
| `gdp_mx.csv` | processed | One row per year | `date`, `year`, `gdp_growth` |
| `rates_mx.csv` | processed | One row per date per series | `date`, `series`, `value` |
| `fundamentals_annual.csv` | private or processed | One row per fiscal year | `year`, revenue, income (consolidated and controlling), EBITDA, assets, equity (consolidated and controlling), debt by maturity, cash, `weighted_shares`, source |
| `eps_quarterly.csv` | private or processed | One row per fiscal quarter | `period_end`, `quarter`, `diluted_eps`, net and comprehensive income, `shares_outstanding`, `usdmxn`, `reported_eps_*`, `currency`, `instrument`, source |

`adjusted` is the series every return calculation uses. `close` is retained unadjusted so
price-level narratives can reference the figure a market participant actually saw.

> **Open question on the vendor series.** FactSet's `adjust` parameter controls split and
> spin-off adjustment, not dividend adjustment. Until `analysis/00_probe_factset.R` has
> been run against an entitled key, `adjusted` mirrors the close on the FactSet path.
> If the endpoint does not return a total-return series, this understates long-horizon
> returns while barely moving volatility — which makes it hard to spot and worth settling
> before stage 03.

## Fundamentals

Manual FactSet exports must match the schemas in `R/data_fundamentals.R` and land in
`raw/factset/manual/`. `analysis/03_ingest_fundamentals.R` validates them into
`private/`. Without those files, the report reads the GAP/SEC snapshot from `processed/`.

That snapshot is no longer typed by hand. `analysis/06_ingest_gap_filings.R` rebuilds it
from the filings themselves, following the manifest in `config/filings.yml`, which names
the accession and the column behind every reporting period. The annual panel comes from
one Form 20-F, so its three fiscal years share an accounting basis; the quarterly panel
comes from twenty-eight Form 6-K earnings releases.

Two properties of that dataset are worth stating because they are easy to get wrong.

**`diluted_eps` is calculated, not transcribed.** It is quarterly net income over the
share count GAP states in each release's own footnote. GAP separately publishes a figure
it calls *comprehensive income per share*, kept here as `reported_eps_mxn` and
`reported_eps_usd_ads`: that figure includes other comprehensive income and is not EPS
under IAS 33, though market aggregators republish it under that name. The two are held in
separate columns precisely so they cannot be mistaken for one another.

**The quarterly numerator is consolidated.** GAP does not split net income between the
controlling and non-controlling interests quarterly, only annually, so the quarterly
series carries the non-controlling share. The annual panel has both figures, and the
report quantifies the gap rather than hiding it.

Every run reconciles the four quarters of each fiscal year against the audited annual
figure and stops if they disagree.

## Adding a source

1. Fetch it in a numbered script under `analysis/`, never inside a report.
2. Write the untouched response to `raw/`.
3. Reshape into a tidy, documented dataset.
4. Route it by licence: restricted to `private/`, public to `processed/`.
5. Add a row to the table above and to the source table in the project README.
