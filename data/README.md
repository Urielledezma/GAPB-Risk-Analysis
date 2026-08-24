# Data

Two directories, one boundary between them.

| Directory | Contents | Tracked in git | Refreshed by |
|---|---|---|---|
| `raw/` | Untouched vendor extracts, including all FactSet pulls | **No** | `analysis/01_…`–`03_…` |
| `processed/` | Analysis-ready datasets derived from public sources | **Yes** | `analysis/04_build_datasets.R` |

`raw/` is excluded from version control in its entirety. It is where restricted vendor
content lands, and keeping it untracked is what allows this repository to be public
without redistributing anything it has no right to redistribute. Treat it as a local
cache: deleting it costs a re-download, never a loss.

`processed/` **is** tracked, deliberately. Every file in it is derived exclusively from
public sources and is small enough to version. Committing it is what lets a teammate
clone the repository and render every report without a single credential, and what
pins the reported figures to a dataset rather than to whatever a vendor happens to be
serving on the day of the re-render.

---

## Processed datasets

| File | Grain | Key columns | Source |
|---|---|---|---|
| `prices_daily.csv` | One row per ticker per trading day | `date`, `ticker`, `open`, `high`, `low`, `close`, `adjusted`, `volume` | Yahoo Finance / BMV |
| `returns_daily.csv` | One row per ticker per trading day | `date`, `ticker`, `log_return` | Derived from `prices_daily.csv` |
| `gdp_mx.csv` | One row per period | `date`, `period`, `gdp_growth` | INEGI |
| `rates_mx.csv` | One row per date per series | `date`, `series`, `value` | Banxico SIE |

`adjusted` is the split- and dividend-adjusted close and is the series every return
calculation uses. `close` is retained unadjusted so that price-level narratives in the
reports can reference the figure a market participant actually saw on the day.

---

## Fundamentals

Company fundamentals — quarterly EPS, margins, leverage ratios — come from FactSet and
are **not** committed. They are pulled into `raw/factset/` by
`analysis/03_ingest_fundamentals.R`, which requires an entitled API key. Reports cite
FactSet as the source for figures derived from those extracts, in the same way a
research note cites a data vendor.

Without a key, that one script fails with a named error and every other part of the
pipeline continues to work.

---

## Adding a source

1. Fetch it in a numbered script under `analysis/`, never inside a report.
2. Write the untouched response to `raw/`.
3. Reshape into a tidy, documented dataset under `processed/`.
4. Add a row to the table above, and to the source table in the project README.
5. If the licence forbids redistribution, it stays in `raw/`. No exceptions.
