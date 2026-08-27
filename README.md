# Market Risk Analysis — GAPB.MX

[![checks](https://github.com/Urielledezma/Analisis-de-Riesgo/actions/workflows/checks.yml/badge.svg)](https://github.com/Urielledezma/Analisis-de-Riesgo/actions/workflows/checks.yml)
[![R](https://img.shields.io/badge/R-%3E%3D%204.2-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-reports-39729E?logo=quarto&logoColor=white)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Reports](https://img.shields.io/badge/reports-live-brightgreen)](https://urielledezma.github.io/Analisis-de-Riesgo/)

An end-to-end market risk research framework in **R**, applied to **Grupo Aeroportuario
del Pacífico, Serie B (BMV: GAPB)** and, in the portfolio stage, to a six-asset Mexican
equity book.

Stages 01 through 04 concern GAPB alone. Stage 05 adds five further BMV issuers, because
a Value at Risk exercise needs a portfolio and a portfolio needs a covariance matrix.

The project moves from the fundamentals of the issuer to a fully backtested Value at
Risk model, in five stages: business and financial characterisation, price-path and
event attribution, return distribution and geometric Brownian motion, conditional
variance models, and portfolio VaR / Expected Shortfall. Each stage renders to a
standalone report; together they form the basis of a research article.

The analytics are deliberately kept out of the reports and live in `R/` as a small
library driven by `config/`. Point `config/assets.yml` at a different universe and the
same pipeline runs unchanged — the repository is intended to be reused.

**Reports:** <https://urielledezma.github.io/Analisis-de-Riesgo/>

---

## Research pipeline

| Stage | Question it answers | Report |
|---|---|---|
| 01 — Asset profile | What is the business, how is it financed, and what risks does it structurally carry? | `reports/01-asset-profile.qmd` |
| 02 — Price history & events | How has the price behaved, where are the drawdowns, and what real-world events explain them? | `reports/02-price-history-events.qmd` |
| 03 — Returns & GBM | Are returns normal? Is the price lognormal? What does a GBM projection imply? | `reports/03-returns-volatility-gbm.qmd` |
| 04 — Variance models | Which conditional variance model — MA, EWMA, data-driven EWMA, ARCH/GARCH — forecasts best out of sample? | `reports/04-variance-models.qmd` |
| 05 — VaR & Expected Shortfall | What is the portfolio's loss exposure, does the model survive backtesting, and which weights minimise it? | `reports/05-value-at-risk.qmd` |
| — Article | Publication-format synthesis of all five stages | `paper/article.qmd` |

---

## Repository structure

```text
.
├── R/                  # Analytics library — returns, tests, GBM, variance models, VaR
├── analysis/           # Numbered ingest scripts. The only code that touches a network
├── config/             # Asset universe and model parameters (YAML, single source of truth)
├── reports/            # Quarto sources for the five stage reports
├── paper/              # Research article (renders to .docx and .html)
├── data/
│   ├── raw/            # Untouched vendor extracts. Never committed
│   ├── private/        # FactSet-derived datasets. Never committed
│   └── processed/      # Public-source datasets. Committed, the renderable floor
├── outputs/            # Figures, fitted models, scratch. Regenerated, never committed
├── docs/               # Rendered site (GitHub Pages source)
└── tests/testthat/     # Unit tests for the analytics library
```

---

## Quickstart

**Requirements:** R ≥ 4.2, [Quarto](https://quarto.org/docs/get-started/) ≥ 1.4.
RStudio recommended. No API credentials are needed to reproduce the reports.

```bash
git clone https://github.com/Urielledezma/Analisis-de-Riesgo.git
cd Analisis-de-Riesgo
```

```r
# 1. Restore the pinned dependency set
install.packages("renv")
renv::restore()

# 2. With a FactSet key: confirm entitlement, then ingest from the vendor
source("analysis/00_probe_factset.R")
source("analysis/01_ingest_prices.R")     # add --universe for stage 05
source("analysis/03_ingest_fundamentals.R")
source("analysis/04_build_datasets.R")

# Without a key, skip step 2 entirely — the committed snapshot renders as is.
```

```bash
# 3. Render the reports into docs/
quarto render reports

# 4. Verify the analytics library
Rscript tests/testthat.R
```

Fundamentals ingestion (`analysis/03_ingest_fundamentals.R`) is the one step that
requires credentials. See [Data sources](#data-sources) below.

---

## Data sourcing and reproducibility

**FactSet is the primary source.** Every price series and all fundamentals are requested
from FactSet first. Yahoo Finance / BMV is a fallback for an unentitled key or an
outage — never a preference. The ladder lives in one place, `R/data_sources.R`, and
nothing else in the project knows where a price came from.

Every fallback is announced, every row carries a `source` column, and a provenance
manifest records which vendor served which ticker. A run that quietly degraded and a run
that used FactSet throughout must never look alike.

**Reports never call an API.** Scripts in `analysis/` fetch and write; the Quarto sources
in `reports/` read and nothing else. That separation is what pins a reported figure to a
fixed dataset rather than to whatever a vendor is serving on the day of a re-render.

**Vendor output is not committed.** FactSet's terms restrict redistribution, and that
restriction attaches to the content rather than to who holds a key — so FactSet-derived
datasets live in `data/private/`, which is untracked. The tracked snapshot in
`data/processed/` comes from public sources alone and exists as the floor that keeps the
repository renderable. `read_dataset()` prefers the vendor copy when it is present, so a
teammate with a key sees FactSet numbers and a reader without one still sees a rendered
report.

Random draws (Monte Carlo VaR, GBM path simulation) are seeded from `config/params.yml`.

---

## Data sources

| Source | Role | Used for | Access | Committed? |
|---|---|---|---|---|
| **FactSet** | **Primary** | Daily prices, quarterly EPS, margins, leverage ratios | Entitled API key | **No** |
| Yahoo Finance / BMV | Fallback | Daily OHLCV, full quotation history | Public, via `quantmod` | Yes — `data/processed/` |
| World Bank | Primary | Mexican annual real GDP growth | Public, no token | Yes |
| Banxico SIE | Optional | Reference rate, FX | Free token | Yes — derived series only |
| INEGI | Optional | Quarterly GDP | Free token | Yes — derived series only |

**FactSet content is cited, never redistributed.** Vendor extracts land in
`data/raw/factset/` and vendor-derived datasets in `data/private/`, both excluded from
version control. Figures derived from them appear in the reports with the source cited,
as any research note cites a data vendor.

Before relying on the FactSet path, settle entitlement and the response schema in one
cheap call:

```bash
Rscript analysis/00_probe_factset.R
```

It reports whether the credentials authenticate (401) as distinct from whether the
account is entitled (403), prints the field names the endpoints actually return, and
confirms the identifier convention in `config/assets.yml`.

Credentials are read from a machine-wide `.secrets/api.env` first, then from a
project-local `.env`, which takes precedence. Copy `.env.example` to `.env` to configure;
neither file is ever committed. To see which paths are searched on your machine and
whether they were found:

```r
source("R/utils_io.R"); source_lib(); env_paths()
```

The home directory is resolved from `USERPROFILE`/`HOME` rather than through `~`. On
Windows, R expands the tilde via `R_USER`, which commonly points at a OneDrive-synced
Documents folder — so `~/.secrets` would both miss the real file and place any secret
written there into cloud storage. A unit test asserts the resolved path is not inside a
synced folder.

---

## Methodology

| Area | Approach |
|---|---|
| Returns | Continuously compounded (log) returns; √252 annualisation of daily volatility |
| Distribution | Jarque–Bera normality tests on returns, price and log price; skewness and excess kurtosis; one-sample *t*-test for zero mean |
| Price dynamics | Geometric Brownian motion, closed-form expectation and 95% intervals, plus Monte Carlo paths |
| Conditional variance | Moving-average windows, EWMA with a justified λ, data-driven EWMA with an optimised α, and ARCH/GARCH selected on information criteria |
| Model evaluation | Out-of-sample comparison against realised volatility, estimation cut off at 2025-12-31 |
| Value at Risk | Historical simulation, variance–covariance, and Monte Carlo; 95 / 97.5 / 99% at 1, 5, 10 and 20-day horizons |
| Tail risk | Expected Shortfall at 99% |
| Validation | Violation-count backtesting against expected exceedances |
| Optimisation | Minimum-VaR portfolio weights, compared against the equally weighted allocation |

---

## Conventions

- `snake_case` for objects and functions; functions live in `R/`, never in a report.
- Ingest scripts are numbered by pipeline stage: `analysis/01_…` through `analysis/04_…`.
- Parameters are never hard-coded in a report. They come from `config/params.yml`.
- Tickers are never hard-coded either. They come from `config/assets.yml`.
- Code, documentation and commit messages in English; report and article prose in Spanish.
- Commits follow [Conventional Commits](https://www.conventionalcommits.org/).

---

## Roadmap

- [x] Repository scaffold, analytics library and report skeletons
- [ ] Stage 01 — issuer profile and risk taxonomy
- [ ] Stage 02 — price path, drawdowns and event attribution
- [ ] Stage 03 — return distribution and GBM projection
- [ ] Stage 04 — conditional variance models and out-of-sample evaluation
- [ ] Stage 05 — portfolio VaR, Expected Shortfall and optimisation
- [ ] Research article
- [ ] Render-in-CI once the dependency lockfile is stable

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the branching model, commit conventions and
the review checklist.

---

## License and citation

MIT — see [LICENSE](LICENSE). Citation metadata is in [CITATION.cff](CITATION.cff).
Third-party data remains subject to its own provider's terms.
