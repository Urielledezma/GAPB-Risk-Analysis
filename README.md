# Market Risk Analysis — GAPB.MX

[![R](https://img.shields.io/badge/R-%3E%3D%204.2-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-reports-39729E?logo=quarto&logoColor=white)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Reports](https://img.shields.io/badge/reports-live-brightgreen)](https://urielledezma.github.io/Analisis-de-Riesgo/)

An end-to-end market risk research framework in **R**, applied to **Grupo Aeroportuario
del Pacífico, Serie B (BMV: GAPB)** and to a six-asset Mexican equity portfolio.

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
├── paper/              # Research article
├── data/
│   ├── raw/            # Vendor pulls. Local only, never committed
│   └── processed/      # Public-source datasets. Committed, reproducible
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

# 2. Refresh the datasets from public sources (optional — snapshots are committed)
source("analysis/01_ingest_prices.R")
source("analysis/02_ingest_macro.R")
source("analysis/04_build_datasets.R")
```

```bash
# 3. Render the reports
quarto render
```

Fundamentals ingestion (`analysis/03_ingest_fundamentals.R`) is the one step that
requires credentials. See [Data sources](#data-sources) below.

---

## Reproducibility contract

**Reports never call an API.** Scripts in `analysis/` fetch from the network and write to
`data/processed/`; the Quarto sources in `reports/` read from `data/processed/` and
nothing else.

Three things follow from that single rule, and they are the reason it exists:

- Anyone can clone the repository and render every report with no credentials at all.
- Results are pinned to a dataset that is versioned in git, so a re-render months later
  reproduces the same numbers rather than silently picking up revised vendor data.
- Restricted vendor content never has to enter the repository to make the analysis
  verifiable.

Random draws (Monte Carlo VaR, GBM path simulation) are seeded from `config/params.yml`.

---

## Data sources

| Source | Used for | Access | Committed? |
|---|---|---|---|
| Yahoo Finance / BMV | Daily OHLCV for the six-asset universe, full quotation history | Public, via `quantmod` | Yes — `data/processed/` |
| Banxico SIE | Reference rates, FX | Free API token | Yes — derived series only |
| INEGI | Mexican GDP growth, quarterly and annual | Free API token | Yes — derived series only |
| FactSet | Company fundamentals, quarterly EPS, leverage and margin ratios | Entitled API key | **No** |

**FactSet content is not redistributed.** FactSet data is pulled locally into
`data/raw/factset/`, which is excluded from version control. Figures and ratios derived
from it appear in the reports with the source cited, as any research note would cite a
data vendor, but the underlying extracts stay on the analyst's machine. The pipeline is
designed so that an unentitled account degrades to a clear error at the fundamentals
step and leaves everything else working.

Credentials are read from `~/.secrets/api.env` first, then from a project-local `.env`,
which takes precedence. Copy `.env.example` to `.env` to configure. Neither file is ever
committed.

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
