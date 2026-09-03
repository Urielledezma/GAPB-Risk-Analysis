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
variance models, and portfolio VaR / Expected Shortfall. Each stage renders as a page
of the research site; together they form the basis of a research article.

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

# 2. Refresh public prices and derived datasets
source("analysis/01_ingest_prices.R")     # add --universe for stage 05
source("analysis/04_build_datasets.R")

# Optional: normalize manual FactSet exports after placing them in
# data/raw/factset/manual/
source("analysis/03_ingest_fundamentals.R")

# Without a key, skip step 2 entirely — the committed snapshot renders as is.
```

```bash
# 3. Render the reports into docs/
quarto render reports

# If an academic portal requires one portable HTML file, render that report
# explicitly. This profile is for submission copies, not the GitHub Pages site.
quarto render reports/01-asset-profile.qmd --profile submission

# 4. Verify the analytics library
Rscript tests/testthat.R
```

The committed public snapshot renders without credentials. See
[Data sources](#data-sources) below for the manual FactSet override.

The regular site build is intentionally multi-file: its HTML pages depend on
`docs/site_libs/` and on external PNG files such as
`docs/01-asset-profile_files/figure-html/fig-margins-1.png`. Copy or publish the
complete `docs/` directory. A loose HTML file will lose styling, scripts and
figures. Use the `submission` profile with an explicit report path only when the
academic delivery channel requires one self-contained HTML document; the output is
written under `outputs/submission/`.

---

## Data sourcing and reproducibility

**FactSet is the preferred licensed source, but ITESO has no API entitlement.** Prices
therefore use the public Yahoo Finance / BMV path and fundamentals use cited GAP/SEC
filings unless a teammate supplies a manual FactSet export. The source ladder lives in
`R/data_sources.R`; both paths produce the same report schemas.

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
teammate with a normalized manual export sees FactSet numbers and a reader without one
still sees a rendered report.

Random draws (Monte Carlo VaR, GBM path simulation) are seeded from `config/params.yml`.

---

## Data sources

| Source | Role | Used for | Access | Committed? |
|---|---|---|---|---|
| **FactSet workstation** | Manual licensed override | Fundamentals and market data exported by a licensed teammate | Manual export; no ITESO API entitlement | **No** |
| GAP / SEC filings | Primary | Annual fundamentals, margins, leverage ratios | Public | Yes — curated snapshot |
| Yahoo Finance / BMV | Primary public path | Daily OHLCV, full quotation history | Public, via `quantmod` | Yes — `data/processed/` |
| World Bank | Primary | Mexican annual real GDP growth | Public, no token | Yes |
| Banxico SIE | Optional | Reference rate, FX | Free token | Yes — derived series only |
| INEGI | Optional | Quarterly GDP | Free token | Yes — derived series only |

**FactSet content is cited, never redistributed.** Vendor extracts land in
`data/raw/factset/` and vendor-derived datasets in `data/private/`, both excluded from
version control. Figures derived from them appear in the reports with the source cited,
as any research note cites a data vendor.

The historical API probe remains under `analysis/00_probe_factset.R` for diagnostic
reference, but it is not part of the production ingest path under the current licence.

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
- [x] Etapa 01 — issuer profile and risk taxonomy
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
