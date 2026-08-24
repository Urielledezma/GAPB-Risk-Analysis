# Contributing

## Getting set up

```bash
git clone https://github.com/Urielledezma/Analisis-de-Riesgo.git
cd Analisis-de-Riesgo
```

```r
install.packages("renv")
renv::restore()
```

That is the whole setup. The committed datasets under `data/processed/` mean you can
render every report immediately, with no credentials and no network. Only refreshing
the data from source needs anything more.

Verify the environment before you start:

```bash
Rscript tests/testthat.R     # analytics library
quarto render reports        # full site into docs/
```

## Branching

One branch per stage, merged by pull request:

```
stage/01-asset-profile
stage/02-price-events
stage/03-returns-gbm
stage/04-variance-models
stage/05-var-es
stage/06-article
```

Fixes and infrastructure use `fix/…` and `chore/…`. Nothing is committed directly to
`main`.

## Commits

[Conventional Commits](https://www.conventionalcommits.org/): `feat:`, `fix:`, `docs:`,
`test:`, `chore:`, `refactor:`, `data:`, `perf:`, `ci:`.

Keep them small and self-contained — one idea per commit. The body is where the
reasoning goes: not what changed, which the diff already says, but why this approach and
not the obvious alternative. A commit that changes a number in a report explains what
moved it.

## Where code goes

| Kind of change | Where it belongs |
|---|---|
| A reusable calculation | `R/`, with a unit test |
| Anything that touches a network | `analysis/`, never a report |
| A ticker, window, confidence level or seed | `config/`, never inline |
| Prose, tables and figures for a stage | `reports/<stage>.qmd` |

Two rules follow from the reproducibility contract and are not negotiable:

**Reports never call an API.** They read `data/processed/` and nothing else. If a report
needs new data, the fetch goes into a numbered script under `analysis/` and the result
is committed as a dataset.

**Nothing derived from a restricted vendor feed is committed.** FactSet extracts live in
`data/raw/`, which is untracked. Cite the vendor for a figure; never version its data.

## Before opening a pull request

- [ ] `Rscript tests/testthat.R` passes.
- [ ] `quarto render reports` completes with no warnings, and `docs/` is committed.
- [ ] New functions in `R/` have tests and a roxygen-style comment block.
- [ ] No credential, token or `.env` file is staged. Check `git diff --cached`.
- [ ] Every figure with more than one series is accompanied by its table.
- [ ] Every reported number has an interpretation in the prose beside it.
- [ ] Sources are cited in `reports/references.bib` and referenced with `@key`.

## Style

R follows the [tidyverse style guide](https://style.tidyverse.org/): `snake_case`, two
spaces, 100 characters. `styler::style_dir("R")` and `lintr::lint_dir("R")` settle
arguments about formatting, and CI enforces the latter.

Two deviations from the lintr defaults are configured in `.lintr` and are worth
knowing about. `SNAKE_CASE` is permitted for module-level constants — `RISK_PALETTE`,
`FACTSET_BASE` — and for nothing else; functions and variables stay `snake_case`. And
`object_usage_linter` is disabled, because the analytics library is a set of sourced
files rather than a package: it cannot resolve cross-file references and would report
every one of them as an undefined global.

Code, comments, documentation and commit messages are in English. Report and article
prose is in Spanish.

## Reviewing

A review checks the reasoning, not only the syntax. The questions worth asking are
whether the estimate uses information that would genuinely have been available at the
time, whether the sign convention holds, whether the number in the table matches the
number in the sentence, and whether a reader who disagrees with the conclusion could
find the evidence to argue with it.
