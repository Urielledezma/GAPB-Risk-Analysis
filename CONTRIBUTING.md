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

## Working on a stage

Everything below can be done from RStudio. No terminal is required.

### 1. Start from a clean branch

Pull first, using the blue arrow in the **Git** pane. Then create the stage branch with
the purple branch icon → **New Branch**, following the naming in *Branching* above:

```
stage/02-price-events
```

Nothing is committed directly to `main`.

### 2. Decide whether the data needs refreshing

Most stages do not need it. The committed snapshot in `data/processed/` renders every
report as is, and `config/params.yml` fixes a hard estimation cut-off, so refreshing
prices does **not** move the fitted parameters in stages 03 to 05.

Check what the snapshot currently covers before assuming it is stale.

If it genuinely needs extending, do it **on its own branch and in its own commit**,
before touching any report:

1. Branch `data/refresh-snapshot-<yyyy-mm>`.
2. Run the ingest scripts from the R console, in the order given in the README.
3. Inspect the diff on `data/processed/*.csv`. New dates should appear; old rows should
   not disappear. A shrinking file means an incomplete download, not a successful one.
4. `testthat::test_dir("tests/testthat")` — still 197.
5. Commit as `data: refresh public snapshot through <yyyy-mm>`.
6. **Re-render the whole site**, not only the stage being worked on. Build pane →
   **Render Website**. A site with one page on new data and five on old data is worse
   than one that was not refreshed at all.
7. Commit `docs/` separately, then open the pull request.

Only once that is merged does stage work begin, on data that is already settled. Keeping
the two apart means that if a figure looks wrong, it is immediately clear whether the
data moved or the analysis did.

### 3. Write the stage

Open `reports/<stage>.qmd` and work there. Use the **Render** button as often as needed
while iterating; it writes into `docs/` but nothing is committed until you say so.

Two things follow from *Where code goes* and are easy to forget:

- A reusable calculation belongs in `R/` with a unit test, never inside the `.qmd`.
- Every source cited goes into `reports/references.bib` and is referenced with `@key`.

Any chunk producing a figure or a table must be labelled with a `fig-` or `tbl-` prefix.
Without it Quarto falls back to plain knitr markup, the figure loses its number and its
caption, and the image is emitted without `img-fluid` — which makes it overflow the body
column and collide with the sidebar table of contents.

### 4. Commit as you go

Use the **Git** pane: tick the files, write the message, **Commit**. Commit each finished
block rather than the whole day at once, so a single piece can be reverted without losing
the rest.

Stage the `.qmd` and `references.bib` only. Leave `docs/` for the end.

### 5. Close the stage

One person runs Build → **Render Website** and commits the whole of `docs/` in a single
final commit. Two people committing generated HTML from parallel branches produces a
conflict in thousands of machine-written lines, which is the one merge conflict worth
going out of your way to avoid.

Then open the pull request and work through the checklist below.

### 6. Produce a submission copy, if one is required

The site under `docs/` is deliberately multi-file: its pages depend on `docs/site_libs/`
and on external figure directories. It is published through GitHub Pages and is **not**
what you upload to an academic portal.

For a single portable HTML file, render the stage explicitly with the `submission`
profile, from the R console:

```r
quarto::quarto_render("reports/02-price-history-events.qmd", profile = "submission")
```

The output lands in `outputs/submission/`, which is gitignored and never committed. It is
a disposable artefact: regenerate it on the day you need it. Verify it by copying that
one file somewhere else and opening it on its own — if the figures render, it is
self-contained.

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
