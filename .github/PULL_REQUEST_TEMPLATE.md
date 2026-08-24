## What this changes

<!-- One paragraph. What the reader gets that they did not have before. -->

## Why this approach

<!-- The reasoning the diff cannot show: what alternative was considered and
     why this one won. If a reported figure moved, say what moved it. -->

## Checks

- [ ] `Rscript tests/testthat.R` passes
- [ ] `quarto render reports` completes without warnings and `docs/` is committed
- [ ] New functions in `R/` have unit tests
- [ ] No credential, token or `.env` file is staged
- [ ] Multi-series figures are accompanied by their table
- [ ] Sources cited in `reports/references.bib`

## Notes for the reviewer

<!-- Anything worth a second opinion: an assumption you are unsure of, a
     parameter chosen by judgement, a result that surprised you. -->
