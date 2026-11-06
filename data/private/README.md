# data/private/

Datasets derived from **FactSet**. Nothing here is tracked, and nothing here should be.

FactSet's terms restrict redistribution of its content. That restriction attaches to the
data itself, not to who holds a licence, so this directory stays out of version control
even when the data came from a licensed workstation. Reports cite FactSet as a source in the
way any research note cites a data vendor; the series themselves stay on the machine
that pulled them.

If this directory is empty, no FactSet export has been processed on this machine. That
is a working state, not a broken one: `read_dataset()` falls back to the public snapshot
in `data/processed/` and every report still renders. The reports say which source they
used.

ITESO's licence carries no API entitlement, so the directory is populated from a manual
export: a licensed teammate saves the files into `data/raw/factset/manual/`, then

```r
source("analysis/03_ingest_fundamentals.R")   # validates the exports into private/
source("analysis/04_build_datasets.R")
```
