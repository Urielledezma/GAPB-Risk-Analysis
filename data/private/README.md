# data/private/

Datasets derived from **FactSet**. Nothing here is tracked, and nothing here should be.

FactSet's terms restrict redistribution of its content. That restriction attaches to the
data itself, not to who holds a licence, so this directory stays out of version control
even though every member of the team has a key. Reports cite FactSet as a source in the
way any research note cites a data vendor; the series themselves stay on the machine
that pulled them.

If this directory is empty, the pipeline has not been run against a FactSet key on this
machine. That is a working state, not a broken one: `read_dataset()` falls back to the
public snapshot in `data/processed/` and every report still renders. The reports say
which source they used.

To populate it:

```r
source("analysis/00_probe_factset.R")   # confirm entitlement first
source("analysis/01_ingest_prices.R")   # add --universe for stage 05
source("analysis/04_build_datasets.R")
```
