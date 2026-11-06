# archives/

Code and material retired from the project. Nothing here is sourced by
`source_lib()`, linted in CI or run by the test suite: the library, the reports and
the scripts never read this directory.

It exists because retired work is moved, not deleted. Each file opens with the date it
was archived, where it came from and what replaced it, so a reader who finds an old
function referenced in a commit message can still read it without digging through
history.

| File | Came from | Why it was retired |
|---|---|---|
| `R/vol_models_pre_class.R` | `R/vol_models.R` | EWMA, GARCH and scoring helpers written before the course's Modulo 3 procedures were adopted. Superseded by `R/vol_class.R`. |
| `tests/test-vol-models-pre-class.R` | `tests/testthat/test-vol-models.R` | The tests of the file above. |
| `R/unused_helpers.R` | `R/plots.R`, `R/returns.R` | `save_figure()` and `wealth_index()`: no report, script or function called them. |

To bring something back, move it into `R/` with its tests and let CI lint it.
