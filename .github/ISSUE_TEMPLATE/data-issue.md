---
name: Data issue
about: Something is wrong, missing or suspicious in a dataset
title: "[Data] "
labels: data
---

## What looks wrong

<!-- The observation, not the interpretation. A ticker, a date range, a value
     that cannot be right. -->

## How it was found

<!-- The check that surfaced it: a validation failure, a chart that looked
     wrong, a figure that disagreed with a published source. -->

## Affected

- Dataset:
- Ticker(s) / series:
- Date range:
- Reports that depend on it:

## Impact

<!-- Does this change a reported number? If so, which one and by how much. -->

## Fix

- [ ] Root cause identified — a source problem, an ingest bug, or a genuine market event
- [ ] Validation extended so the same problem fails at ingest next time
- [ ] Dataset regenerated and committed
- [ ] Affected reports re-rendered
