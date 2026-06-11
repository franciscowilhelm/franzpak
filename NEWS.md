# franzpak 0.4.0

## Background jobs (`bgjm_*`): new mirai engine

* Background-job execution was migrated from **callr** to **mirai**. Jobs now
  run on a dedicated daemon pool (compute profile `"franzpak"`, started lazily;
  configure with `bgjm_daemons()` or `options(franzpak.bgjm_daemons=)`). On top
  of mirai, franzpak adds a named job registry and per-job filesystem/artifact
  capture (each job gets its own directory; stdout/stderr and any files written
  during the run are collected under `artifacts/`).
* `bgjm_collect()` and `bgjm_result()` **block** until a job resolves (mirroring
  mirai's `m[]`). This is intentional — especially in Quarto / R Markdown
  reports, where returning early would render past unfinished results. Use
  `bgjm_status()` / `bgjm_unresolved()` for non-blocking checks.
* Removed as part of the migration: `bgjm_poll()`, and the `return_strategy` /
  `pass_data_as` arguments.

## New functions

* `bgjm_start_blavaan()` — launch Bayesian SEM jobs (`bsem()`, `bcfa()`,
  `bgrowth()`, `blavaan()`), mirroring `bgjm_start_lavaan()` and forwarding
  sampler arguments (`n.chains`, `burnin`, `sample`, `bcontrol`, `seed`, …)
  through `...`. With `mcmcfile = TRUE` the generated Stan/JAGS files are
  captured as artifacts. (blavaan added to Suggests.)
* `bgjm_result()` — returns just the fitted object, so user code avoids the
  easy-to-miss `bgjm_collect(job)$result` idiom. Blocks and errors like
  `bgjm_collect()`.
* `bgjm_unresolved()` — non-blocking analogue of `mirai::unresolved()` (`TRUE`
  while running, `FALSE` once resolved).

## Parallelism and CPU budgeting

* New vignette section covering the two layers of parallelism that combine on
  the pool: the outer pool (`bgjm_daemons(W)` workers) and a single job using
  multiple cores inside. Rule of thumb: keep `workers × cores-per-job` within
  your physical cores. Covers lavaan bootstrap (`parallel`/`ncpus`/`iseed` pass
  through `...`; prefer `"snow"` over `"multicore"` inside workers), Mplus
  `PROCESSORS`, and blavaan MCMC chains.
* blavaan MCMC chains can be parallelised with `bcontrol = list(cores =
  n.chains)`; verified to work inside a daemon with near-linear speed-up. The
  earlier caution that this was "fragile" has been corrected.

## Documentation

* The background-jobs vignette and the `bgjm_*` help pages were reworked for
  users without a software background: the job id is a handle (not the model),
  `auto_remove = TRUE` deletes the job directory (including Mplus `.out`), the
  default `output_base = tempdir()` is cleared when R exits, `seed` does **not**
  make a parallel bootstrap or MCMC sampling reproducible (use `iseed` /
  blavaan's `seed`), and Mplus inputs must keep the data file beside the `.inp`.
  Adds a plain-language glossary and a blocking-vs-"frozen prompt" note.

## Known limitations / internal

* Mplus's live console output (its TECH8 progress stream) is **not** captured to
  the job's stdout log — the Mplus subprocess writes it to its own stdout. The
  `.out` artifact (and the parsed `readModels()` result) is the authoritative
  record.
* Internal, unexported groundwork for Mplus progress reporting exists
  (`bgjm_progress()`), but it is **not yet functional**, pending an OS-level
  stdout redirect in the Mplus runner. See the code notes in `R/bgjm.R` and
  `R/bgjm_progress.R`.


# franzpak 0.3.0 – 0.3.3

* Added `coef_table_mplus()` for extracting and formatting MplusAutomation model
  coefficients into publication-ready tables, with automatic Bayes detection,
  `type` / `display_type` options, and two-level (multilevel) support.
* Bayes output reports one-tailed p-values with a message (no more p-value
  doubling).
