# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

franzpak is an R package containing utility functions for social science questionnaire data analysis. The package focuses on multilevel/diary data analysis, psychometric scale scoring, and interfacing with Mplus through MplusAutomation.

## Development Commands

### Building and Testing
```r
# Build package documentation
devtools::document()

# Install package locally
devtools::install()

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-cortable_multilevel.R")

# Check package (CRAN-style, though not targeting CRAN)
devtools::check()
```

### Common Workflows
```r
# Load package for interactive development
devtools::load_all()

# Rebuild README from README.Rmd
rmarkdown::render("README.Rmd")
```

## Architecture

### Core Functional Areas

**1. Multilevel Data Analysis** (`R/cortable_multilevel.R`, `R/pivot_longer_multilevel.R`)
- `cortable_multilevel()`: Creates APA-formatted correlation tables for two-level data using `psych::statsBy()`
  - Upper triangle: between-person correlations
  - Lower triangle: within-person correlations
  - Returns formatted tables by default; can return list with numeric tables via `return_list = TRUE`
  - Automatically detects Level-2 variables (ICC ≈ 1) and handles them appropriately
  - Uses `papaja` for APA formatting
- `pivot_longer_multilevel()`: Converts wide two-level repeated measures data to long format
  - Designed for diary/repeated measures where sequence matters
  - Creates `id` (level-2 unit) and `time` (level-1 unit) columns

**2. Psychometric Scale Scoring** (`R/scoreItemsMulti.R`, `R/scoreItemsMultiFast.R`)
- `scoreItemsMulti()`: Wrapper for `psych::scoreItems()` to score multiple scales at once
  - Automatically detects reverse-coded items via PCA loadings
  - Supports manual key specification via `manual_keys` parameter
  - Excludes participants with >1/3 missing items when `exclude = TRUE`
  - Always uses `impute = "none"` (overrides scoreItems default)
  - Returns list with `$scores`, `$alpha`, and `$negative_index`

**3. Background Job Management** (`R/bgjm.R`, `R/zzz.R`)
- Runs long-running model fits on the **mirai** engine (a dedicated daemon pool,
  compute profile `"franzpak"`, started lazily; configure via `bgjm_daemons()`
  or `options(franzpak.bgjm_daemons=)`). Migrated off `callr` (see the internal
  design vignette `inst/quarto/background-jobs-internal.qmd`).
- Adds two things mirai does not: an in-memory **named registry**
  (`.bgjm_env$jobs`) and **per-job filesystem/artifact capture** (the runner
  `.bgjm_run_in_daemon()` sets the working dir, captures stdout/stderr to log
  files, and relocates files written during the run into `artifacts/`).
- Key functions:
  - `bgjm_start_lavaan()`, `bgjm_start_blavaan()`, `bgjm_start_tidylpa()`, `bgjm_start_mplus()`: launch jobs
  - `bgjm_list()`, `bgjm_status()`: monitor (non-blocking)
  - `bgjm_progress()`: **internal, not yet functional** (unexported groundwork
    in `R/bgjm_progress.R`). The TECH8-stream parsers are correct and tested
    against fixtures, but a live probe confirmed Mplus's console stream is never
    captured into the job's stdout log — the subprocess writes it to the
    daemon's OS-level fd 1, which `sink()` does not catch. Future work: redirect
    the Mplus subprocess stdout at the OS level (`system2(..., stdout =)`) in
    the runner instead of `runModels(showOutput = TRUE)` + `sink`; see the
    future-work NOTE in `R/bgjm.R`. (`.out` is also no help: it is an input-echo
    stub until the run ends, so its existence is not a completion signal either.)
  - `bgjm_collect()`: **blocks** until resolved, retrieves result (`auto_remove =`)
  - `bgjm_kill()` (→ `mirai::stop_mirai()`), `bgjm_remove()`, `bgjm_daemons()`
  - `bgjm_read_stdout()`, `bgjm_read_stderr()`: per-job log files
- Shipped functions are detached to `baseenv()` before submission so they run on
  a clean daemon without franzpak loaded. Error/interrupt classification uses
  `mirai::is_error_value()` (catches interrupts/timeouts, unlike `is_mirai_error()`).
- No `bgjm_poll()` / `return_strategy` / `pass_data_as` (removed in the mirai migration).
- `bgjm_start_mplus()` stages `.inp`/`.dat` into the job dir, runs `runModels()`
  with `logFile=`/`showOutput=TRUE`/`quiet=FALSE`, captures `.out` + `mplus_run.log`,
  returns parsed `readModels()`. Tests build fixtures via `mplusModeler(run = 0)`.
- **tidyLPA API**: Uses tidyLPA >= 1.0.0 API with `n_profiles` (not `K`), `models` (not `model`), and `package` (not `engine`)

**4. Mplus Integration** (`R/mplus_coef_tools.R`)
- `coef_wrapper()`: Extracts and formats coefficients from MplusAutomation models
- Requires MplusAutomation package
- function functionalities are tested using Mplus example inputs and outputs, in exampleData/ or inst/extdata

### Package Dependencies

**Heavy dependencies:**
- `psych`: Scale scoring (`scoreItems()`, `statsBy()`)
- `papaja`: APA formatting for correlation tables
- `dplyr`, `tidyr`, `purrr`: Data manipulation (uses tidyverse notation throughout)
- `callr`: Background job execution

**Optional dependencies (in Suggests):**
- `MplusAutomation`: For Mplus model coefficient extraction
- `lavaan`: For lavaan background job wrappers
- `tidyLPA` (>= 1.0.0): For tidyLPA background job wrappers
- `mclust`: For tidyLPA mclust backend

### Code Conventions

- Uses tidyverse notation and quasi-quotation (NSE) throughout
- Not CRAN-compliant by design (author decision)
- Roxygen2 for documentation (`@export` tags mark public API)
- Internal helpers marked with `@noRd` (not documented)
- 2-space indentation (per `.Rproj` settings)

## Testing

- Test framework: testthat (edition 3)
- Test data: `mc_twolevel`, `mc_items` (in `R/data.R`)
- Tests emphasize backward compatibility (e.g., `cortable_multilevel` default return behavior)

## Important Notes

- README is generated from `README.Rmd` - always edit the `.Rmd`, never the `.md` directly
- The package includes example data accessible via `franzpak::mc_twolevel` and `franzpak::mc_items`
- When modifying functions with complex return structures (like `cortable_multilevel`), ensure backward compatibility by checking existing tests
- Background job functions create temporary directories - cleanup is manual unless `auto_remove = TRUE`
- Vignettes are in Quarto (`.qmd`) format in `vignettes/` directory:
  - `background-jobs.qmd`: Comprehensive workflow guide for background job management
