# franzpak Vignettes

This directory contains vignettes documenting the use of franzpak functions.

## Available Vignettes

### Background Jobs (`background-jobs.qmd`)

Comprehensive guide to using the background job management system (`bgjm_*` functions) for running long-running statistical models in isolated R sessions.

**Topics covered:**
- Starting and monitoring background jobs
- Working with lavaan models (CFA, SEM, mediation)
- Running tidyLPA latent profile analyses
- Parallel model comparison
- Return strategies (inline vs. RDS)
- Error handling and troubleshooting
- Best practices

### Mplus Coefficient Tools (`mplus-coef-tools.qmd`)

Guide to formatting Mplus coefficients with `coef_wrapper()` and
`coef_table_mplus()`, using the bundled Mplus User's Guide example outputs.

**Topics covered:**
- Tidy coefficient extraction and DV/IV label splitting
- Publication-ready predictor x outcome tables (via `gt`)
- Standardized (`STDYX`) coefficients and confidence/credibility intervals
- Automatic Bayesian vs. frequentist detection and display
- Two-level models: Within/Between sections, with random-slope means/intercepts
  reported as between-person means of within-person effects

**To build the vignettes:**

The Quarto sources live in `inst/quarto/`. Using Quarto:
```bash
quarto render inst/quarto/background-jobs.qmd
quarto render inst/quarto/mplus-coef-tools.qmd
```

Or from R:
```r
quarto::quarto_render("inst/quarto/mplus-coef-tools.qmd")
```

The rendered HTML lives in this `vignettes/` directory.
