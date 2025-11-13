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

**To build the vignette:**

Using Quarto:
```bash
quarto render vignettes/background-jobs.qmd
```

Or from R:
```r
quarto::quarto_render("vignettes/background-jobs.qmd")
```

The HTML output will be created in the vignettes directory.
