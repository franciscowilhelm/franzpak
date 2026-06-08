# `coef_table_mplus()` unnecessary outcome columns

This synthetic example reproduces outcome columns that contain only a
mean/intercept and no regression coefficients.

The Mplus model is intentionally small:

```mplus
%WITHIN%
  Y ON X W;

%BETWEEN%
  X W Y;
```

`Y` is the only regression outcome. `X` and `W` are predictors with estimated
between-level means and variances.

The trigger is a `label_replace` entry that changes the structural token
`"Means"`:

```r
label_replace = c(
  "X" = "Predictor X",
  "W" = "Predictor W",
  "Y" = "Outcome Y",
  "Means" = "Mean / intercept"
)
```

`coef_table_mplus()` applies `label_replace` before it identifies expectation
rows using the original tokens `"Means"`, `"Intercepts"`, and `"Thresholds"`.
Consequently, `"Mean / intercept"` is treated as an ordinary predictor rather
than an expectation row. The means of `X` and `W` then make those variables
qualify as outcome spanners.

Run:

```r
source("reproduce.R")
```

The returned table has outcome spanners for `Outcome Y`, `Predictor X`, and
`Predictor W`. The `Predictor X` and `Predictor W` columns each contain only a
between-level mean/intercept. They have no regression-coefficient cells and are
therefore unnecessary in a regression coefficient table.

To regenerate the synthetic data and Mplus output before reproducing the
table:

```sh
Rscript reproduce.R --run-mplus
```

Files:

- `unnecessary_columns.inp`: minimal two-level Mplus model.
- `unnecessary_columns.out`: Mplus 8.10 output generated from the input/data.
  (The synthetic `unnecessary_columns.dat` is not committed; `reproduce.R`
  regenerates it deterministically via `set.seed()` when run with `--run-mplus`.)
- `reproduce.R`: data generation, optional Mplus execution, and R reproduction.
- `coef_table_output.csv`: unformatted data returned by
  `franzpak::coef_table_mplus()`.
