required_packages <- c("franzpak", "MplusAutomation")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_dir <- if (length(script_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", script_arg)))
} else {
  normalizePath(getwd())
}
setwd(script_dir)

generate_data <- function(path = "unnecessary_columns.dat") {
  set.seed(20260608)

  n_clusters <- 60
  observations_per_cluster <- 5
  cluster <- rep(seq_len(n_clusters), each = observations_per_cluster)

  cluster_x <- rep(rnorm(n_clusters, sd = 0.7), each = observations_per_cluster)
  cluster_w <- rep(rnorm(n_clusters, sd = 0.6), each = observations_per_cluster)
  cluster_y <- rep(rnorm(n_clusters, sd = 0.8), each = observations_per_cluster)

  x <- cluster_x + rnorm(length(cluster))
  w <- cluster_w + rnorm(length(cluster))
  y <- 0.5 * x - 0.3 * w + cluster_y + rnorm(length(cluster), sd = 0.8)

  data <- data.frame(CLUSTER = cluster, X = x, W = w, Y = y)
  write.table(
    data,
    file = path,
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    na = "."
  )

  invisible(data)
}

run_mplus <- "--run-mplus" %in% commandArgs(trailingOnly = TRUE)

if (run_mplus) {
  generate_data()
  MplusAutomation::runModels(
    target = "unnecessary_columns.inp",
    recursive = FALSE,
    replaceOutfile = "always",
    showOutput = TRUE
  )
}

if (!file.exists("unnecessary_columns.out")) {
  stop(
    "`unnecessary_columns.out` is missing. Run ",
    "`Rscript reproduce.R --run-mplus` first.",
    call. = FALSE
  )
}

model <- MplusAutomation::readModels("unnecessary_columns.out")

label_replace <- c(
  "X" = "Predictor X",
  "W" = "Predictor W",
  "Y" = "Outcome Y",
  "Means" = "Mean / intercept"
)

table_data <- franzpak::coef_table_mplus(
  model,
  label_replace = label_replace,
  na_replace = NULL,
  return_data = TRUE
)

if (is.list(table_data) && !inherits(table_data, "data.frame")) {
  table_data <- table_data$fixed
}

write.csv(table_data, "coef_table_output.csv", row.names = FALSE, na = "")

outcome_columns <- unique(sub(
  "^[^_]+_col__",
  "",
  grep("_col__", names(table_data), value = TRUE)
))

nonempty_rows <- vapply(outcome_columns, function(outcome) {
  columns <- grep(
    paste0("__", outcome, "$"),
    names(table_data),
    value = TRUE
  )
  sum(rowSums(!is.na(table_data[, columns, drop = FALSE])) > 0)
}, integer(1))

cat("Outcome spanners produced by coef_table_mplus():\n")
print(outcome_columns)

cat("\nNumber of nonempty predictor rows under each outcome spanner:\n")
print(nonempty_rows)

cat(
  "\nExpected reproduction: Outcome Y is the only regression outcome, while ",
  "Predictor X and Predictor W each appear as outcome spanners with one ",
  "nonempty row containing only their between-level mean/intercept.\n",
  sep = ""
)

print(table_data)
