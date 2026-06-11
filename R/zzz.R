# Silence R CMD check's "no visible binding for global variable" notes for
# names that are not free variables:
#   * job_dir/art_dir/tmp_dir/stdout_log/stderr_log are injected into the
#     `mirai::mirai()` expression in .bgjm_submit() via its `.args` list and
#     bound on the daemon at evaluation time.
#   * the remaining names are dplyr/tidyselect NSE column references used in the
#     Mplus coefficient-table helpers.
utils::globalVariables(c(
  "job_dir", "art_dir", "tmp_dir", "stdout_log", "stderr_log",
  "est_col", "ll_col", "ul_col", "se_col",
  ".slope", ".ord", ".sig", "Parameter"
))

.onUnload <- function(libpath) {
  # Shut down the background-job daemon pool if it was started.
  if (isTRUE(.bgjm_env$daemons_set) &&
      requireNamespace("mirai", quietly = TRUE)) {
    try(mirai::daemons(0, .compute = .bgjm_compute), silent = TRUE)
  }
}
