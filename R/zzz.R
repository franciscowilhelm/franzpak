.onUnload <- function(libpath) {
  # Shut down the background-job daemon pool if it was started.
  if (isTRUE(.bgjm_env$daemons_set) &&
      requireNamespace("mirai", quietly = TRUE)) {
    try(mirai::daemons(0, .compute = .bgjm_compute), silent = TRUE)
  }
}
