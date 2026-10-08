# ============================================================================
# run_until_stopped.R
#
# Runs forever (while TRUE + Sys.sleep) until you stop it manually.
#
# HOW TO STOP:
#   - RStudio / Workbench console or Source button: click the red STOP button
#     in the console toolbar, or press Esc.
#   - Background job: click "Stop" in the Background Jobs pane.
#   - Terminal / Rscript: press Ctrl+C.
#
# Base R only; no packages needed. Uses almost no CPU while sleeping.
# ============================================================================

SLEEP_SECONDS <- 5     # pause between iterations (change as you like)

start_time <- Sys.time()
iteration  <- 0

cat("Started at", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Running until stopped manually (Stop button / Esc / Ctrl+C)...\n")

tryCatch({
  while (TRUE) {
    iteration <- iteration + 1
    elapsed   <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    cat(sprintf("[%s] iteration %d | running for %.0f s\n",
                format(Sys.time(), "%H:%M:%S"), iteration, elapsed))
    flush.console()

    Sys.sleep(SLEEP_SECONDS)
  }
}, interrupt = function(e) {
  cat(sprintf("\nStopped manually after %d iterations (%.0f s).\n",
              iteration,
              as.numeric(difftime(Sys.time(), start_time, units = "secs"))))
})
