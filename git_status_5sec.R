# ---- Settings ----
repo <- "/home/posit/test_version_control"
interval  <- 5                                    # seconds between runs

# ---- Checks before starting ----
stopifnot(dir.exists(file.path(repo, ".git")))
if (Sys.which("git") == "") stop("git not found on PATH")


cat("Polling git status every", interval, "seconds. Press Stop (or Esc) to end.\n\n")

# ---- Polling loop ----
repeat {
  loop_start <- Sys.time()
  
  exit_code <- NA_integer_
  t <- system.time({
    exit_code <- suppressWarnings(
      system2("git", c("-C", shQuote(repo), "status", "--porcelain"),
              stdout = FALSE, stderr = FALSE)
    )
  })
  
  stamp <- format(Sys.time(), "%F %T")
  
  # Print to Console
  cat(sprintf("[%s] git status took %.3f sec (exit code: %s)\n",
              stamp, t[["elapsed"]], exit_code))
  flush.console()
  
  
  # Sleep only for the time left in the interval
  used <- as.numeric(difftime(Sys.time(), loop_start, units = "secs"))
  Sys.sleep(max(0, interval - used))
}