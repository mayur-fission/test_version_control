#!/usr/bin/env Rscript
# ============================================================================
# generate_large_file.R
#
# Generates a large synthetic CSV data file (target size configurable,
# default ~4 GB) while pacing execution to take a target wall-clock duration
# (default ~120 minutes / 2 hours) when run in RStudio (or via Rscript).
#
# WHY IT TAKES ~2 HOURS:
#   Writing 3-5 GB of data to disk is normally a fast (I/O-bound) operation
#   that would finish in minutes on modern hardware. To deliberately stretch
#   execution to ~2 hours, this script performs real per-chunk computation
#   (matrix algebra + summary statistics on every chunk of generated data)
#   and adaptively paces itself (via calibrated Sys.sleep) so that the
#   TOTAL wall-clock time converges on TARGET_MINUTES, regardless of how
#   fast or slow the machine running it is. The pacing is proportional and
#   recalculated after every chunk, so it self-corrects even if the machine
#   is faster/slower than expected.
#
# HOW TO USE:
#   1. Open this file in RStudio.
#   2. Optionally adjust the CONFIGURATION block below.
#   3. Source it (Ctrl+Shift+S) or run: Rscript generate_large_file.R
#   4. For a quick correctness check before committing to the full run,
#      set DRY_RUN <- TRUE (see below) -- this scales the job down to a
#      ~20 MB file that takes ~20 seconds, exercising the exact same code
#      path as the full run.
#
# OUTPUT:
#   A single CSV file (default: large_dataset.csv) containing numeric data
#   with a header row, written incrementally in chunks. The script prints
#   progress after every chunk (rows written, cumulative size, elapsed
#   time, ETA) so progress is visible in the RStudio console.
# ============================================================================

# No non-base packages required -- keeps this script portable and avoids
# "package not found" failures on a fresh R/RStudio install.

## ---------------------------------------------------------------------
## CONFIGURATION -- edit these values as needed
## ---------------------------------------------------------------------

DRY_RUN <- FALSE          # TRUE = fast smoke test (~20 MB, ~20 sec).
                           # Set TRUE first to verify the script works,
                           # then set FALSE for the real 3-5 GB / 2-hour run.

OUTPUT_FILE     <- "large_dataset.csv"  # Output path (relative to working dir)
TARGET_SIZE_GB  <- 4.0                  # Target output file size, in GB (falls within 3-5 GB)
TARGET_MINUTES  <- 120                  # Target total wall-clock runtime, in minutes
NUM_COLUMNS     <- 25                   # Number of numeric columns per row
CHUNK_TARGET_MB <- 40                   # Approx. size of each write chunk, in MB
RANDOM_SEED     <- 42                   # For reproducibility

if (isTRUE(DRY_RUN)) {
  message(">>> DRY_RUN is TRUE: running a scaled-down smoke test (~20 MB / ~20 sec). <<<")
  message(">>> Set DRY_RUN <- FALSE at the top of the script for the full 3-5 GB / 2-hour run. <<<")
  TARGET_SIZE_GB <- 20 / 1024   # ~20 MB
  TARGET_MINUTES <- 20 / 60     # ~20 seconds
  OUTPUT_FILE <- "large_dataset_DRYRUN.csv"
}

## ---------------------------------------------------------------------
## SETUP
## ---------------------------------------------------------------------

set.seed(RANDOM_SEED)

TARGET_SIZE_BYTES  <- TARGET_SIZE_GB * 1024^3
TARGET_SECONDS     <- TARGET_MINUTES * 60
CHUNK_TARGET_BYTES <- CHUNK_TARGET_MB * 1024^2

col_names <- paste0("var_", sprintf("%02d", seq_len(NUM_COLUMNS)))

# Clean up any pre-existing output file so runs don't append to stale data
if (file.exists(OUTPUT_FILE)) {
  invisible(file.remove(OUTPUT_FILE))
}

script_start_time <- Sys.time()

log_progress <- function(msg) {
  elapsed <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
  cat(sprintf("[%6.1fs elapsed] %s\n", elapsed, msg))
  flush.console()
}

## ---------------------------------------------------------------------
## HELPER: generate one chunk of synthetic numeric data as a data.frame
## ---------------------------------------------------------------------

make_chunk <- function(n_rows, n_cols) {
  m <- matrix(rnorm(n_rows * n_cols, mean = 100, sd = 25), nrow = n_rows, ncol = n_cols)
  df <- as.data.frame(m)
  names(df) <- col_names
  df
}

## ---------------------------------------------------------------------
## HELPER: do some real, non-trivial computation on a chunk.
## This is deliberate CPU work (not just padding) -- e.g. what a real
## analysis pipeline might do per batch: correlations, matrix products,
## summary statistics. Its cost also gets folded into the timing model.
## ---------------------------------------------------------------------

compute_on_chunk <- function(df) {
  m <- as.matrix(df)
  # Correlation matrix across columns
  cm <- suppressWarnings(cor(m))
  # A modest matrix multiplication (m %*% t(m) would be huge; keep it
  # bounded to a fixed-size operation so cost is predictable per call)
  sub <- m[seq_len(min(300, nrow(m))), , drop = FALSE]
  mp <- sub %*% t(sub)
  # Summary statistics
  col_means <- colMeans(m)
  col_sds   <- apply(m, 2, sd)
  invisible(list(cor = cm, prod_trace = sum(diag(mp)), means = col_means, sds = col_sds))
}

## ---------------------------------------------------------------------
## STEP 1: Calibrate -- measure bytes/row and per-chunk compute time using
## a small sample chunk. This lets us plan how many rows/chunks are needed
## to hit the target size, and how to pace compute+sleep to hit the target
## duration, without hardcoding machine-specific assumptions.
## ---------------------------------------------------------------------

log_progress("Calibrating: generating a sample chunk to measure row size and compute cost...")

CALIBRATION_ROWS <- 2000
calib_chunk <- make_chunk(CALIBRATION_ROWS, NUM_COLUMNS)

calib_file <- tempfile(fileext = ".csv")
write.table(calib_chunk, file = calib_file, sep = ",", row.names = FALSE,
            col.names = TRUE, quote = FALSE)
bytes_per_row <- file.info(calib_file)$size / CALIBRATION_ROWS
invisible(file.remove(calib_file))

compute_start <- Sys.time()
invisible(compute_on_chunk(calib_chunk))
compute_seconds_per_calib_chunk <- as.numeric(difftime(Sys.time(), compute_start, units = "secs"))
# Guard against a measurement of (near) zero on very fast machines
compute_seconds_per_calib_chunk <- max(compute_seconds_per_calib_chunk, 0.001)

rows_per_chunk <- max(1, round(CHUNK_TARGET_BYTES / bytes_per_row))
total_rows_needed <- ceiling(TARGET_SIZE_BYTES / bytes_per_row)
num_chunks <- max(1, ceiling(total_rows_needed / rows_per_chunk))

# Estimated compute-only time for the whole job, extrapolated from the
# calibration chunk (compute cost scales roughly linearly with rows chosen
# here since compute_on_chunk's work is bounded/near-linear per row batch)
est_compute_seconds_per_chunk <- compute_seconds_per_calib_chunk * (rows_per_chunk / CALIBRATION_ROWS)
est_total_compute_seconds <- est_compute_seconds_per_chunk * num_chunks

log_progress(sprintf(
  "Calibration done: ~%.0f bytes/row, %d rows/chunk, %d chunks planned, ~%.1fs estimated total compute time.",
  bytes_per_row, rows_per_chunk, num_chunks, est_total_compute_seconds
))
log_progress(sprintf(
  "Target: %.2f GB output, %.1f minutes total runtime.",
  TARGET_SIZE_GB, TARGET_MINUTES
))

## ---------------------------------------------------------------------
## STEP 2: Main generation loop.
## Writes chunk-by-chunk via an open file connection (fast, avoids
## reopening the file on every iteration). After each chunk, compares
## actual elapsed time to the proportional target-so-far, and sleeps just
## enough to stay on pace -- this makes total runtime converge on
## TARGET_SECONDS regardless of machine speed, while still doing genuine
## computation on every chunk.
## ---------------------------------------------------------------------

con <- file(OUTPUT_FILE, open = "w")

# Write header once
writeLines(paste(col_names, collapse = ","), con)

total_rows_written <- 0
chunk_index <- 0

result <- tryCatch({
  repeat {
    chunk_index <- chunk_index + 1
    remaining_rows <- total_rows_needed - total_rows_written
    if (remaining_rows <= 0) break

    this_chunk_rows <- min(rows_per_chunk, remaining_rows)

    chunk_df <- make_chunk(this_chunk_rows, NUM_COLUMNS)

    # Real computation on this chunk (contributes to elapsed time)
    invisible(compute_on_chunk(chunk_df))

    # Write chunk to the open connection (no header, no row names)
    write.table(chunk_df, file = con, sep = ",", row.names = FALSE,
                col.names = FALSE, quote = FALSE)
    flush(con)

    total_rows_written <- total_rows_written + this_chunk_rows

    # --- Adaptive pacing ---
    elapsed_seconds <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
    progress_frac <- min(1, total_rows_written / total_rows_needed)
    target_elapsed_at_this_point <- progress_frac * TARGET_SECONDS
    pacing_gap <- target_elapsed_at_this_point - elapsed_seconds

    if (pacing_gap > 0) {
      # Sleep in short bursts so the process stays responsive / interruptible
      # and so progress is still logged reasonably often.
      sleep_chunk <- min(pacing_gap, 30)
      Sys.sleep(sleep_chunk)
    }

    current_size_bytes <- file.info(OUTPUT_FILE)$size
    elapsed_seconds <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
    remaining_est_seconds <- max(0, TARGET_SECONDS - elapsed_seconds)

    log_progress(sprintf(
      "Chunk %d/%d | rows: %d/%d (%.1f%%) | size: %.2f/%.2f GB | ETA: %.1f min",
      chunk_index, num_chunks, total_rows_written, total_rows_needed,
      100 * progress_frac,
      current_size_bytes / 1024^3, TARGET_SIZE_GB,
      remaining_est_seconds / 60
    ))
  }
  TRUE
}, error = function(e) {
  message("ERROR during generation: ", conditionMessage(e))
  FALSE
}, finally = {
  # Always close the connection, even on error, to avoid a dangling handle
  # and to ensure buffered data is flushed to disk.
  if (isOpen(con)) close(con)
})

if (!isTRUE(result)) {
  stop("large_dataset generation failed -- see error message above.")
}

## ---------------------------------------------------------------------
## STEP 3: If we finished writing all rows but haven't yet reached the
## target duration (e.g. compute+IO ran faster than planned), spend the
## remaining time on additional genuine computation passes so total
## runtime still converges on TARGET_MINUTES. This does NOT grow the file
## further -- file size is already at target from the loop above.
## ---------------------------------------------------------------------

elapsed_seconds <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
if (elapsed_seconds < TARGET_SECONDS) {
  log_progress(sprintf(
    "File size target reached with %.1f minutes remaining on the time budget -- running additional verification/compute passes to use the remaining time productively.",
    (TARGET_SECONDS - elapsed_seconds) / 60
  ))
  extra_pass <- 0
  repeat {
    elapsed_seconds <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))
    remaining <- TARGET_SECONDS - elapsed_seconds
    if (remaining <= 0) break
    extra_pass <- extra_pass + 1
    # Re-read a sample of the file and compute stats on it -- genuine,
    # useful work (a basic data-quality check) rather than a bare sleep.
    sample_rows <- min(50000, total_rows_written)
    sample_df <- tryCatch(
      read.csv(OUTPUT_FILE, nrows = sample_rows, header = TRUE),
      error = function(e) NULL
    )
    if (!is.null(sample_df) && nrow(sample_df) > 0) {
      invisible(compute_on_chunk(sample_df))
    }
    sleep_chunk <- min(remaining, 30)
    Sys.sleep(sleep_chunk)
    if (extra_pass %% 5 == 0) {
      log_progress(sprintf("Extra verification pass %d | %.1f min remaining", extra_pass, remaining / 60))
    }
  }
}

## ---------------------------------------------------------------------
## STEP 4: Final report and sanity checks
## ---------------------------------------------------------------------

final_size_bytes <- file.info(OUTPUT_FILE)$size
total_elapsed_seconds <- as.numeric(difftime(Sys.time(), script_start_time, units = "secs"))

cat("\n============================== SUMMARY ==============================\n")
cat(sprintf("Output file:        %s\n", normalizePath(OUTPUT_FILE)))
cat(sprintf("Final file size:    %.3f GB (%.0f bytes)\n", final_size_bytes / 1024^3, final_size_bytes))
cat(sprintf("Total rows written: %d\n", total_rows_written))
cat(sprintf("Total runtime:      %.2f minutes (%.1f seconds)\n", total_elapsed_seconds / 60, total_elapsed_seconds))
cat("=======================================================================\n")

if (!isTRUE(DRY_RUN)) {
  if (final_size_bytes < 3 * 1024^3 || final_size_bytes > 5 * 1024^3) {
    warning(sprintf(
      "Final file size (%.2f GB) fell outside the requested 3-5 GB range. Adjust TARGET_SIZE_GB and re-run if needed.",
      final_size_bytes / 1024^3
    ))
  } else {
    cat("File size is within the target 3-5 GB range.\n")
  }
}

cat("Done.\n")
