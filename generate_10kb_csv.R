# ============================================================================
# generate_10kb_csv.R
#
# A ~10 KB R script that writes a ~10 KB CSV file (generated_data_10kb.csv)
# into R's CURRENT WORKING DIRECTORY, then keeps running (re-reading the CSV
# and recomputing statistics) until 1 minute of total runtime has elapsed. Works the same from the RStudio /
# Workbench console, Source button, Rscript, or a Workbench/RStudio
# background job. Base R only; no packages needed.
#
# The full path of the CSV is printed when the script runs.
# To choose the folder, run setwd("your/folder") before running the script.
# ============================================================================

OUTPUT_FILE    <- "generated_data_10kb.csv"
TARGET_SIZE_KB <- 10
NUM_COLUMNS    <- 8
RANDOM_SEED    <- 42
TARGET_SECONDS <- 60   # total run time in seconds

set.seed(RANDOM_SEED)
start_time <- Sys.time()
elapsed <- function() as.numeric(difftime(Sys.time(), start_time, units = "secs"))

output_path <- file.path(getwd(), OUTPUT_FILE)
cat("Working directory:", getwd(), "\n")
cat("Output CSV will be saved to:", output_path, "\n")

make_rows <- function(n_rows, n_cols) {
  m <- matrix(rnorm(n_rows * n_cols, mean = 100, sd = 25), nrow = n_rows)
  df <- as.data.frame(m)
  names(df) <- paste0("var_", sprintf("%02d", seq_len(n_cols)))
  df
}

# Measure bytes per row on a small sample so the final file lands near 10 KB.
sample_file <- tempfile(fileext = ".csv")
write.csv(make_rows(50, NUM_COLUMNS), sample_file, row.names = FALSE, quote = FALSE)
bytes_per_row <- file.info(sample_file)$size / 50
invisible(file.remove(sample_file))

n_rows <- max(1, ceiling(TARGET_SIZE_KB * 1024 / bytes_per_row))
out_df <- make_rows(n_rows, NUM_COLUMNS)

tryCatch(
  write.csv(out_df, output_path, row.names = FALSE, quote = FALSE),
  error = function(e) stop("Could not write CSV to ", output_path, ": ", conditionMessage(e))
)

if (!file.exists(output_path)) stop("CSV not found after writing: ", output_path)

cat(sprintf("CSV written: %d rows x %d columns, %.2f KB\n",
            n_rows, NUM_COLUMNS, file.info(output_path)$size / 1024))

# Spend the rest of the time on real work: re-read the CSV and recompute
# correlations / summary stats, in short bursts, until TARGET_SECONDS is up.
pass <- 0
while (elapsed() < TARGET_SECONDS) {
  pass <- pass + 1
  d <- read.csv(output_path)
  invisible(list(cor(d), colMeans(d), sapply(d, sd)))
  Sys.sleep(max(0, min(5, TARGET_SECONDS - elapsed())))
  cat(sprintf("[%5.1fs] pass %d done, %.1f s remaining\n",
              elapsed(), pass, max(0, TARGET_SECONDS - elapsed())))
}

cat(sprintf("Done: total runtime %.1f s; CSV is %.2f KB\n",
            elapsed(), file.info(output_path)$size / 1024))
cat("Saved at:", normalizePath(output_path), "\n")

# ---------------------------------------------------------------------------
# Notes (comment-only padding so this script file is ~10 KB on disk)
# ---------------------------------------------------------------------------
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
# This trailing comment block has no effect on execution; it only sizes the file.
#######
