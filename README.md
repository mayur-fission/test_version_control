# Large File Generator (R)

`generate_large_file.R` generates a synthetic numeric CSV file sized
**~4 GB** (within your requested 3–5 GB range), paced to take **~2 hours**
of wall-clock time when run in RStudio.

## Why it takes 2 hours

Writing 4 GB to disk alone would normally finish in minutes — that's an
I/O-bound operation. To stretch this to ~2 hours in a way that is
meaningful rather than a fake `Sys.sleep(7200)`, the script:

1. Generates the data in chunks (~40 MB each).
2. Runs real computation on every chunk (correlation matrix, a bounded
   matrix multiplication, column means/SDs) — the kind of work a real
   per-batch analysis pipeline would do.
3. Adaptively paces itself: after each chunk it compares actual elapsed
   time to where it *should* be if progress were spread evenly across the
   full 2 hours, and sleeps just enough to stay on schedule. This makes
   total runtime converge on the 2-hour target regardless of whether the
   machine is fast or slow.
4. If all the data finishes writing before the 2-hour mark, it spends any
   remaining time running extra data-quality verification passes (reading
   back samples and re-computing statistics) rather than just idling.

## How to run it

1. Open `generate_large_file.R` in RStudio.
2. (Recommended) First do a quick correctness check: change
   `DRY_RUN <- FALSE` to `DRY_RUN <- TRUE` near the top, then Source the
   script. This runs the exact same code path but scaled down to produce
   a ~20 MB file in ~20 seconds, so you can confirm it works on your
   machine before committing to the full run.
3. Set `DRY_RUN <- FALSE` (the default) and Source the script, or run from
   a terminal:
   ```
   Rscript generate_large_file.R
   ```
4. Progress prints to the console after every chunk (rows written,
   cumulative file size, elapsed time, ETA). When finished, a summary
   block reports the final file size and total runtime.

## Configuration

All adjustable at the top of the script:

| Variable          | Default | Meaning                              |
|--------------------|---------|---------------------------------------|
| `OUTPUT_FILE`      | `large_dataset.csv` | Output file path       |
| `TARGET_SIZE_GB`   | `4.0`   | Target output size in GB (3–5 range)  |
| `TARGET_MINUTES`   | `120`   | Target total runtime in minutes       |
| `NUM_COLUMNS`      | `25`    | Numeric columns per row               |
| `CHUNK_TARGET_MB`  | `40`    | Approx. size per write chunk          |
| `RANDOM_SEED`      | `42`    | For reproducible data                 |

## Tested

This script was run end-to-end (scaled-down `DRY_RUN` mode, several
configurations including multi-chunk runs) to confirm:
- It runs to completion with no errors or warnings.
- The output CSV is well-formed (correct row/column counts, no NAs,
  readable with `read.csv`).
- Pacing correctly stretches runtime to match the configured target.
- File handles are always closed, even on error (`tryCatch`/`finally`).

Only the base R runtime is required — no external packages to install.

## Requirements

- R (tested on R 4.3.3) and RStudio.
- Free disk space: at least ~5 GB for the default settings.
