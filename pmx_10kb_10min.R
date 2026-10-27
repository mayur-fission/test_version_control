# ==============================================================================
# Project   : Population PK simulation - small NONMEM-style dataset (~10 KB)
# Script    : pmx_10kb_10min.R
# Purpose   : (1) Simulate a small population PK dataset in NONMEM format
#                 (about 10 KB) from a 1-compartment oral model with BSV,
#                 covariates and residual error.
#             (2) Keep working for RUN_MINUTES (default 10) by simulating
#                 virtual-trial replicates and summarising steady-state
#                 exposure (AUCtau, Cmax, Ctrough) and probability of target
#                 attainment (PTA) - the usual simulation-based check.
# Model     : 1-cmt, first-order absorption/elimination, analytical solution
#             CL (L/h), V (L), KA (1/h); allometric WT effect; log-normal BSV
#             with CL-V correlation; combined proportional + additive error
# Dosing    : 100 mg QD x 7 days (NONMEM style: one dose record, ADDL=6, II=24)
# Software  : base R only (no packages)
# Run       : source() in console, Source button, background job, or Rscript.
#             Stop early with Stop / Esc / Ctrl+C (summaries still written).
# Output    : ./pmx_10kb_output/{data,tables,logs}
# Units     : time h, dose mg, concentration mg/L, AUC mg*h/L
# ==============================================================================

# ---- 0. Run control -----------------------------------------------------------
RUN_MINUTES       <- 10       # total wall-clock run time
TARGET_DATA_BYTES <- 10240    # size of the NONMEM dataset (about 10 KB)
N_SUBJ_REP        <- 200      # subjects per Monte Carlo replicate
TARGET_REPS_PER_SEC <- 5      # pacing (light CPU use); Inf = flat out
LOG_EVERY_SECONDS <- 30
SEED              <- 20261021
OUT_ROOT          <- file.path(getwd(), "pmx_10kb_output")

# ---- 1. Model specification (THETA / OMEGA / SIGMA) --------------------------
THETA <- c(CL = 5, V = 50, KA = 1.0)      # L/h, L, 1/h at 70 kg
WT_REF <- 70
WT_EXP <- c(CL = 0.75, V = 1.0)
OMEGA_SD   <- c(CL = 0.30, V = 0.25, KA = 0.40)   # log-scale SDs
OMEGA_CORR <- matrix(c(1.0, 0.5, 0.0,
                       0.5, 1.0, 0.0,
                       0.0, 0.0, 1.0), 3, 3, byrow = TRUE)
OMEGA <- OMEGA_CORR * outer(OMEGA_SD, OMEGA_SD)
SIGMA_PROP <- 0.15                         # proportional SD
SIGMA_ADD  <- 0.02                         # additive SD, mg/L
LLOQ       <- 0.05                         # mg/L; BLQ -> LLOQ/2 and flagged (M5)

DOSE <- 100; TAU <- 24; N_DOSES <- 7
DOSE_TIMES <- (seq_len(N_DOSES) - 1) * TAU
LAST_DOSE  <- max(DOSE_TIMES)
OBS_TIMES  <- sort(unique(c(c(0.5, 1, 2, 4, 8, 12),
                            LAST_DOSE + c(0, 0.5, 1, 2, 4, 8, 12, TAU))))
SS_GRID    <- LAST_DOSE + seq(0, TAU, by = 0.5)
TARGET_CTROUGH <- 0.2                      # mg/L, PTA threshold (placeholder)

# ---- 2. Directories and logging -------------------------------------------------
DIR_DATA <- file.path(OUT_ROOT, "data")
DIR_TABLES <- file.path(OUT_ROOT, "tables")
DIR_LOGS <- file.path(OUT_ROOT, "logs")
for (d in c(DIR_DATA, DIR_TABLES, DIR_LOGS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  if (!dir.exists(d)) stop("Cannot create directory: ", d)
}
invisible(file.remove(list.files(c(DIR_DATA, DIR_TABLES, DIR_LOGS),
  pattern = "^(nm_dataset|exposure_summary|replicate_summary|run_summary|model_spec|session_info|run_log)",
  full.names = TRUE)))

RUN_LOG <- file.path(DIR_LOGS, "run_log.txt")
t_start <- Sys.time()
elapsed <- function() as.numeric(difftime(Sys.time(), t_start, units = "secs"))
say <- function(...) {
  msg <- sprintf("[%7.1fs] %s", elapsed(), sprintf(...))
  cat(msg, "\n", sep = ""); flush.console()
  cat(msg, "\n", sep = "", file = RUN_LOG, append = TRUE)
}
set.seed(SEED)

# ---- 3. Functions ----------------------------------------------------------------
conc_matrix <- function(times, CL, V, KA) {        # rows = subjects, cols = times
  n <- length(CL); K <- CL / V
  KA <- ifelse(abs(KA - K) < 1e-4, KA + 1e-3, KA)  # avoid KA == K singularity
  f <- DOSE * KA / (V * (KA - K))
  out <- matrix(0, n, length(times))
  tm <- matrix(rep(times, each = n), n, length(times))
  for (td in DOSE_TIMES) {
    tt <- tm - td; pos <- tt >= 0; tt <- pmax(tt, 0)
    out <- out + pos * f * (exp(-K * tt) - exp(-KA * tt))
  }
  out
}
simulate_subjects <- function(n) {
  wt  <- pmin(pmax(70 * exp(rnorm(n, 0, 0.20)), 40), 140)
  sex <- rbinom(n, 1, 0.5)
  age <- round(pmin(pmax(rnorm(n, 45, 12), 18), 80))
  eta <- matrix(rnorm(n * 3), n, 3) %*% chol(OMEGA)
  data.frame(ID = seq_len(n), WT = round(wt, 1), SEX = sex, AGE = age,
             CL = as.numeric(THETA["CL"] * (wt / WT_REF)^WT_EXP["CL"] * exp(eta[, 1])),
             V  = as.numeric(THETA["V"]  * (wt / WT_REF)^WT_EXP["V"]  * exp(eta[, 2])),
             KA = as.numeric(THETA["KA"] * exp(eta[, 3])))
}
add_error <- function(ipred) {
  ipred * (1 + rnorm(length(ipred), 0, SIGMA_PROP)) + rnorm(length(ipred), 0, SIGMA_ADD)
}
geomean <- function(x) exp(mean(log(x)))

# NONMEM-style records for the given subjects: 1 dose record (ADDL/II) + observations
build_nm_dataset <- function(subj) {
  n <- nrow(subj); no <- length(OBS_TIMES)
  ipred <- conc_matrix(OBS_TIMES, subj$CL, subj$V, subj$KA)
  dv  <- pmax(add_error(ipred), 0)
  blq <- dv < LLOQ
  dv[blq] <- LLOQ / 2
  dose <- data.frame(ID = subj$ID, TIME = 0, AMT = DOSE, ADDL = N_DOSES - 1, II = TAU,
                     DV = NA_real_, EVID = 1L, CMT = 1L, MDV = 1L, BLQ = 0L)
  obs  <- data.frame(ID = rep(subj$ID, each = no), TIME = rep(OBS_TIMES, times = n),
                     AMT = NA_real_, ADDL = NA_integer_, II = NA_real_,
                     DV = round(as.vector(t(dv)), 4), EVID = 0L, CMT = 2L, MDV = 0L,
                     BLQ = as.integer(as.vector(t(blq))))
  d <- rbind(dose, obs)
  d <- merge(d, subj[, c("ID", "WT", "SEX", "AGE")], by = "ID", sort = FALSE)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  d[, c("ID", "TIME", "AMT", "ADDL", "II", "DV", "EVID", "CMT", "MDV", "BLQ", "WT", "SEX", "AGE")]
}

# ---- 4. Part 1: NONMEM-style dataset of about 10 KB -------------------------------
say("Output directory: %s", normalizePath(OUT_ROOT))
cand <- build_nm_dataset(simulate_subjects(60))     # candidate pool of subjects
tmp  <- tempfile(fileext = ".csv")
write.table(cand, tmp, sep = ",", row.names = FALSE, quote = FALSE, na = ".")
lines <- readLines(tmp); invisible(file.remove(tmp))
bytes <- nchar(lines, type = "bytes") + 1L            # +1 for the newline
ids   <- as.integer(sub(",.*", "", lines[-1]))
cum   <- cumsum(bytes[-1]) + bytes[1]
# keep whole subjects while the file stays within the target size
per_id_end <- tapply(cum, ids, max)
n_keep <- sum(per_id_end <= TARGET_DATA_BYTES)
if (n_keep < 1) stop("TARGET_DATA_BYTES is too small for a single subject")
nm <- cand[cand$ID <= n_keep, ]
f_data <- file.path(DIR_DATA, "nm_dataset.csv")
write.table(nm, f_data, sep = ",", row.names = FALSE, quote = FALSE, na = ".")
if (!file.exists(f_data)) stop("Dataset was not written: ", f_data)
data_bytes <- file.info(f_data)$size
say("NONMEM-style dataset written: %d subjects, %d records, %.2f KB (%s)",
    n_keep, nrow(nm), data_bytes / 1024, f_data)

writeLines(c(
  "MODEL SPECIFICATION",
  "1-compartment oral PK, analytical solution; log-normal BSV; combined error",
  sprintf("THETA: CL=%g L/h, V=%g L, KA=%g 1/h (reference WT %g kg)", THETA["CL"], THETA["V"], THETA["KA"], WT_REF),
  sprintf("Allometric exponents: CL %g, V %g", WT_EXP["CL"], WT_EXP["V"]),
  sprintf("OMEGA SD (log scale): CL %g, V %g, KA %g; corr(CL,V)=%g", OMEGA_SD["CL"], OMEGA_SD["V"], OMEGA_SD["KA"], OMEGA_CORR[1, 2]),
  sprintf("SIGMA: proportional SD %g, additive SD %g mg/L; LLOQ %g mg/L (M5: LLOQ/2)", SIGMA_PROP, SIGMA_ADD, LLOQ),
  sprintf("Dosing: %g mg q%gh x %d (ADDL=%d, II=%g); observations at %s h", DOSE, TAU, N_DOSES, N_DOSES - 1, TAU, paste(OBS_TIMES, collapse = ", ")),
  sprintf("Seed %d", SEED)), file.path(DIR_LOGS, "model_spec.txt"))
writeLines(capture.output(sessionInfo()), file.path(DIR_LOGS, "session_info.txt"))

# ---- 5. Part 2: Monte Carlo replicates until RUN_MINUTES is reached ---------------
MAX_REPS <- 100000
auc_gm <- cmax_gm <- ctr_gm <- pta <- rep(NA_real_, MAX_REPS)
n_rep <- 0; last_log <- 0; stop_reason <- "completed"
say("Simulating replicates of %d subjects for %.1f min...", N_SUBJ_REP, RUN_MINUTES)

tryCatch({
  while (elapsed() < RUN_MINUTES * 60 && n_rep < MAX_REPS) {
    n_rep <- n_rep + 1
    s   <- simulate_subjects(N_SUBJ_REP)
    css <- conc_matrix(SS_GRID, s$CL, s$V, s$KA)
    ng  <- ncol(css)
    auc <- rowSums((css[, -1] + css[, -ng]) / 2) * 0.5      # AUCtau, trapezoid
    cmx <- css[cbind(seq_len(N_SUBJ_REP), max.col(css, ties.method = "first"))]
    ctr <- css[, ng]
    auc_gm[n_rep]  <- geomean(auc)
    cmax_gm[n_rep] <- geomean(cmx)
    ctr_gm[n_rep]  <- geomean(pmax(ctr, 1e-9))
    pta[n_rep]     <- mean(ctr >= TARGET_CTROUGH)

    if (elapsed() - last_log >= LOG_EVERY_SECONDS) {
      say("replicate %5d | AUCtau GM %.2f | Cmax GM %.3f | Ctrough GM %.3f | PTA %.2f",
          n_rep, auc_gm[n_rep], cmax_gm[n_rep], ctr_gm[n_rep], pta[n_rep])
      last_log <- elapsed()
    }
    if (is.finite(TARGET_REPS_PER_SEC)) {
      ahead <- n_rep / TARGET_REPS_PER_SEC - elapsed()
      if (ahead > 0) Sys.sleep(ahead)
    }
  }
}, interrupt = function(e) {
  stop_reason <<- "stopped manually"
}, error = function(e) {
  stop_reason <<- paste("error:", conditionMessage(e))
  message("ERROR: ", conditionMessage(e))
})

# ---- 6. Post-processing (also runs after a manual stop) ---------------------------
expo <- NULL
if (n_rep > 0) {
  ix <- seq_len(n_rep)
  m  <- list(AUCTAU_GM = auc_gm[ix], CMAX_GM = cmax_gm[ix],
             CTROUGH_GM = ctr_gm[ix], PTA_CTROUGH = pta[ix])
  expo <- data.frame(METRIC = names(m),
                     MEDIAN = sapply(m, median),
                     LO95   = sapply(m, quantile, 0.025),
                     HI95   = sapply(m, quantile, 0.975), row.names = NULL)
  write.csv(data.frame(lapply(expo, function(x) if (is.numeric(x)) round(x, 4) else x)),
            file.path(DIR_TABLES, "exposure_summary.csv"), row.names = FALSE, quote = FALSE)
}
summ <- c("POPULATION PK SIMULATION - RUN SUMMARY",
          sprintf("Finished because:  %s", stop_reason),
          sprintf("Runtime:           %.1f s (%.2f min)", elapsed(), elapsed() / 60),
          sprintf("NONMEM dataset:    %d subjects, %.2f KB", n_keep, data_bytes / 1024),
          sprintf("Replicates:        %d x %d subjects", n_rep, N_SUBJ_REP),
          sprintf("Output directory:  %s", normalizePath(OUT_ROOT)))
writeLines(summ, file.path(DIR_LOGS, "run_summary.txt"))
cat("\n", paste(summ, collapse = "\n"), "\n", sep = "")
if (!is.null(expo)) {
  cat("\nExposure summary (median and 95% range across replicates):\n")
  print(expo, row.names = FALSE)
}
