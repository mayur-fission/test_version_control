# ==============================================================================
# Project   : Population PK estimation check with nlmixr2 (simulate-estimate)
# Script    : nlmixr2_sse_10min.R
# Purpose   : Repeatedly (1) simulate a small population PK dataset from a known
#             1-compartment oral model with between-subject variability and
#             combined residual error, then (2) fit it with nlmixr2 (FOCEi)
#             starting from wrong initial values, and (3) record how well the
#             true parameters are recovered (bias and RMSE). Runs for
#             RUN_MINUTES (default 10).
# Model     : CL (L/h), V (L), KA (1/h); log-normal IIV; add + prop error
# Design    : 100 mg single oral dose, 13 samples to 48 h, 40 subjects
# Software  : R with nlmixr2 (install.packages("nlmixr2")); a C/C++ compiler is
#             needed (Rtools on Windows) because rxode2 compiles the model.
# Run       : source() in the RStudio console, Source button, background job,
#             or Rscript. Stop early with Stop / Esc / Ctrl+C (summary is
#             still written).
# Output    : ./nlmixr2_output/{data,tables,logs}
# Units     : time h, dose mg, concentration mg/L
# ==============================================================================

# ---- 0. Packages --------------------------------------------------------------
if (!requireNamespace("nlmixr2", quietly = TRUE)) {
  stop("Package 'nlmixr2' is not installed. Run: install.packages(\"nlmixr2\")")
}
suppressPackageStartupMessages({ library(nlmixr2); library(rxode2) })

# ---- 1. Run control -----------------------------------------------------------
RUN_MINUTES <- 10       # total wall-clock run time
N_SUBJ      <- 40       # subjects per simulated study
DOSE        <- 100      # mg
OBS_TIMES   <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 16, 24, 36, 48)
LLOQ        <- 0.01     # mg/L; simulated values below this are set to LLOQ
SEED        <- 20261022
OUT_ROOT    <- file.path(getwd(), "nlmixr2_output")

# ---- 2. Model (true values in ini(); start values are changed for the fit) ----
# Parameters: tcl/tv/tka = log of typical CL (L/h), V (L), KA (1/h); eta.* = IIV
# variances on the log scale; add.sd (mg/L) and prop.sd = residual error SDs.
# (No comments inside ini()/model() so nlmixr2 stays quiet in every run mode.)
pk_model <- function() {
  ini({
    tcl <- log(5)
    tv  <- log(50)
    tka <- log(1)
    eta.cl ~ 0.09
    eta.v  ~ 0.0625
    eta.ka ~ 0.16
    add.sd  <- 0.02
    prop.sd <- 0.15
  })
  model({
    cl <- exp(tcl + eta.cl)
    v  <- exp(tv  + eta.v)
    ka <- exp(tka + eta.ka)
    d/dt(depot) <- -ka * depot
    d/dt(centr) <-  ka * depot - cl / v * centr
    cp <- centr / v
    cp ~ add(add.sd) + prop(prop.sd)
  })
}
TRUTH <- c(CL = 5, V = 50, KA = 1, OMEGA_CL = 0.3, OMEGA_V = 0.25, OMEGA_KA = 0.4,
           ADD_SD = 0.02, PROP_SD = 0.15)

# ---- 3. Directories and logging --------------------------------------------------
dirs <- file.path(OUT_ROOT, c("data", "tables", "logs"))
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  if (!dir.exists(d)) stop("Cannot create directory: ", d)
}
invisible(file.remove(list.files(dirs,
  pattern = "^(nlmixr2_dataset_rep001|sse_estimates|sse_summary|fit_rep001|run_summary|run_log|session_info)",
  full.names = TRUE)))
RUN_LOG <- file.path(OUT_ROOT, "logs", "run_log.txt")
t_start <- Sys.time()
elapsed <- function() as.numeric(difftime(Sys.time(), t_start, units = "secs"))
say <- function(...) {
  msg <- sprintf("[%7.1fs] %s", elapsed(), sprintf(...))
  cat(msg, "\n", sep = ""); flush.console()
  cat(msg, "\n", sep = "", file = RUN_LOG, append = TRUE)
}
writeLines(capture.output(sessionInfo()), file.path(OUT_ROOT, "logs", "session_info.txt"))

# ---- 4. Functions ------------------------------------------------------------------
# Simulate one study from the true model; returns a NONMEM-style data.frame
simulate_study <- function() {
  ev <- et(amt = DOSE, cmt = "depot")
  ev <- et(ev, time = OBS_TIMES)
  ev <- et(ev, id = seq_len(N_SUBJ))
  
  sim <- as.data.frame(suppressMessages(rxSolve(pk_model, ev)))
  obs <- data.frame(ID = sim$id, TIME = sim$time, DV = round(pmax(sim$sim, LLOQ), 4),
                    EVID = 0L, AMT = NA_real_, CMT = 2L)
  dos <- data.frame(ID = seq_len(N_SUBJ), TIME = 0, DV = NA_real_, EVID = 1L,
                    AMT = DOSE, CMT = 1L)
  d <- rbind(dos, obs)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  rownames(d) <- NULL
  d
}

fit_study <- function(dat) {
  start <- suppressMessages(
    ini(pk_model, tcl = log(3.5), tv = log(65), tka = log(0.6),
        eta.cl = 0.15, eta.v = 0.15, eta.ka = 0.3, add.sd = 0.05, prop.sd = 0.25))
  fit <- NULL
  invisible(capture.output(suppressMessages(suppressWarnings(
    fit <- nlmixr2(start, dat, est = "focei",
                   control = foceiControl(print = 0, covMethod = ""))
  ))))
  fit
}

extract_estimates <- function(fit) {
  th <- fit$theta; om <- diag(fit$omega)
  c(CL = exp(th[["tcl"]]), V = exp(th[["tv"]]), KA = exp(th[["tka"]]),
    OMEGA_CL = sqrt(om[["eta.cl"]]), OMEGA_V = sqrt(om[["eta.v"]]), OMEGA_KA = sqrt(om[["eta.ka"]]),
    ADD_SD = th[["add.sd"]], PROP_SD = th[["prop.sd"]])
}

# ---- 5. Main loop --------------------------------------------------------------------
say("Output directory: %s", normalizePath(OUT_ROOT))
say("Simulate-estimate loop: %d subjects/study, FOCEi, for %.1f min", N_SUBJ, RUN_MINUTES)

est_names <- names(TRUTH)
rows <- list(); n_try <- 0; n_ok <- 0; fit_secs <- numeric(0)
stop_reason <- "completed"

tryCatch({
  repeat {
    avg <- if (length(fit_secs)) mean(fit_secs) else 0
    if (elapsed() + 1.2 * avg >= RUN_MINUTES * 60) break     # next fit would overrun
    n_try <- n_try + 1
    set.seed(SEED + n_try)
    t0 <- Sys.time()
    dat <- simulate_study()
    if (n_try == 1) {
      write.csv(dat, file.path(OUT_ROOT, "data", "nlmixr2_dataset_rep001.csv"),
                row.names = FALSE, quote = FALSE, na = ".")
    }
    est <- tryCatch({
      fit <- fit_study(dat)
      if (n_try == 1) {
        writeLines(capture.output(print(fit)), file.path(OUT_ROOT, "logs", "fit_rep001.txt"))
      }
      c(extract_estimates(fit), OBJF = fit$objf)
    }, error = function(e) NULL)
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (n_try > 1) fit_secs <- c(fit_secs, secs)   # first one includes model compilation
    if (is.null(est)) {
      say("study %3d | fit FAILED (%.1f s)", n_try, secs)
    } else {
      n_ok <- n_ok + 1
      rows[[n_ok]] <- data.frame(REP = n_try, t(est), SECONDS = round(secs, 2), row.names = NULL)
      say("study %3d | CL %.2f | V %.1f | KA %.2f | OBJF %.1f | %.1f s",
          n_try, est[["CL"]], est[["V"]], est[["KA"]], est[["OBJF"]], secs)
    }
  }
  # use any remaining time budget so the run lasts RUN_MINUTES
  left <- RUN_MINUTES * 60 - elapsed()
  if (left > 0) { say("Waiting %.0f s to complete the %g-minute run", left, RUN_MINUTES); Sys.sleep(left) }
}, interrupt = function(e) {
  stop_reason <<- "stopped manually"
}, error = function(e) {
  stop_reason <<- paste("error:", conditionMessage(e))
  message("ERROR: ", conditionMessage(e))
})

# ---- 6. Summaries (also after a manual stop) ---------------------------------------------
smry <- NULL
if (n_ok > 0) {
  est_df <- do.call(rbind, rows)
  num <- vapply(est_df, is.numeric, logical(1))
  write.csv(data.frame(est_df[1], round(est_df[-1], 5)), file.path(OUT_ROOT, "tables", "sse_estimates.csv"),
            row.names = FALSE, quote = FALSE)
  smry <- do.call(rbind, lapply(est_names, function(p) {
    x <- est_df[[p]]; tru <- TRUTH[[p]]
    data.frame(PARAMETER = p, TRUE_VALUE = tru, MEDIAN_EST = median(x),
               REL_BIAS_PCT = 100 * (mean(x) - tru) / tru,
               REL_RMSE_PCT = 100 * sqrt(mean((x - tru)^2)) / tru, row.names = NULL)
  }))
  write.csv(data.frame(smry[1], round(smry[-1], 4)), file.path(OUT_ROOT, "tables", "sse_summary.csv"),
            row.names = FALSE, quote = FALSE)
}
summ <- c("POPULATION PK SIMULATE-ESTIMATE (nlmixr2) - RUN SUMMARY",
          sprintf("Finished because: %s", stop_reason),
          sprintf("Runtime:          %.1f s (%.2f min)", elapsed(), elapsed() / 60),
          sprintf("Studies:          %d simulated, %d fitted OK, %d failed", n_try, n_ok, n_try - n_ok),
          sprintf("nlmixr2 version:  %s", as.character(packageVersion("nlmixr2"))),
          sprintf("Output directory: %s", normalizePath(OUT_ROOT)))
writeLines(summ, file.path(OUT_ROOT, "logs", "run_summary.txt"))
cat("\n", paste(summ, collapse = "\n"), "\n", sep = "")
if (!is.null(smry)) {
  cat("\nParameter recovery across studies:\n")
  print(smry, row.names = FALSE, digits = 4)
}
