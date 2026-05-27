# =============================================================================
# scripts/run_daily.R
# Headless renderer for cron / Task Scheduler / GitHub Actions.
# Usage:
#   Rscript scripts/run_daily.R
# =============================================================================

suppressPackageStartupMessages({
  library(rmarkdown)
  library(here)
})

setwd(here::here())

# --- API keys: read from environment (.Renviron in production) --------------
# Local dev fallback (DO NOT commit a real key to git):
if (nchar(Sys.getenv("FRED_API_KEY")) == 0) {
  stop("FRED_API_KEY not set. Configure it in .Renviron (see .Renviron.example).")
}

out_dir <- file.path("output", "reports")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

date_stamp <- format(Sys.Date(), "%Y-%m-%d")
out_file   <- file.path(out_dir, paste0("daily_macro_", date_stamp, ".html"))

message(sprintf("[%s] Rendering -> %s", Sys.time(), out_file))

rmarkdown::render(
  input       = "daily_macro_dashboard.Rmd",
  output_file = basename(out_file),
  output_dir  = out_dir,
  quiet       = FALSE,
  envir       = new.env()
)

# Keep a "latest" symlink/copy for convenience
latest <- file.path(out_dir, "daily_macro_latest.html")
file.copy(out_file, latest, overwrite = TRUE)

message(sprintf("[%s] Done. Latest at %s", Sys.time(), latest))
