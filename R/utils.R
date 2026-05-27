# =============================================================================
# utils.R
# Helper utilities: safe API wrappers, formatting, caching, logging
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(scales)
  library(stringr)
  library(purrr)
  library(zoo)
})

# ---- Logging ---------------------------------------------------------------

log_msg <- function(msg, level = "INFO") {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [%s] %s", ts, level, msg)
  message(line)
  log_dir <- "output/logs"
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  cat(line, "\n", file = file.path(log_dir, paste0("run_", Sys.Date(), ".log")), append = TRUE)
}

# ---- Safe execution wrapper ------------------------------------------------
# Wraps any expression with tryCatch; returns NULL on failure and logs error.

safely_run <- function(expr, label = "task", fallback = NULL) {
  out <- tryCatch(
    force(expr),
    error = function(e) {
      log_msg(sprintf("FAILED [%s]: %s", label, conditionMessage(e)), "ERROR")
      fallback
    },
    warning = function(w) {
      log_msg(sprintf("WARN [%s]: %s", label, conditionMessage(w)), "WARN")
      suppressWarnings(force(expr))
    }
  )
  out
}

# ---- Caching helpers -------------------------------------------------------
# Avoid hammering APIs: cache responses on disk for `ttl` minutes.

cache_get <- function(key, ttl_min = 60) {
  cache_dir <- "data/cache"
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  path <- file.path(cache_dir, paste0(key, ".rds"))
  if (!file.exists(path)) return(NULL)
  age_min <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "mins"))
  if (age_min > ttl_min) return(NULL)
  readRDS(path)
}

cache_put <- function(key, value) {
  cache_dir <- "data/cache"
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  saveRDS(value, file.path(cache_dir, paste0(key, ".rds")))
  invisible(value)
}

with_cache <- function(key, ttl_min = 60, expr) {
  hit <- cache_get(key, ttl_min)
  if (!is.null(hit)) return(hit)
  val <- force(expr)
  if (!is.null(val)) cache_put(key, val)
  val
}

# ---- Number / date formatters ---------------------------------------------

fmt_pct <- function(x, digits = 2) {
  ifelse(is.na(x), "—", sprintf(paste0("%+.", digits, "f%%"), x * 100))
}

fmt_bps <- function(x, digits = 1) {
  ifelse(is.na(x), "—", sprintf(paste0("%+.", digits, "f bps"), x * 10000))
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "—", formatC(x, format = "f", big.mark = ",", digits = digits))
}

today_label <- function() format(Sys.Date(), "%A, %B %d, %Y")

# ---- Statistical helpers ---------------------------------------------------

zscore <- function(x, n = 252) {
  if (length(x) < 30) return(NA_real_)
  win <- tail(x, n)
  (tail(win, 1) - mean(win, na.rm = TRUE)) / sd(win, na.rm = TRUE)
}

ann_vol <- function(returns, periods = 252) {
  sd(returns, na.rm = TRUE) * sqrt(periods)
}

drawdown <- function(prices) {
  cummax_p <- cummax(prices)
  (prices / cummax_p) - 1
}

momentum <- function(prices, n = 20) {
  if (length(prices) < n + 1) return(NA_real_)
  tail(prices, 1) / tail(prices, n + 1)[1] - 1
}
