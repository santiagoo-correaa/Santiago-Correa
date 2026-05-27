# =============================================================================
# analytics.R
# Cross-asset analytics: regime detection, tactical signals, correlation,
# PCA macro, Fear & Greed proxy, simple VaR.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(zoo)
})

source("R/utils.R")

# ---- Returns matrix --------------------------------------------------------

build_returns_matrix <- function(price_list, names_map, days = 120) {
  prices <- imap_dfr(price_list, function(df, key) {
    if (is.null(df) || nrow(df) < 30) return(NULL)
    nm <- names_map[[key]]
    if (is.null(nm) || is.na(nm) || nm == "") nm <- key
    tibble(date = df$date, asset = nm, close = as.numeric(df$close))
  })

  if (nrow(prices) == 0) return(tibble(date = as.Date(character())))

  wide <- prices %>%
    filter(!is.na(asset), !is.na(close)) %>%
    pivot_wider(names_from = asset, values_from = close,
                values_fn = list(close = mean)) %>%
    arrange(date) %>%
    tail(days)

  # Keep only numeric (price) columns, drop empty ones
  num_cols <- names(wide)[vapply(wide, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, "date")
  num_cols <- num_cols[vapply(num_cols,
                              function(c) sum(!is.na(wide[[c]])) >= 10,
                              logical(1))]
  wide <- wide[, c("date", num_cols), drop = FALSE]

  wide %>%
    mutate(across(all_of(num_cols), ~ c(NA, diff(log(.x))))) %>%
    drop_na()
}

# ---- Correlation matrix ----------------------------------------------------

correlation_matrix <- function(returns_df) {
  m <- returns_df %>% select(-date) %>% as.matrix()
  cor(m, use = "pairwise.complete.obs")
}

# ---- Market regime classifier ---------------------------------------------
# Heuristic: weighted score across VIX level/Δ, SPX trend, HY OAS, USD trend.

classify_regime <- function(spx_df, vix_df, dxy_df, hy_oas = NULL) {
  score <- 0
  if (!is.null(spx_df) && nrow(spx_df) > 50) {
    sma50  <- mean(tail(spx_df$close, 50))
    sma200 <- mean(tail(spx_df$close, 200))
    last   <- tail(spx_df$close, 1)
    score <- score + ifelse(last > sma50, 1, -1)
    score <- score + ifelse(sma50 > sma200, 1, -1)
  }
  if (!is.null(vix_df) && nrow(vix_df) > 20) {
    vix_last <- tail(vix_df$close, 1)
    score <- score + ifelse(vix_last < 16, 1,
                            ifelse(vix_last < 22, 0, -2))
  }
  if (!is.null(dxy_df) && nrow(dxy_df) > 50) {
    dxy_mom <- momentum(dxy_df$close, 20)
    score <- score - sign(dxy_mom %||% 0)  # USD strength = risk-off lean
  }
  if (!is.null(hy_oas) && nrow(hy_oas) > 30) {
    z <- zscore(hy_oas$value, 252)
    score <- score - ifelse(!is.na(z) && z > 1, 2,
                            ifelse(!is.na(z) && z < -0.5, 1, 0))
  }
  regime <- dplyr::case_when(
    score >=  3 ~ "Risk-On",
    score <= -2 ~ "Risk-Off",
    TRUE        ~ "Neutral"
  )
  list(regime = regime, score = score)
}

# ---- Tactical signals -----------------------------------------------------

generate_tactical_signals <- function(screener_tables, fred_panel) {
  sigs <- list()

  # USD strength
  dxy <- screener_tables$fx %>% filter(asset == "DXY")
  if (nrow(dxy) > 0) {
    sigs$dollar <- ifelse(!is.na(dxy$mom_20) && dxy$mom_20 > 0.01, "Dollar Strength",
                          ifelse(dxy$mom_20 < -0.01, "Dollar Weakness", "Dollar Neutral"))
  }

  # Equity momentum
  spx <- screener_tables$indices %>% filter(asset == "S&P 500")
  if (nrow(spx) > 0) {
    sigs$equity <- ifelse(!is.na(spx$mom_20) && spx$mom_20 > 0.02, "Equity Momentum Bullish",
                          ifelse(spx$mom_20 < -0.02, "Equity Momentum Weakening",
                                 "Equity Momentum Neutral"))
  }

  # Curve / duration
  panel <- fred_panel
  if (!is.null(panel) && nrow(panel) > 0) {
    last_2y  <- panel %>% filter(name == "ust_2y") %>% arrange(date) %>% tail(1) %>% pull(value)
    last_10y <- panel %>% filter(name == "ust_10y") %>% arrange(date) %>% tail(1) %>% pull(value)
    if (length(last_2y) && length(last_10y)) {
      slope <- last_10y - last_2y
      sigs$duration <- ifelse(slope < -0.25, "Curve Inverted — Recession Watch",
                              ifelse(slope > 0.5, "Curve Steepening — Duration Attractive",
                                     "Curve Flat — Duration Neutral"))
    }
  }

  # Commodities
  oil <- screener_tables$commodities %>% filter(asset == "WTI")
  if (nrow(oil) > 0) {
    sigs$commodities <- ifelse(!is.na(oil$mom_20) && oil$mom_20 > 0.05,
                               "Commodities Bullish (inflation pressure)",
                               ifelse(oil$mom_20 < -0.05, "Commodities Bearish (disinflation)",
                                      "Commodities Neutral"))
  }

  # Volatility
  vix <- screener_tables$indices %>% filter(asset == "VIX")
  if (nrow(vix) > 0) {
    sigs$vol <- ifelse(vix$last < 15, "Vol Compression — Risk-On Tilt",
                       ifelse(vix$last > 25, "Vol Expansion — De-Risk",
                              "Vol Range-Bound"))
  }

  sigs
}

# ---- Fear & Greed proxy ---------------------------------------------------
# Composite of: VIX z-score (inverted), SPX 20d momentum, HY OAS z-score (inverted),
# put/call proxy via VIX/VIX3M (skipped if not available), USD strength.

fear_greed_proxy <- function(vix_df, spx_df, hy_oas = NULL, dxy_df = NULL) {
  components <- c()
  if (!is.null(vix_df) && nrow(vix_df) > 60) {
    z <- zscore(vix_df$close, 252)
    components <- c(components, -z)  # higher VIX -> more fear
  }
  if (!is.null(spx_df) && nrow(spx_df) > 60) {
    m <- momentum(spx_df$close, 20)
    components <- c(components, m * 10)  # scale
  }
  if (!is.null(hy_oas) && nrow(hy_oas) > 60) {
    z <- zscore(hy_oas$value, 252)
    components <- c(components, -z)
  }
  if (!is.null(dxy_df) && nrow(dxy_df) > 60) {
    z <- zscore(dxy_df$close, 252)
    components <- c(components, -z * 0.3)  # mild weight
  }
  if (length(components) == 0) return(list(score = 50, label = "N/A"))
  raw <- mean(components, na.rm = TRUE)
  # Map roughly to 0..100
  score <- pmax(0, pmin(100, 50 + raw * 12))
  label <- dplyr::case_when(
    score < 20 ~ "Extreme Fear",
    score < 40 ~ "Fear",
    score < 60 ~ "Neutral",
    score < 80 ~ "Greed",
    TRUE       ~ "Extreme Greed"
  )
  list(score = round(score, 0), label = label)
}

# ---- Simple parametric VaR -------------------------------------------------

simple_var <- function(returns, alpha = 0.05) {
  if (length(returns) < 30) return(NA_real_)
  mu  <- mean(returns, na.rm = TRUE)
  sig <- sd(returns, na.rm = TRUE)
  qnorm(alpha, mu, sig)
}

# ---- PCA macro -------------------------------------------------------------

pca_macro <- function(returns_df, n_pc = 3) {
  m <- returns_df %>% select(-date) %>% as.matrix()
  m <- m[, apply(m, 2, function(x) sd(x, na.rm = TRUE) > 0)]
  if (ncol(m) < 3) return(NULL)
  pr <- prcomp(m, center = TRUE, scale. = TRUE)
  list(
    var_explained = summary(pr)$importance[2, seq_len(min(n_pc, ncol(pr$rotation)))],
    loadings      = pr$rotation[, seq_len(min(n_pc, ncol(pr$rotation))), drop = FALSE]
  )
}
