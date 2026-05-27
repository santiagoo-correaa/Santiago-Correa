# =============================================================================
# data_fetchers.R
# Modular data ingestion: Yahoo Finance, FRED, NewsAPI, economic calendar.
# Each function is wrapped with safely_run() + with_cache() to be production safe.
# =============================================================================

suppressPackageStartupMessages({
  library(quantmod)
  library(tidyquant)
  library(fredr)
  library(httr)
  library(jsonlite)
  library(rvest)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(stringr)
})

source("R/utils.R")

# ---- Universe definitions --------------------------------------------------

FX_TICKERS <- c(
  "USD/MXN" = "MXN=X",
  "EUR/USD" = "EURUSD=X",
  "DXY"     = "DX-Y.NYB",
  "USD/JPY" = "JPY=X",
  "GBP/USD" = "GBPUSD=X",
  "USD/CNY" = "CNY=X"
)

COMMODITY_TICKERS <- c(
  "Gold"        = "GC=F",
  "Silver"      = "SI=F",
  "Copper"      = "HG=F",
  "WTI"         = "CL=F",
  "Brent"       = "BZ=F",
  "Nat Gas"     = "NG=F"
)

INDEX_TICKERS <- c(
  "S&P 500"          = "^GSPC",
  "Nasdaq 100"       = "^NDX",
  "Dow Jones"        = "^DJI",
  "Russell 2000"     = "^RUT",
  "VIX"              = "^VIX",
  "IPC Mexico"       = "^MXX",
  "Euro Stoxx 50"    = "^STOXX50E",
  "Nikkei"           = "^N225",
  "Shanghai Comp"    = "000001.SS"
)

# US Treasury yields via Yahoo (CBOE indices, already in %)
RATES_TICKERS <- c(
  "UST 2Y"  = "^IRX",   # 13 wk — used as ST proxy when 2Y not available
  "UST 2Y*" = "^FVX",   # 5Y; we use FRED for true 2Y below
  "UST 10Y" = "^TNX",
  "UST 30Y" = "^TYX"
)

# FRED series codes
FRED_SERIES <- list(
  ust_2y       = "DGS2",
  ust_10y      = "DGS10",
  ust_30y      = "DGS30",
  fed_funds    = "DFF",
  sofr         = "SOFR",
  cpi_yoy      = "CPIAUCSL",
  core_pce_yoy = "PCEPILFE",
  unemployment = "UNRATE",
  nfci         = "NFCI",      # Chicago Fed financial conditions
  consumer_conf = "UMCSENT",
  ism_pmi      = "MANEMP",    # proxy if ISM unavailable freely
  hy_oas       = "BAMLH0A0HYM2",  # ICE BofA HY OAS
  ig_oas       = "BAMLC0A0CM",    # ICE BofA IG OAS
  mxn_usd      = "DEXMXUS"
)

# Mexican rates (Banxico has its own API; we proxy via FRED where possible)
FRED_MEXICO <- list(
  cetes_28   = "INTGSTMXM193N",  # may rate-limit; fallback handled
  mx_10y     = "IRLTLT01MXM156N"
)

# ---- Yahoo Finance bulk fetch ---------------------------------------------

fetch_yahoo_prices <- function(tickers, from = Sys.Date() - 400,
                               to = Sys.Date(), cache_key = NULL,
                               ttl_min = 60) {
  key <- cache_key %||% paste0("yahoo_", digest::digest(list(tickers, from, to)))
  with_cache(key, ttl_min, {
    log_msg(sprintf("Fetching %d Yahoo tickers...", length(tickers)))
    out <- map(tickers, function(t) {
      safely_run({
        df <- quantmod::getSymbols(t, from = from, to = to,
                                   auto.assign = FALSE, warnings = FALSE)
        df <- na.omit(df)
        tibble(
          date  = zoo::index(df),
          open  = as.numeric(df[, 1]),
          high  = as.numeric(df[, 2]),
          low   = as.numeric(df[, 3]),
          close = as.numeric(df[, 4]),
          volume = if (ncol(df) >= 5) as.numeric(df[, 5]) else NA_real_
        )
      }, label = paste0("yahoo:", t))
    })
    out
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Summary table builder -------------------------------------------------
# Given a named list of OHLCV tibbles, produce a screener-style table.

build_screener_table <- function(price_list, names_map) {
  imap_dfr(price_list, function(df, key) {
    if (is.null(df) || nrow(df) < 5) {
      return(tibble(asset = names_map[key] %||% key,
                    last = NA_real_, chg_d = NA_real_,
                    chg_w = NA_real_, chg_m = NA_real_,
                    chg_ytd = NA_real_, vol_20 = NA_real_,
                    spark = list(NULL)))
    }
    df <- arrange(df, date)
    last_px  <- tail(df$close, 1)
    prev_px  <- tail(df$close, 2)[1]
    wk_px    <- tail(df$close, 6)[1]
    mo_px    <- tail(df$close, 22)[1]
    ytd_start <- df$close[which(year(df$date) == year(Sys.Date()))[1]]
    rets <- diff(log(df$close))
    tibble(
      asset   = names_map[key] %||% key,
      last    = last_px,
      chg_d   = last_px / prev_px - 1,
      chg_w   = last_px / wk_px - 1,
      chg_m   = last_px / mo_px - 1,
      chg_ytd = if (!is.na(ytd_start)) last_px / ytd_start - 1 else NA_real_,
      vol_20  = ann_vol(tail(rets, 20)),
      mom_20  = momentum(df$close, 20),
      z_60    = zscore(df$close, 60),
      drawdown = tail(drawdown(df$close), 1),
      spark   = list(tail(df$close, 60))
    )
  })
}

# ---- FRED wrapper ----------------------------------------------------------

ensure_fred_key <- function() {
  k <- Sys.getenv("FRED_API_KEY")
  if (nchar(k) == 0) {
    stop("FRED_API_KEY no está definido. Use .Renviron o Sys.setenv().")
  }
  fredr_set_key(k)
}

fetch_fred_series <- function(series_id, observation_start = Sys.Date() - years(3),
                              ttl_min = 720) {
  with_cache(paste0("fred_", series_id), ttl_min, {
    safely_run({
      ensure_fred_key()
      fredr(series_id = series_id,
            observation_start = as.Date(observation_start),
            observation_end   = Sys.Date())
    }, label = paste0("fred:", series_id))
  })
}

fetch_fred_panel <- function(series_list, ttl_min = 720) {
  imap_dfr(series_list, function(id, name) {
    df <- fetch_fred_series(id, ttl_min = ttl_min)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>%
      transmute(date, name = !!name, series_id = id, value)
  })
}

# ---- News fetcher (NewsAPI optional, RSS fallback) -------------------------
# If NEWSAPI_KEY is set we hit the real endpoint; otherwise we scrape Reuters/Yahoo RSS.

fetch_news <- function(query = "markets OR fed OR inflation OR treasury OR oil",
                       n = 25, ttl_min = 30) {
  with_cache(paste0("news_", digest::digest(query)), ttl_min, {
    api_key <- Sys.getenv("NEWSAPI_KEY")
    if (nchar(api_key) > 0) {
      safely_run({
        resp <- GET("https://newsapi.org/v2/everything",
                    query = list(q = query, pageSize = n,
                                 sortBy = "publishedAt", language = "en",
                                 apiKey = api_key))
        stop_for_status(resp)
        parsed <- content(resp, as = "parsed")
        tibble(
          title = map_chr(parsed$articles, "title", .default = NA),
          source = map_chr(parsed$articles, ~ .x$source$name %||% NA),
          published = ymd_hms(map_chr(parsed$articles, "publishedAt", .default = NA)),
          url = map_chr(parsed$articles, "url", .default = NA),
          summary = map_chr(parsed$articles, "description", .default = NA)
        )
      }, label = "newsapi")
    } else {
      safely_run({
        rss <- "https://feeds.reuters.com/reuters/businessNews"
        doc <- read_xml(rss)
        items <- xml2::xml_find_all(doc, "//item")
        tibble(
          title     = xml2::xml_text(xml2::xml_find_first(items, "title")),
          source    = "Reuters",
          published = ymd_hms(xml2::xml_text(xml2::xml_find_first(items, "pubDate")),
                              quiet = TRUE),
          url       = xml2::xml_text(xml2::xml_find_first(items, "link")),
          summary   = xml2::xml_text(xml2::xml_find_first(items, "description"))
        ) %>% head(n)
      }, label = "reuters-rss",
      fallback = tibble(title = character(), source = character(),
                        published = as.POSIXct(character()),
                        url = character(), summary = character()))
    }
  })
}

# ---- Economic calendar -----------------------------------------------------
# Scrapes Investing.com economic calendar (public). If it fails, returns a stub.

fetch_econ_calendar <- function(days_ahead = 5, ttl_min = 360) {
  with_cache("econ_calendar", ttl_min, {
    safely_run({
      url <- "https://sslecal2.investing.com/?columns=exc_flags,exc_currency,exc_importance,exc_actual,exc_forecast,exc_previous&importance=2,3&features=datepicker,timezone&countries=5,6,17,72&calType=week&timeZone=8&lang=1"
      doc <- read_html(url)
      tbl <- doc %>%
        html_node("table#economicCalendarData") %>%
        html_table(fill = TRUE)
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      tbl <- as_tibble(tbl, .name_repair = "unique")
      names(tbl) <- c("time","ccy","importance","event","actual","forecast","previous")[seq_len(ncol(tbl))]
      tbl %>%
        filter(!is.na(event), event != "") %>%
        head(40)
    }, label = "econ-calendar",
    fallback = tibble(time = character(), ccy = character(),
                      importance = character(), event = character(),
                      actual = character(), forecast = character(),
                      previous = character()))
  })
}
