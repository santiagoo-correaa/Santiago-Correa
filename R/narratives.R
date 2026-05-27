# =============================================================================
# narratives.R
# Automatic financial commentary generation (rule-based "research desk" voice).
# Each builder returns a Markdown string ready to be inserted in the .Rmd.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(syuzhet)
})

source("R/utils.R")

# ---- FX narrative ----------------------------------------------------------

narrate_fx <- function(fx_tbl) {
  if (is.null(fx_tbl) || nrow(fx_tbl) == 0) return("Datos cambiarios no disponibles.")
  dxy <- fx_tbl %>% filter(asset == "DXY")
  mxn <- fx_tbl %>% filter(asset == "USD/MXN")
  eur <- fx_tbl %>% filter(asset == "EUR/USD")
  parts <- c()
  if (nrow(dxy) > 0) {
    dir <- ifelse(dxy$chg_d > 0, "se fortalece", "cede")
    parts <- c(parts, sprintf("El **USD %s** hoy (%s en DXY).",
                              dir, fmt_pct(dxy$chg_d)))
  }
  if (nrow(mxn) > 0) {
    dir <- ifelse(mxn$chg_d > 0, "se deprecia frente al dólar",
                  "se aprecia frente al dólar")
    parts <- c(parts, sprintf("El **peso mexicano %s** (%s, último %s).",
                              dir, fmt_pct(mxn$chg_d), fmt_num(mxn$last, 4)))
  }
  if (nrow(eur) > 0 && !is.na(eur$mom_20)) {
    bias <- ifelse(eur$mom_20 > 0, "sesgo alcista", "sesgo bajista")
    parts <- c(parts, sprintf("EUR/USD mantiene **%s** a 20 días (%s).",
                              bias, fmt_pct(eur$mom_20)))
  }
  paste(parts, collapse = " ")
}

# ---- Rates narrative -------------------------------------------------------

narrate_rates <- function(fred_panel) {
  if (is.null(fred_panel) || nrow(fred_panel) == 0) {
    return("Datos de FRED no disponibles para tasas.")
  }
  last_val <- function(nm) {
    fred_panel %>% filter(name == nm) %>% arrange(date) %>% tail(1) %>% pull(value)
  }
  prev_val <- function(nm) {
    fred_panel %>% filter(name == nm) %>% arrange(date) %>% tail(5) %>% head(1) %>% pull(value)
  }
  ust2 <- last_val("ust_2y"); ust10 <- last_val("ust_10y")
  ust2_p <- prev_val("ust_2y"); ust10_p <- prev_val("ust_10y")

  parts <- c()
  if (length(ust10)) {
    chg <- (ust10 - (ust10_p %||% ust10)) * 100
    dir <- ifelse(chg >= 0, "subió", "cayó")
    parts <- c(parts, sprintf("El **UST 10Y** %s a %.2f%% (%+.1f pb vs. 5d).",
                              dir, ust10, chg))
  }
  if (length(ust2) && length(ust10)) {
    slope <- (ust10 - ust2) * 100
    parts <- c(parts, sprintf("Pendiente **2s10s en %+.0f pb** — %s.",
                              slope,
                              ifelse(slope < 0, "curva invertida, señal histórica de recesión",
                                     ifelse(slope < 50, "curva plana",
                                            "curva con pendiente positiva"))))
  }
  paste(parts, collapse = " ")
}

# ---- Commodities narrative -------------------------------------------------

narrate_commodities <- function(com_tbl) {
  if (is.null(com_tbl) || nrow(com_tbl) == 0) return("Sin datos de commodities.")
  oil  <- com_tbl %>% filter(asset == "WTI")
  gold <- com_tbl %>% filter(asset == "Gold")
  cop  <- com_tbl %>% filter(asset == "Copper")
  parts <- c()
  if (nrow(oil) > 0) {
    tone <- ifelse(oil$chg_d > 0.01, "rebota con fuerza",
            ifelse(oil$chg_d < -0.01, "corrige",
                   "opera lateral"))
    parts <- c(parts, sprintf("El **WTI %s** (%s, último US$%s).",
                              tone, fmt_pct(oil$chg_d), fmt_num(oil$last, 2)))
  }
  if (nrow(gold) > 0) {
    parts <- c(parts, sprintf("El **oro** se ubica en US$%s (%s diario), %s.",
                              fmt_num(gold$last, 2), fmt_pct(gold$chg_d),
                              ifelse(gold$mom_20 > 0, "manteniendo tendencia alcista de mediano plazo",
                                     "con momentum más débil")))
  }
  if (nrow(cop) > 0 && !is.na(cop$mom_20)) {
    parts <- c(parts, sprintf("Cobre con momentum 20d de %s — proxy de demanda global %s.",
                              fmt_pct(cop$mom_20),
                              ifelse(cop$mom_20 > 0, "constructivo", "deteriorándose")))
  }
  paste(parts, collapse = " ")
}

# ---- Equities narrative ----------------------------------------------------

narrate_equities <- function(idx_tbl) {
  if (is.null(idx_tbl) || nrow(idx_tbl) == 0) return("Sin datos de índices.")
  spx <- idx_tbl %>% filter(asset == "S&P 500")
  vix <- idx_tbl %>% filter(asset == "VIX")
  ipc <- idx_tbl %>% filter(asset == "IPC Mexico")
  ndx <- idx_tbl %>% filter(asset == "Nasdaq 100")
  parts <- c()
  if (nrow(spx) > 0) {
    parts <- c(parts, sprintf("**S&P 500** %s (%s), %s desde máximos.",
                              ifelse(spx$chg_d > 0, "avanza", "retrocede"),
                              fmt_pct(spx$chg_d),
                              fmt_pct(spx$drawdown)))
  }
  if (nrow(ndx) > 0) {
    parts <- c(parts, sprintf("Nasdaq 100 con momentum 20d %s.", fmt_pct(ndx$mom_20)))
  }
  if (nrow(vix) > 0) {
    tone <- ifelse(vix$last < 15, "complacencia",
            ifelse(vix$last < 20, "normalidad",
            ifelse(vix$last < 28, "stress moderado", "stress elevado")))
    parts <- c(parts, sprintf("VIX en %.1f indica **%s**.", vix$last, tone))
  }
  if (nrow(ipc) > 0) {
    parts <- c(parts, sprintf("**IPC México** %s (%s diario).",
                              ifelse(ipc$chg_d > 0, "al alza", "a la baja"),
                              fmt_pct(ipc$chg_d)))
  }
  paste(parts, collapse = " ")
}

# ---- News sentiment --------------------------------------------------------

analyze_news_sentiment <- function(news_df) {
  if (is.null(news_df) || nrow(news_df) == 0) {
    return(list(
      scored = tibble(title = character(), sentiment = numeric(), label = character()),
      summary = "Sin titulares disponibles.",
      top_words = tibble(word = character(), n = integer()),
      avg_score = 0
    ))
  }
  text <- paste(news_df$title, news_df$summary %||% "", sep = ". ")
  scores <- safely_run(syuzhet::get_sentiment(text, method = "syuzhet"),
                       label = "sentiment", fallback = rep(0, length(text)))
  scored <- news_df %>%
    mutate(
      sentiment = scores,
      label = case_when(
        sentiment >  0.5  ~ "Bullish",
        sentiment < -0.5  ~ "Bearish",
        TRUE              ~ "Neutral"
      )
    )

  # top words
  stop_words_local <- tidytext::stop_words
  tw <- scored %>%
    select(title) %>%
    tidytext::unnest_tokens(word, title) %>%
    anti_join(stop_words_local, by = "word") %>%
    filter(str_detect(word, "[a-z]"), nchar(word) > 3) %>%
    count(word, sort = TRUE) %>%
    head(15)

  avg <- mean(scored$sentiment, na.rm = TRUE)
  tone <- case_when(
    avg >  0.3 ~ "tono **bullish**",
    avg < -0.3 ~ "tono **bearish**",
    TRUE       ~ "tono **neutral**"
  )
  summary <- sprintf("El flujo de noticias del día muestra %s (score promedio %+.2f).",
                     tone, avg)
  list(scored = scored, summary = summary, top_words = tw, avg_score = avg)
}

# ---- Executive summary -----------------------------------------------------

build_executive_summary <- function(regime, signals, fg, fx_tbl, idx_tbl, news_sent) {
  bullets <- c(
    sprintf("- **Régimen de mercado:** %s (score %+d).",
            regime$regime, regime$score),
    sprintf("- **Fear & Greed proxy:** %d — %s.", fg$score, fg$label),
    sprintf("- **Señales tácticas:** %s.",
            paste(unlist(signals), collapse = "; ")),
    sprintf("- **Tono de noticias:** %s.", news_sent$summary)
  )
  paste(bullets, collapse = "\n")
}

# ---- Market posture (final section) ---------------------------------------

build_market_posture <- function(regime, signals, fred_panel, fg) {
  posture <- case_when(
    regime$regime == "Risk-On"  ~ "**Sobreponderar renta variable**, infraponderar duración corta, mantener exposición a commodities cíclicos.",
    regime$regime == "Risk-Off" ~ "**Reducir beta de equity**, sobreponderar UST y oro, neutralizar HY y EM, considerar puts protectivos.",
    TRUE                        ~ "**Mantener neutralidad táctica**: barbell entre cash/UST corta y selectividad en equity de calidad."
  )

  risks <- c(
    "Sorpresas inflacionarias (CPI/PCE) que reprecien la curva.",
    "Escalada geopolítica con impacto en crudo y safe-havens.",
    "Deterioro en spreads de crédito (HY OAS) como señal temprana.",
    "Liquidez del Tesoro y operaciones del Treasury / Fed."
  )

  narrative <- sprintf(
    paste0("El mercado opera en régimen **%s** con Fear & Greed en %d (%s). ",
           "La narrativa dominante combina la trayectoria de tasas, ",
           "la fortaleza del USD y el flujo de noticias macro. ",
           "**Posicionamiento sugerido:** %s"),
    regime$regime, fg$score, fg$label, posture
  )

  list(
    narrative = narrative,
    risks     = risks,
    posture   = posture
  )
}
