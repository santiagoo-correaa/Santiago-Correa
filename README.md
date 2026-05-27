# Daily Macro Screener Dashboard

Institutional-grade cross-asset macro dashboard, built in RMarkdown.
Consolida diariamente FX, tasas, commodities, índices globales, crédito,
volatilidad, calendario económico, noticias con sentiment, indicadores
FRED, señales tácticas y postura de mercado — todo en un único HTML
ejecutivo, automatizable vía cron o GitHub Actions.

---

## 1. Estructura del proyecto

```
.
├── daily_macro_dashboard.Rmd     # Reporte principal (entrypoint)
├── R/
│   ├── utils.R                   # logging, caché, formato, estadística
│   ├── data_fetchers.R           # Yahoo / FRED / News / calendario
│   ├── analytics.R               # signals, regime, PCA, VaR, Fear & Greed
│   └── narratives.R              # comentarios financieros automáticos
├── css/
│   └── dashboard.css             # estilo institucional (Bloomberg-like)
├── config/
│   └── config.yml                # universo, TTLs, parámetros
├── scripts/
│   └── run_daily.R               # renderer headless (cron / Actions)
├── data/cache/                   # caché en disco (ignorado por git)
├── output/
│   ├── reports/                  # HTML diarios + "latest"
│   └── logs/                     # logs por día
├── .github/workflows/
│   └── daily_render.yml          # GitHub Actions schedule
├── .Renviron.example             # plantilla de API keys
└── README.md
```

---

## 2. Setup rápido

```r
# 1. Instalar paquetes (la primera vez)
install.packages(c(
  "tidyverse","lubridate","scales","stringr","zoo","purrr",
  "quantmod","tidyquant","fredr",
  "httr","jsonlite","rvest","xml2",
  "plotly","highcharter","reactable","gt","kableExtra",
  "tidytext","syuzhet","yaml","digest","patchwork","htmltools",
  "rmarkdown","here"
))

# 2. Configurar las API keys
file.copy(".Renviron.example", ".Renviron")
# Editar .Renviron y reiniciar R

# 3. Render
rmarkdown::render("daily_macro_dashboard.Rmd")
# o, equivalente:
Rscript scripts/run_daily.R
```

El HTML se genera en `output/reports/daily_macro_<fecha>.html` y se
mantiene un alias `daily_macro_latest.html` para enlace fijo.

---

## 3. API keys

Las claves NUNCA se hardcodean en código. Se leen de variables de
entorno definidas en `.Renviron` (auto-cargado por R al arrancar).

```ini
# .Renviron — NO subir a git
FRED_API_KEY=YOUR_FRED_KEY_HERE          # tu key personal de FRED
NEWSAPI_KEY=opcional_para_noticias_premium
ALPHA_VANTAGE_KEY=opcional
```

> La key de FRED que se te asignó debe pegarse en `.Renviron`
> (NUNCA en código que se versiona). El proxy git de este sandbox
> bloquea pushes que contengan tokens con apariencia de secreto.

- **FRED** (gratis, requerido): https://fredaccount.stlouisfed.org/apikeys
- **NewsAPI** (opcional; si no hay clave, se cae a RSS de Reuters):
  https://newsapi.org/
- **Yahoo Finance**: sin clave, vía `quantmod`/`tidyquant`.
- **Calendario económico**: scraping de `sslecal2.investing.com`
  (público; manejado con tolerancia a fallos).

En `scripts/run_daily.R` y en el chunk `setup` del `.Rmd` hay un
fallback temporal que setea la FRED key vía `Sys.setenv()` — quitar en
producción y usar `.Renviron`.

---

## 4. Automatización diaria

### 4.a · GitHub Actions (recomendado)

Ya incluido en `ci_examples/github_actions_daily_render.yml`.
Para activarlo, **muévelo manualmente** a `.github/workflows/daily_render.yml`
en tu fork (Claude no puede subir archivos en `.github/workflows/` sin
permisos extra de `workflow` scope en el token).
- Corre L-V a las 14:30 UTC (post-apertura NYSE).
- Renderiza el HTML.
- Lo sube como *artifact*.
- Opcional: publica en `gh-pages` para tener URL pública.

Configurar los *secrets* del repo:
`Settings → Secrets and variables → Actions → New repository secret`
- `FRED_API_KEY`
- `NEWSAPI_KEY` *(opcional)*

### 4.b · `cronR` en servidor Linux/Mac

```r
library(cronR)
cmd <- cron_rscript("scripts/run_daily.R")
cron_add(cmd, frequency = "daily", at = "07:00",
         id = "daily-macro", description = "Daily Macro Screener")
```

### 4.c · Windows Task Scheduler

Crear tarea programada que ejecute:

```
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe"
   "C:\proyectos\daily-macro\scripts\run_daily.R"
```

---

## 5. Personalización

| Quiero cambiar...        | Editar                                          |
|--------------------------|-------------------------------------------------|
| Universo de activos      | `config/config.yml` *y* mapas en `R/data_fetchers.R` |
| Lookback histórico       | `config.yml → dashboard.history_days`           |
| TTL de caché             | `config.yml → dashboard.*_ttl_min`              |
| Series FRED              | `FRED_SERIES` en `R/data_fetchers.R`            |
| Reglas tácticas          | `generate_tactical_signals()` en `R/analytics.R`|
| Narrativa automática     | `R/narratives.R`                                |
| Paleta de colores        | variables `--bg-*`, `--accent` en `css/dashboard.css` |

---

## 6. Roadmap / mejoras sugeridas

- **PDF**: pasar a `pagedown::chrome_print()` o `quarto` para PDF
  paginado tipo *research note*.
- **Email diario**: usar `blastula` + `smtp_send()` para distribuir
  por mailing list del comité.
- **Slack / Teams**: webhook con resumen ejecutivo + screenshot del HTML.
- **LLM commentary**: reemplazar plantillas rule-based de
  `narratives.R` por una llamada al Claude API que reciba las tablas
  y genere comentario contextual.
- **Datos México**: integrar Banxico SIE API para CETES/TIIE/Mbonos
  con detalle real (actualmente proxy vía FRED).
- **Carry trade monitor**: añadir tabla EM FX con carry, vol, Sharpe.
- **Nowcasting**: pequeño modelo AR + factor model sobre indicadores
  FRED para "GDP nowcast".
- **Liquidity**: tracking de balance de la Fed, RRP, TGA, M2 vs.
  índices.
- **Backtest panel**: PnL hipotético de las señales tácticas en una
  pestaña separada.
- **Multi-tenant**: parametrizar por *book* / cliente y renderizar N
  variantes con `purrr::walk()`.

---

## 7. Notas operativas

- Todas las descargas pasan por `safely_run()` + caché en
  `data/cache/`. Una caída de Yahoo/FRED **no rompe** el render.
- Los logs quedan en `output/logs/run_<fecha>.log`.
- El `.Rmd` está pensado para ser auto-contenido (`self_contained:
  true`); el HTML resultante es portable.
- Para servir múltiples reports históricos, basta apuntar
  GitHub Pages a `output/reports/`.

---

**Disclaimer:** Documento de uso interno para mesa de inversión.
No constituye recomendación de inversión.
