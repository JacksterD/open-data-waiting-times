# NHS Scotland Waiting Times

An open, automatically-updated view of NHS Scotland waiting times, built from
the [NHS Scotland open data platform](https://www.opendata.nhs.scot/).

## Two ways the project runs

### 1. Static Quarto site (new — self-updating, free to host)

A static dashboard published to GitHub Pages. The data is fetched and
pre-aggregated at build time, so the published site needs no live server.

```
pipeline/build-data.R        # fetch CKAN data, wrangle, pre-aggregate -> site/data/*.json
site/                        # Quarto dashboard (Observable JS + Observable Plot)
.github/workflows/publish.yml  # weekly cron: rebuild + deploy to gh-pages
```

Build locally:

```bash
Rscript pipeline/build-data.R      # writes site/data/*.json
quarto preview site                # or: quarto render site
```

The GitHub Actions workflow does the same on every push and weekly, then
publishes to the `gh-pages` branch. Enable it under **Settings → Pages**
(source: `gh-pages` branch).

Currently implemented: **Waiting Profile**. The remaining tabs (Balance,
12-week Target, Diagnostics, Cancer, A&E) follow the same recipe — add a
section to `build-data.R` and a page to the dashboard.

### 2. R Shiny app (original)

The original interactive app lives in `waiting-times/` (`global.R`, `ui.R`,
`server.R`) and fetches data live at startup. Run with `shiny::runApp("waiting-times")`.
