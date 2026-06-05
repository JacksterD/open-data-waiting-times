# NHS Scotland Waiting Times

An open, automatically-updated dashboard of NHS Scotland waiting times, built
from the [NHS Scotland open data platform](https://www.opendata.nhs.scot/).

**Live site:** https://jacksterd.github.io/open-data-waiting-times/

The dashboard is a **static site** — there is no server. A scheduled job fetches
the live open data, pre-aggregates it, and bakes the results into the page, so
hosting is free (GitHub Pages) and the figures refresh themselves.

## How it works

The project deliberately separates the **data pipeline** from the **website**:

```
pipeline/build-data.R          # fetch CKAN open data -> wrangle + pre-aggregate -> site/data/*.json
site/                          # Quarto dashboard (Observable JS + Observable Plot)
  index.qmd                    # the dashboard (one page per tab)
  theme.scss                   # NHS-styled theme + value boxes / info boxes
  _quarto.yml                  # dashboard config
.github/workflows/publish.yml  # build on push + weekly cron, deploy to GitHub Pages
```

Because every aggregation happens in the pipeline at build time, the browser only
ever *filters* pre-computed rows — no client-side number crunching, no R server.

### Tabs

All six tabs from the original app are implemented:

| Tab | Source data | Notes |
|-----|-------------|-------|
| Waiting Profile | Ongoing waits (stage of treatment) | List size + breakdown by length of wait |
| Balance | Additions & removals | Demand vs capacity |
| 12-Week Target | Completed waits | % seen within 12 weeks, median / 90th-percentile waits |
| Diagnostic Waits | Key diagnostic tests | Cascading test type → description filters |
| Cancer Waiting Times | 31-day & 62-day standards | % treated within standard |
| A&E Waits | Weekly A&E activity | Health board → treatment location; 4/8/12-hour waits |

Each tab has its own sidebar filters and opens with an info box summarising the
relevant Scottish Government / NHS Scotland standard (e.g. the 12-week Treatment
Time Guarantee, the 6-week diagnostics standard, the 95% four-hour A&E standard).

## Local development

You need [R](https://www.r-project.org/) and the
[Quarto CLI](https://quarto.org/docs/get-started/) (recent RStudio bundles
Quarto).

```bash
# 1. Install the R packages used by the pipeline (one time)
Rscript -e 'install.packages(c("readr","dplyr","tidyr","stringr","lubridate","janitor","jsonlite","phsmethods"))'

# 2. Fetch live data and write site/data/*.json
Rscript pipeline/build-data.R

# 3. Preview the dashboard (live-reloading)
quarto preview site
#    or render once:  quarto render site
```

Run the pipeline (step 2) from the repository root — it writes to `site/data/`
relative to the working directory. The generated `site/data/*.json` files are
git-ignored; they are rebuilt on every run and in CI.

To refresh the figures, just re-run step 2.

## Deployment & auto-update

`.github/workflows/publish.yml` runs on every push to `main`, on a weekly cron
(Mondays 06:00 UTC), and on manual dispatch. It installs R, runs the pipeline
against the live open data, renders the Quarto site, and deploys it to GitHub
Pages. Network calls to the open-data API are retried with backoff so a transient
failure doesn't break the build.

Pages is configured under **Settings → Pages** with **Source: GitHub Actions**.

## Original R Shiny app

The original interactive app lives in `waiting-times/` (`global.R`, `ui.R`,
`server.R`) and fetches data live at startup. It is kept as a reference and is
unchanged. Run it with:

```r
shiny::runApp("waiting-times")
```

## Data source

All data comes from [opendata.nhs.scot](https://www.opendata.nhs.scot/) (Public
Health Scotland), resolved at build time via the CKAN API. Health-board names are
derived with [`phsmethods::match_area()`](https://github.com/Public-Health-Scotland/phsmethods).
