# build-data.R
# ---------------------------------------------------------------------------
# Data pipeline for the static Quarto site.
#
# This script does the heavy lifting ONCE, at build time (locally or in CI):
#   1. fetches the live NHS Scotland open data from the CKAN API
#   2. wrangles + PRE-AGGREGATES it to the exact grain each chart needs
#      (including an "All" specialty roll-up)
#   3. writes compact JSON into site/data/ for the Quarto dashboard to read
#
# Because all aggregation happens here, the published site is fully static:
# the browser only ever *filters* pre-computed rows — no R server required.
#
# To extend the site to another tab, add a new section following the
# `waiting_distribution` example below and write another JSON file.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(janitor)
  library(jsonlite)
  library(phsmethods)
})

# Resolve output dir relative to this script's location so it works from
# anywhere (CI runs it from the repo root).
out_dir <- file.path("site", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- CKAN helper -----------------------------------------------------------
get_ckan_url <- function(resource_id) {
  api_url <- paste0(
    "https://www.opendata.nhs.scot/api/3/action/resource_show?id=",
    resource_id
  )
  fromJSON(api_url)$result$url
}

# --- Health board lookup ---------------------------------------------------
# The original Shiny app's `area_lookup` mapped NHS geography codes to names.
# phsmethods::match_area() does exactly that, so we use it directly (this is
# the faithful, auto-maintained equivalent). match_area() returns NA for the
# Scotland country code, so we special-case it, and fall back to the raw code
# for anything unmatched rather than silently dropping rows.
add_area_name <- function(df, code_col = "hbt") {
  df %>%
    mutate(
      hb_name = if_else(
        .data[[code_col]] == "S92000003",
        "Scotland",
        suppressWarnings(match_area(.data[[code_col]]))
      ),
      hb_name = coalesce(hb_name, .data[[code_col]])
    )
}

write_data <- function(df, name) {
  path <- file.path(out_dir, paste0(name, ".json"))
  write_json(df, path, dataframe = "rows", auto_unbox = TRUE, na = "null")
  message(sprintf("wrote %s (%d rows)", path, nrow(df)))
}

# --- Specialty lookup ------------------------------------------------------
specialty_lookup <- read_csv(
  get_ckan_url("6f2e3da0-b1b5-46cc-ac04-78495daedfa3"),
  show_col_types = FALSE
) %>% clean_names()

# ===========================================================================
# Waiting Profile tab  (ongoing waits / stage of treatment)
# ===========================================================================
message("Building waiting profile data ...")

raw <- read_csv(
  get_ckan_url("093f04a5-bb8f-4ce6-9016-d4fa0a912630"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(month_date = ymd(as.character(month_end))) %>%
  filter(month_date >= ymd("2019-01-01")) %>%
  add_area_name("hbt") %>%
  left_join(specialty_lookup, by = c("specialty" = "specialty")) %>%
  filter(!is.na(specialty_name)) %>%
  filter(specialty_name != "General Surgery (excl Vascular)") %>%
  mutate(
    w1 = less_than4week_wait + x4to8week_wait + x8to12week_wait +
      x12to16week_wait + x16to20week_wait + x20to24week_wait,
    w2 = x24to28week_wait + x28to32week_wait + x32to36week_wait +
      x36to40week_wait + x40to44week_wait + x44to48week_wait + x48to52week_wait,
    w3 = x52to65week_wait + x65to78week_wait,
    w4 = x78to91week_wait + x91to104week_wait + x104to117week_wait +
      x117to130week_wait + x130to143week_wait + x143to156week_wait +
      over156week_wait
  )

# Some datasets don't carry patient_type; guard against that.
if (!"patient_type" %in% names(raw)) raw$patient_type <- "All"

# Aggregate to (board, patient type, specialty, month)
by_specialty <- raw %>%
  group_by(hb_name, patient_type, spec = specialty_name, date = month_date) %>%
  summarise(across(c(w1, w2, w3, w4), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# "All" specialty roll-up so the browser never has to aggregate
all_specialties <- raw %>%
  group_by(hb_name, patient_type, date = month_date) %>%
  summarise(across(c(w1, w2, w3, w4), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(spec = "All")

waiting_profile <- bind_rows(by_specialty, all_specialties) %>%
  transmute(
    hb = hb_name,
    ptype = patient_type,
    spec,
    date = format(date, "%Y-%m-%d"),
    w1 = round(w1), w2 = round(w2), w3 = round(w3), w4 = round(w4)
  ) %>%
  arrange(hb, ptype, spec, date)

write_data(waiting_profile, "waiting_profile")

message("Done.")
