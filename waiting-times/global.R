# global.R
library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)
library(phsmethods)

# Function to load specialty lookup
load_specialty_lookup <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/688c7ea0-4845-4b03-9df0-4149c72cb7f0/resource/6f2e3da0-b1b5-46cc-ac04-78495daedfa3/download/specialty_codes.csv"
  
  df <- read_csv(url) %>%
    clean_names()
  
  return(df)
}

load_waiting_times_data <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/e9dbef36-a343-4b9a-ab7e-b6e6cbcbb38e/resource/4c091d26-1492-41e5-9577-832cbc1cd4cf/download/sot_performance_completed_waits_sep24.csv"
  
  df <- read_csv(url) %>%
    clean_names() %>%
    rename(waited_over_12_weeks = waited_over12weeks) %>%
    select(hbt, quarter_ending, patient_type, specialty, number_seen, waited_over_12_weeks, median, x90th_percentile) %>%
    mutate(
      quarter_date = ymd(as.character(quarter_ending)),
      quarter_label = paste0(year(quarter_date), " Q", quarter(quarter_date))
    ) %>%
    filter(quarter_date >= ymd("2019-01-01")) %>%  # Add this line
    filter(!is.na(number_seen)) %>%
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name) %>%
    left_join(specialty_lookup, by = c("specialty" = "specialty"))
  
  return(df)
}

# In load_balance_data(), add after the initial data processing:
load_balance_data <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/e9dbef36-a343-4b9a-ab7e-b6e6cbcbb38e/resource/10dd6ca4-1868-464c-8d20-7f9261070484/download/sot_removal_reasons_sep24.csv"
  
  df <- read_csv(url) %>%
    clean_names() %>%
    rename(
      number_added = additions,
      number_removed = removals,
      attended = attended,
      referred_gp = referred_back_to_gp,
      transferred = transferred,
      no_treatment_required = treatment_no_longer_required,
      other_reasons = other_reasons
    ) %>%
    mutate(
      quarter_date = ymd(as.character(quarter_ending)),
      quarter_label = paste0("Q", quarter(quarter_date), " ", year(quarter_date))
    ) %>%
    filter(quarter_date >= ymd("2019-01-01")) %>%  # Add this line
    mutate(
      attended_pct = attended / number_removed * 100,
      referred_pct = referred_gp / number_removed * 100,
      transferred_pct = transferred / number_removed * 100,
      no_treatment_pct = no_treatment_required / number_removed * 100,
      other_pct = other_reasons / number_removed * 100
    ) %>%
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name) %>%
    left_join(specialty_lookup, by = c("specialty" = "specialty"))
  
  return(df)
}

# In load_waiting_distribution(), add after the initial data processing:
load_waiting_distribution <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/e9dbef36-a343-4b9a-ab7e-b6e6cbcbb38e/resource/093f04a5-bb8f-4ce6-9016-d4fa0a912630/download/sot_distribution_of_ongoing_waits_sep24.csv"
  
  df <- read_csv(url) %>%
    clean_names() %>%
    mutate(month_date = ymd(as.character(month_end))) %>%
    filter(month_date >= ymd("2019-01-01")) %>%  # Add this line
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name) %>%
    left_join(specialty_lookup, by = c("specialty" = "specialty")) %>%
    mutate(
      waiting_0_to_26 = less_than4week_wait + x4to8week_wait + x8to12week_wait + 
        x12to16week_wait + x16to20week_wait + x20to24week_wait,
      waiting_26_to_52 = x24to28week_wait + x28to32week_wait + x32to36week_wait + 
        x36to40week_wait + x40to44week_wait + x44to48week_wait + x48to52week_wait,
      waiting_52_to_78 = x52to65week_wait + x65to78week_wait,
      waiting_over_78 = x78to91week_wait + x91to104week_wait + x104to117week_wait + 
        x117to130week_wait + x130to143week_wait + x143to156week_wait + over156week_wait,
      total_waiting = waiting_0_to_26 + waiting_26_to_52 + waiting_52_to_78 + waiting_over_78
    )
}

load_diagnostic_waiting_times <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/10dfe6f3-32de-4039-84c2-7e7794a06b31/download/diagnostics_by_board_september_2024.csv"
  
  # First get board level data
  board_level <- read_csv(url) %>%
    clean_names() %>%
    rename(hbt = hbt, waiting_time_category = waiting_time, number_waiting = number_on_list) %>%
    mutate(
      month_date = ymd(as.character(month_ending)),
      month_label = paste0(year(month_date), " ", month(month_date, label = TRUE)),
      waiting_time_group = case_when(
        waiting_time_category %in% c("0-7 days", "8-14 days", "15-21 days", "22-28 days") ~ "0 to 4 weeks",
        waiting_time_category %in% c("29-35 days", "36-42 days", "43-49 days", "50-56 days") ~ "4 to 8 weeks",
        waiting_time_category %in% c("57-63 days", "64-70 days", "71-77 days", "78-84 days", "85-91 days", "92-182 days") ~ "8 to 18 weeks",
        waiting_time_category %in% c("183-273 days", "274-364 days") ~ "4 to 12 months",
        waiting_time_category == "365 days and over" ~ "Over 1 year",
        TRUE ~ "Other"
      )
    ) %>%
    filter(month_date >= ymd("2020-10-01")) %>%
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name)
  
  # Create Scotland level data
  scotland_level <- board_level %>%
    group_by(month_date, month_ending, month_label,
             diagnostic_test_type, diagnostic_test_description,
             waiting_time_category, waiting_time_group) %>%
    summarise(
      number_waiting = sum(number_waiting, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      hbt = "S92000003",
      hb_name = "Scotland",
      number_on_list_qf = "d"  # Maintaining the same structure as board level data
    )
  
  # Combine board and Scotland level data
  combined_data <- bind_rows(board_level, scotland_level)
  
  return(combined_data)
}

load_cancer_31day_data <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/11c61a02-205b-43f6-9297-243679103617/resource/58527343-a930-4058-bf9e-3c6e5cb04010/download/cwt_31_day_standard.csv"
  
  # First get the HBT level data
  hbt_level <- read_csv(url) %>%
    clean_names() %>%
    # First group and sum by health board of treatment, cancer type and quarter
    group_by(hbt, cancer_type, quarter) %>%
    summarise(
      number_of_eligible_referrals31day_standard = sum(number_of_eligible_referrals31day_standard, na.rm = TRUE),
      number_of_eligible_referrals_treated_within31days = sum(number_of_eligible_referrals_treated_within31days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Then create date and percentage calculations
    mutate(
      quarter_date = ymd(paste0(str_sub(quarter, 1, 4), 
                                case_when(
                                  str_sub(quarter, -2) == "Q1" ~ "-03-31",
                                  str_sub(quarter, -2) == "Q2" ~ "-06-30",
                                  str_sub(quarter, -2) == "Q3" ~ "-09-30",
                                  str_sub(quarter, -2) == "Q4" ~ "-12-31"
                                ))),
      percent_within_standard = (number_of_eligible_referrals_treated_within31days / 
                                   number_of_eligible_referrals31day_standard * 100)
    ) %>%
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name) %>%
    filter(!is.na(hb_name))
  
  # Create Scotland level data
  scotland_level <- hbt_level %>%
    group_by(cancer_type, quarter, quarter_date) %>%
    summarise(
      number_of_eligible_referrals31day_standard = sum(number_of_eligible_referrals31day_standard, na.rm = TRUE),
      number_of_eligible_referrals_treated_within31days = sum(number_of_eligible_referrals_treated_within31days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      hbt = "S92000003",  # Scotland's geography code
      hb_name = "Scotland",
      percent_within_standard = (number_of_eligible_referrals_treated_within31days / 
                                   number_of_eligible_referrals31day_standard * 100)
    )
  
  # Combine HBT and Scotland level data
  combined_data <- bind_rows(hbt_level, scotland_level)
  
  return(combined_data)
}

load_cancer_62day_data <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/11c61a02-205b-43f6-9297-243679103617/resource/23b3bbf7-7a37-4f86-974b-6360d6748e08/download/cwt_62_day_standard.csv"
  
  # First get the HB level data
  hb_level <- read_csv(url) %>%
    clean_names() %>%
    # First group and sum by health board of residence, cancer type and quarter
    group_by(hb, cancer_type, quarter) %>%
    summarise(
      number_of_eligible_referrals62day_standard = sum(number_of_eligible_referrals62day_standard, na.rm = TRUE),
      number_of_eligible_referrals_treated_within62days = sum(number_of_eligible_referrals_treated_within62days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Then create date and percentage calculations
    mutate(
      quarter_date = ymd(paste0(str_sub(quarter, 1, 4), 
                                case_when(
                                  str_sub(quarter, -2) == "Q1" ~ "-03-31",
                                  str_sub(quarter, -2) == "Q2" ~ "-06-30",
                                  str_sub(quarter, -2) == "Q3" ~ "-09-30",
                                  str_sub(quarter, -2) == "Q4" ~ "-12-31"
                                ))),
      percent_within_standard = (number_of_eligible_referrals_treated_within62days / 
                                   number_of_eligible_referrals62day_standard * 100)
    ) %>%
    left_join(area_lookup, by = c("hb" = "geo_code")) %>%
    rename(hb_name = area_name) %>%
    filter(!is.na(hb_name))
  
  # Create Scotland level data
  scotland_level <- hb_level %>%
    group_by(cancer_type, quarter, quarter_date) %>%
    summarise(
      number_of_eligible_referrals62day_standard = sum(number_of_eligible_referrals62day_standard, na.rm = TRUE),
      number_of_eligible_referrals_treated_within62days = sum(number_of_eligible_referrals_treated_within62days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      hb = "S92000003",  # Scotland's geography code
      hb_name = "Scotland",
      percent_within_standard = (number_of_eligible_referrals_treated_within62days / 
                                   number_of_eligible_referrals62day_standard * 100)
    )
  
  # Combine HBT and Scotland level data
  combined_data <- bind_rows(hb_level, scotland_level)
  
  return(combined_data)
}


load_ae_waiting_times <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/0d57311a-db66-4eaa-bd6d-cc622b6cbdfa/resource/a5f7ca94-c810-41b5-a7c9-25c18d43e5a4/download/weekly_ae_activity_20250209.csv"
  
  # First get board level data
  board_level <- read_csv(url) %>%
    clean_names() %>%
    mutate(
      week_ending = ymd(as.character(week_ending_date)),
      total_attendances = number_of_attendances_episode,
      pct_within_4_hours = (number_within4hours_episode / total_attendances * 100),
      pct_4_to_8_hours = ((number_over4hours_episode - number_over8hours_episode) / total_attendances * 100),
      pct_8_to_12_hours = ((number_over8hours_episode - number_over12hours_episode) / total_attendances * 100),
      pct_over_12_hours = (number_over12hours_episode / total_attendances * 100)
    ) %>%
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name)
  
  # Create Scotland level data
  scotland_level <- board_level %>%
    group_by(week_ending, week_ending_date) %>%
    summarise(
      number_of_attendances_episode = sum(number_of_attendances_episode, na.rm = TRUE),
      number_within4hours_episode = sum(number_within4hours_episode, na.rm = TRUE),
      number_over4hours_episode = sum(number_over4hours_episode, na.rm = TRUE),
      number_over8hours_episode = sum(number_over8hours_episode, na.rm = TRUE),
      number_over12hours_episode = sum(number_over12hours_episode, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      hbt = "S92000003",  # Scotland's geography code
      hb_name = "Scotland",
      total_attendances = number_of_attendances_episode,
      pct_within_4_hours = (number_within4hours_episode / total_attendances * 100),
      pct_4_to_8_hours = ((number_over4hours_episode - number_over8hours_episode) / total_attendances * 100),
      pct_8_to_12_hours = ((number_over8hours_episode - number_over12hours_episode) / total_attendances * 100),
      pct_over_12_hours = (number_over12hours_episode / total_attendances * 100)
    )
  
  # Combine board and Scotland level data
  combined_data <- bind_rows(board_level, scotland_level)
  
  return(combined_data)
}


# Load the lookups and data globally
specialty_lookup <- load_specialty_lookup()
wt_data <- load_waiting_times_data() %>%
  filter(!is.na(specialty_name)) %>%
  filter(specialty_name != "General Surgery (excl Vascular)")

balance_data <- load_balance_data() %>%
  filter(!is.na(specialty_name)) %>%
  filter(specialty_name != "General Surgery (excl Vascular)")

waiting_distribution <- load_waiting_distribution() %>%
  filter(!is.na(specialty_name)) %>%
  filter(specialty_name != "General Surgery (excl Vascular)")

diagnostic_waiting_times <- load_diagnostic_waiting_times()

cancer_31_day_data <- load_cancer_31day_data()

cancer_62_day_data <- load_cancer_62day_data()

ae_data <- load_ae_waiting_times()

