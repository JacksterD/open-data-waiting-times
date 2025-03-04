load_diagnostic_waiting_times <- function() {
  url <- "https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/10dfe6f3-32de-4039-84c2-7e7794a06b31/download/diagnostics_by_board_september_2024.csv"
  
  df <- read_csv(url) %>%
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
    filter(month_date >= ymd("2019-01-01")) %>%  # Keep data from 2019 onwards
    left_join(area_lookup, by = c("hbt" = "geo_code")) %>%
    rename(hb_name = area_name)
  
  return(df)
}

# Load the new dataset globally
diagnostic_waiting_times <- load_diagnostic_waiting_times()

# UI Update: Fix Conditional Panels for Filters
ui <- fluidPage(
  titlePanel("NHS Scotland Waiting Times"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "hb_select", 
        label = "Health Board",
        choices = unique(balance_data$hb_name)
      ),
      
      conditionalPanel(
        condition = "input.tabs !== '🩺 Diagnostic Waits' && input.tabs !== '🧬 Cancer Waiting Times'",
        selectInput(
          inputId = "patient_type_select",
          label = "Patient Type",
          choices = unique(wt_data$patient_type)
        ),
        selectInput(
          inputId = "specialty_select",
          label = "Specialty",
          choices = c("All", sort(unique(wt_data$specialty_name)))
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs === '🩺 Diagnostic Waits'",
        selectInput(
          inputId = "diagnostic_test_type_select",
          label = "Diagnostic Test Type",
          choices = unique(diagnostic_waiting_times$diagnostic_test_type)
        ),
        selectInput(
          inputId = "diagnostic_test_description_select",
          label = "Diagnostic Test Description",
          choices = unique(diagnostic_waiting_times$diagnostic_test_description)
        )
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("⏳ Waiting Profile",
                 plotlyOutput("total_waiting_plot", height = "420px"),
                 plotlyOutput("waiting_distribution_plot", height = "420px")
        ),
        tabPanel("🧘 Balance",
                 plotlyOutput("additions_removals_plot", height = "420px"),
                 plotlyOutput("balance_plot", height = "420px"),
                 plotlyOutput("removal_reasons_plot", height = "420px")
        ),
        tabPanel("📊 12-week Target Performance",
                 plotlyOutput("waiting_plot", height = "420px"),
                 plotlyOutput("patients_seen_plot", height = "420px"),
                 plotlyOutput("percentage_plot", height = "420px"),
                 plotlyOutput("wait_times_plot", height = "420px")
        ),
        tabPanel("🩺 Diagnostic Waits",
                 plotlyOutput("diagnostic_waits_plot", height = "600px")
        ),
        tabPanel("🧬 Cancer Waiting Times",
                 
                 h3("31-Day Standard Performance"),
                 p("Time from decision to treat to first cancer treatment"),
                 plotlyOutput("cancer_31_day_plot", height = "480px"),
                 
                 h3("62-Day Standard Performance"),
                 p("Time from urgent referral with suspicion of cancer to first cancer treatment"),
                 plotlyOutput("cancer_62_day_plot", height = "480px")
        ),
        tabPanel("🚑 A&E Waits"
        )
      ),
      width = 9
    )
  )
)
