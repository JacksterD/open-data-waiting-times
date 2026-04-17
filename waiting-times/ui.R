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
        condition = "input.tabs !== '🩺 Diagnostic Waits' && input.tabs !== '🧬 Cancer Waiting Times' && input.tabs !== '🚑 A&E Waits'",
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

      conditionalPanel(
        condition = "input.tabs === '🚑 A&E Waits'",
        selectInput(
          inputId = "treatment_location_select",
          label = "Treatment Location",
          choices = sort(unique(ae_data$treatment_location))
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
        tabPanel("🚑 A&E Waits",
                 plotlyOutput("ae_attendances_plot", height = "420px"),
                 plotlyOutput("ae_waits_plot", height = "420px")
        )
      ),
      width = 9
    )
  )
)
