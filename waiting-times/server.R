server <- function(input, output, session) {
  # Function to filter data based on user selections
  filter_data <- function(data) {
    data %>%
      filter(hb_name == input$hb_select) %>%
      filter(patient_type == input$patient_type_select) %>%
      { if (input$specialty_select != "All") filter(., specialty_name == input$specialty_select) else . }
  }
  
  #### Reactive filtered datasets ####
  
  ##### 12 Week Waiting Times #####
  wt_filtered <- reactive({
    filter_data(wt_data) %>%
      group_by(quarter_date, quarter_label) %>%
      summarise(
        waiting_over_12 = sum(waited_over_12_weeks, na.rm = TRUE),
        total_seen = sum(number_seen, na.rm = TRUE),
        seen_within_12 = total_seen - sum(waited_over_12_weeks, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  patients_seen_filtered <- reactive({
    filter_data(wt_data) %>%
      group_by(quarter_date, quarter_label) %>%
      summarise(
        waited_over_12_weeks = sum(waited_over_12_weeks, na.rm = TRUE),
        total_seen = sum(number_seen, na.rm = TRUE),
        waited_under_12_weeks = total_seen - waited_over_12_weeks,
        .groups = "drop"
      )
  })
  
  wait_times_filtered <- reactive({
    filter_data(wt_data) %>%
      group_by(quarter_date, quarter_label) %>%
      summarise(
        median_wait = weighted.mean(median, number_seen, na.rm = TRUE),
        percentile_90 = weighted.mean(x90th_percentile, number_seen, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  ##### Balance #####
  
  balance_filtered <- reactive({
    filter_data(balance_data) %>%
      group_by(quarter_date, quarter_label) %>%
      summarise(
        additions = sum(number_added, na.rm = TRUE),
        removals = sum(number_removed, na.rm = TRUE),
        balance = additions - removals,
        attended_total = sum(attended, na.rm = TRUE),
        referred_total = sum(referred_gp, na.rm = TRUE),
        transferred_total = sum(transferred, na.rm = TRUE),
        no_treatment_total = sum(no_treatment_required, na.rm = TRUE),
        other_total = sum(other_reasons, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        attended_pct = attended_total / removals * 100,
        referred_pct = referred_total / removals * 100,
        transferred_pct = transferred_total / removals * 100,
        no_treatment_pct = no_treatment_total / removals * 100,
        other_pct = other_total / removals * 100
      )
  })
  
  ##### Waiting Distribution #####
  
  waiting_distribution_filtered <- reactive({
    filter_data(waiting_distribution) %>%
      group_by(month_date) %>%
      summarise(
        wait_0_to_26 = sum(waiting_0_to_26, na.rm = TRUE),
        wait_26_to_52 = sum(waiting_26_to_52, na.rm = TRUE),
        wait_52_to_78 = sum(waiting_52_to_78, na.rm = TRUE),
        wait_over_78 = sum(waiting_over_78, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  ##### Diagnostic Waits #####
  diagnostic_waits_filtered <- reactive({
    diagnostic_waiting_times %>%
      filter(hb_name == input$hb_select) %>%
      filter(diagnostic_test_type == input$diagnostic_test_type_select) %>%
      filter(diagnostic_test_description == input$diagnostic_test_description_select) %>%
      group_by(month_date, waiting_time_group) %>%
      summarise(
        total_waiting = sum(number_waiting, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Update the Diagnostic Description filter automatically
  # depending on which Diagnostic Type is selected
  observe({
    # Get descriptions that match the selected type
    diagnostic_filtered_descriptions <- diagnostic_waiting_times %>%
      filter(diagnostic_test_type == input$diagnostic_test_type_select) %>%
      pull(diagnostic_test_description) %>%
      unique()
    
    # Update the description dropdown
    updateSelectInput(
      session,
      "diagnostic_test_description_select",
      choices = diagnostic_filtered_descriptions
    )
  })
  
  ##### A&E Waits #####
  ae_filtered <- reactive({
    ae_data %>%
      filter(hb_name == input$hb_select) %>%
      filter(hospital_name == input$treatment_location_select) %>%
      filter(attendance_category == "All")
  })

  observe({
    locations <- ae_data %>%
      filter(hb_name == input$hb_select) %>%
      pull(hospital_name) %>%
      unique() %>%
      sort()

    updateSelectInput(session, "treatment_location_select", choices = locations)
  })

  ##### Cancer - 31 Day Standard #####
  cancer_31_day_filtered <- reactive({
    cancer_31_day_data %>%
      filter(hb_name == input$hb_select) %>%
      group_by(quarter_date, quarter) %>%
      summarise(
        total_referrals = sum(number_of_eligible_referrals31day_standard, na.rm = TRUE),
        within_standard = sum(number_of_eligible_referrals_treated_within31days, na.rm = TRUE),
        percent_within = within_standard / total_referrals * 100,
        percent_over = 100 - percent_within,
        .groups = "drop"
      )
  })
  
  ##### Cancer - 62 Day Standard #####
  cancer_62_day_filtered <- reactive({
    cancer_62_day_data %>%
      filter(hb_name == input$hb_select) %>%
      group_by(quarter_date, quarter) %>%
      summarise(
        total_referrals = sum(number_of_eligible_referrals62day_standard, na.rm = TRUE),
        within_standard = sum(number_of_eligible_referrals_treated_within62days, na.rm = TRUE),
        percent_within = within_standard / total_referrals * 100,
        percent_over = 100 - percent_within,
        .groups = "drop"
      )
  })
  
  #### Charts ####
  
  ##### Waiting Profile #####
  output$total_waiting_plot <- renderPlotly({
    plot_ly(waiting_distribution_filtered()) %>%
      add_lines(
        x = ~month_date, 
        y = ~(wait_0_to_26 + wait_26_to_52 + wait_52_to_78 + wait_over_78),
        line = list(color = '#0078D4', width = 2),
        name = "Total Patients"
      ) %>%
      layout(
        title = list(
          text = "Total Number of Patients on Waiting List",
          font = list(size = 16),
          pad = list(t = 20)  # Adds padding above the title
        ),
        margin = list(t = 50),  # Adds margin at the top of the entire plot
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Number of Patients",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          rangemode = "tozero"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x"
      )
  })
  
  output$waiting_distribution_plot <- renderPlotly({
    plot_ly(waiting_distribution_filtered()) %>%
      add_lines(
        x = ~month_date, 
        y = ~wait_0_to_26, 
        name = "0-26 weeks",
        line = list(color = '#90CAF9', width = 2)  # Light blue
      ) %>%
      add_lines(
        x = ~month_date, 
        y = ~wait_26_to_52, 
        name = "26-52 weeks",
        line = list(color = '#42A5F5', width = 2)  # Medium blue
      ) %>%
      add_lines(
        x = ~month_date, 
        y = ~wait_52_to_78, 
        name = "52-78 weeks",
        line = list(color = '#1976D2', width = 2)  # Darker blue
      ) %>%
      add_lines(
        x = ~month_date, 
        y = ~wait_over_78, 
        name = "Over 78 weeks",
        line = list(color = '#0D47A1', width = 2)  # Darkest blue
      ) %>%
      layout(
        title = list(
          text = "Breakdown of Patient Waiting Times",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Number of Patients Waiting",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.05
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  
  ##### Balance #####
  output$additions_removals_plot <- renderPlotly({
    plot_ly(balance_filtered()) %>%
      add_lines(
        x = ~quarter_date, 
        y = ~additions, 
        name = "Additions",
        line = list(color = '#FF7043', width = 2)
      ) %>%
      add_lines(
        x = ~quarter_date, 
        y = ~removals, 
        name = "Removals",
        line = list(color = '#00897B', width = 2) 
      )  %>%
      layout(
        title = list(
          text = "Waiting List Additions and Removals",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Number of Patients Waiting",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.1
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  output$balance_plot <- renderPlotly({
    plot_ly(balance_filtered()) %>%
      add_bars(
        x = ~quarter_date, 
        y = ~balance,
        marker = list(
          color = ~ifelse(balance >= 0, '#DC2626', '#059669'),  # Red for positive, green for negative
          line = list(width = 0)
        ),
        name = "Net Change"
      ) %>%
      layout(
        title = list(
          text = "Net Change in Waiting List Size",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Net Change in Number of Patients",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x"
      )
  })
  
  output$removal_reasons_plot <- renderPlotly({
    # Prepare the data
    plot_data <- balance_filtered() %>%
      mutate(total = attended_pct + referred_pct + transferred_pct + no_treatment_pct + other_pct) %>%
      pivot_longer(
        cols = c(attended_pct, referred_pct, transferred_pct, no_treatment_pct, other_pct),
        names_to = "reason",
        values_to = "percentage"
      ) %>%
      mutate(
        percentage = percentage/total * 100,
        reason = factor(reason,
                        levels = c("attended_pct", "referred_pct", "transferred_pct", "no_treatment_pct", "other_pct"),
                        labels = c("Attended", "Referred", "Transferred", "No Treatment", "Other")
        )
      )
    
    plot_ly(plot_data,
            x = ~quarter_date,
            y = ~percentage,
            color = ~reason,
            type = 'scatter',
            mode = 'none',
            stackgroup = 'one'
    ) %>%
      layout(
        title = list(
          text = "Reasons for Removal from Waiting List",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        colorway = c('#0078D4', '#42A5F5', '#90CAF9', '#1976D2', '#0D47A1'),
        margin = list(t = 50, b = 80),
        xaxis = list(
          title = "Quarter",
          tickangle = -45,
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Percentage of Removals",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          range = c(0, 100),
          ticksuffix = "%"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.2
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  ##### 12 Week Waits #####
  output$waiting_plot <- renderPlotly({
    plot_ly(wt_filtered()) %>%
      add_lines(
        x = ~quarter_date,
        y = ~seen_within_12,
        name = "Within 12 weeks",
        line = list(color = '#059669', width = 2)  # Green for target/good
      ) %>%
      add_lines(
        x = ~quarter_date,
        y = ~waiting_over_12,
        name = "Over 12 weeks",
        line = list(color = '#DC2626', width = 2)  # Red for over target
      ) %>%
      layout(
        title = list(
          text = "Number of Patients Treated by Wait Time",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.21
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  output$patients_seen_plot <- renderPlotly({
    plot_ly(patients_seen_filtered()) %>%
      add_trace(
        x = ~quarter_date,
        y = ~waited_under_12_weeks,
        name = "Within 12 weeks",
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = '#059669'  # Green for target/good
      ) %>%
      add_trace(
        x = ~quarter_date,
        y = ~waited_over_12_weeks,
        name = "Over 12 weeks",
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = '#DC2626'  # Red for over target
      ) %>%
      layout(
        title = list(
          text = "Cumulative Number of Patients Treated",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.21
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  output$percentage_plot <- renderPlotly({
    plot_ly(patients_seen_filtered()) %>%
      add_trace(
        x = ~quarter_date, 
        y = ~(waited_under_12_weeks/total_seen*100), 
        name = "Within 12 weeks", 
        type = 'scatter', 
        mode = 'none', 
        stackgroup = 'one',
        fillcolor = '#059669'  # Green for target/good
      ) %>%
      add_trace(
        x = ~quarter_date, 
        y = ~(waited_over_12_weeks/total_seen*100), 
        name = "Over 12 weeks", 
        type = 'scatter', 
        mode = 'none', 
        stackgroup = 'one',
        fillcolor = '#DC2626'  # Red for over target
      ) %>%
      layout(
        title = list(
          text = "Percentage of Patients Treated by Treatment Wait Time",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          range = c(0, 100),
          ticksuffix = "%"  # Add percentage symbol to y-axis values
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.1
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  output$wait_times_plot <- renderPlotly({
    plot_ly(wait_times_filtered()) %>%
      add_lines(
        x = ~quarter_date, 
        y = ~median_wait, 
        name = "Median",
        line = list(color = '#0078D4', width = 2)
      ) %>%
      add_lines(
        x = ~quarter_date, 
        y = ~percentile_90, 
        name = "90th percentile",
        line = list(color = '#FF6B6B', width = 2)
      ) %>%
      layout(
        title = list(
          text = "Breakdown of Wait Times",
          font = list(size = 16),
          pad = list(t = 20)  # Adds padding above the title
        ),
        margin = list(t = 50),  # Adds margin at the top of the entire plot
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Days Waited",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.1
        ),
        showlegend = TRUE,
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x"
      )
  })
  
  ##### Diagnostic Waits ####
  
  # Diagnostic Waits Plot
  output$diagnostic_waits_plot <- renderPlotly({
    plot_ly(diagnostic_waits_filtered(), 
            x = ~month_date, 
            y = ~total_waiting, 
            color = ~waiting_time_group, 
            type = 'scatter', 
            mode = 'none', 
            stackgroup = 'one'
    ) %>%
      layout(
        title = list(text = "Diagnostic Waiting Patients Over Time",
                     font = list(size = 16),
                     pad = list(t = 20)),
        colorway = c('#90CAF9', '#42A5F5', '#1976D2', '#0D47A1', '#1A237E'),
        margin = list(t = 50, b = 80),
        xaxis = list(title = "", 
                     tickangle = -45,
                     gridcolor = 'rgba(220, 220, 220, 0.4)',
                     showgrid = TRUE),
        yaxis = list(title = "Number of Patients Waiting",
                     gridcolor = 'rgba(220, 220, 220, 0.4)',
                     showgrid = TRUE,
                     rangemode = "tozero"),
        legend = list(title = list(text = "Waiting Time Category"),
                      orientation = "h",
                      xanchor = "center",
                      x = 0.5,
                      y = -0.3),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  ##### A&E Waits #####
  output$ae_attendances_plot <- renderPlotly({
    plot_ly(ae_filtered()) %>%
      add_lines(
        x = ~week_ending,
        y = ~total_attendances,
        line = list(color = '#0078D4', width = 2),
        name = "Total Attendances"
      ) %>%
      layout(
        title = list(text = "Total A&E Attendances", font = list(size = 16), pad = list(t = 20)),
        margin = list(t = 50),
        xaxis = list(title = "", gridcolor = 'rgba(220, 220, 220, 0.4)', showgrid = TRUE),
        yaxis = list(title = "Number of Attendances", gridcolor = 'rgba(220, 220, 220, 0.4)', showgrid = TRUE, rangemode = "tozero"),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x"
      )
  })

  output$ae_waits_plot <- renderPlotly({
    plot_ly(ae_filtered()) %>%
      add_trace(
        x = ~week_ending, y = ~pct_within_4_hours,
        name = "Within 4 hours",
        type = 'scatter', mode = 'none', stackgroup = 'one',
        fillcolor = '#059669'
      ) %>%
      add_trace(
        x = ~week_ending, y = ~pct_4_to_8_hours,
        name = "4-8 hours",
        type = 'scatter', mode = 'none', stackgroup = 'one',
        fillcolor = '#F59E0B'
      ) %>%
      add_trace(
        x = ~week_ending, y = ~pct_8_to_12_hours,
        name = "8-12 hours",
        type = 'scatter', mode = 'none', stackgroup = 'one',
        fillcolor = '#F97316'
      ) %>%
      add_trace(
        x = ~week_ending, y = ~pct_over_12_hours,
        name = "Over 12 hours",
        type = 'scatter', mode = 'none', stackgroup = 'one',
        fillcolor = '#DC2626'
      ) %>%
      layout(
        title = list(text = "A&E Waiting Time Breakdown", font = list(size = 16), pad = list(t = 20)),
        margin = list(t = 50),
        xaxis = list(title = "", gridcolor = 'rgba(220, 220, 220, 0.4)', showgrid = TRUE),
        yaxis = list(
          title = "Percentage of Attendances",
          gridcolor = 'rgba(220, 220, 220, 0.4)', showgrid = TRUE,
          range = c(0, 100), ticksuffix = "%"
        ),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })

  ##### Cancer - 31 Day Standard #####
  # Add to your outputs section
  output$cancer_31_day_plot <- renderPlotly({
    plot_ly(cancer_31_day_filtered()) %>%
      add_trace(
        x = ~quarter_date,
        y = ~percent_within,
        name = "Within 31 days",
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = '#059669'  # Green for meeting standard
      ) %>%
      add_trace(
        x = ~quarter_date,
        y = ~percent_over,
        name = "Over 31 days",
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = '#DC2626'  # Red for over standard
      ) %>%
      layout(
        title = list(
          text = "31-Day Cancer Treatment Standard",
          subtitle = "Time from decision to treat to first cancer treatment",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Percentage of Referrals",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          range = c(0, 100),
          ticksuffix = "%"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.1
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
  ##### Cancer - 62 Day Standard #####
  output$cancer_62_day_plot <- renderPlotly({
    plot_ly(cancer_62_day_filtered()) %>%
      add_trace(
        x = ~quarter_date,
        y = ~percent_within,
        name = "Within 62 days",
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = '#059669'  # Green for meeting standard
      ) %>%
      add_trace(
        x = ~quarter_date,
        y = ~percent_over,
        name = "Over 62 days",
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = '#DC2626'  # Red for over standard
      ) %>%
      layout(
        title = list(
          text = "62-Day Cancer Treatment Standard",
          subtitle = "Time from urgent referral to first cancer treatment",
          font = list(size = 16),
          pad = list(t = 20)
        ),
        margin = list(t = 50),
        xaxis = list(
          title = "",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE
        ),
        yaxis = list(
          title = "Percentage of Referrals",
          gridcolor = 'rgba(220, 220, 220, 0.4)',
          showgrid = TRUE,
          range = c(0, 100),
          ticksuffix = "%"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = -0.1
        ),
        hoverlabel = list(bgcolor = "white"),
        hovermode = "x unified"
      )
  })
  
}