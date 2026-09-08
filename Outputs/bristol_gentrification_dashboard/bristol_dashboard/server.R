# =============================================================================
# server.R
# Bristol Gentrification Index — Shiny Dashboard Server Logic
# =============================================================================

server <- function(input, output, session) {

  # ---- TAB 1: WARD MAP -------------------------------------------------------

  output$ward_map <- renderLeaflet({

    map_data <- wards_geo %>%
      left_join(snapshot_data, by = c("NAME" = "ward_name"))

    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$gentrification_index,
      na.color = "#d9d9d9"
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>Gentrification Index: %s<br/>Avg Price: %s<br/>%% Highly Educated: %s",
      map_data$NAME,
      ifelse(is.na(map_data$gentrification_index), "No data", as.character(map_data$gentrification_index)),
      ifelse(is.na(map_data$avg_price), "No data", scales::dollar(map_data$avg_price, prefix = "£")),
      ifelse(is.na(map_data$pc_highly_educated), "No data", paste0(round(map_data$pc_highly_educated, 1), "%"))
    ) %>% lapply(HTML)

    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(gentrification_index),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.75,
        label = labels,
        highlightOptions = highlightOptions(weight = 3, color = "#333333", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal, values = ~gentrification_index, opacity = 0.75,
        title = "Gentrification<br>Index", position = "bottomright"
      )
  })

  observeEvent(input$map_ward_select, {
    if (input$map_ward_select != "None") {
      target <- wards_geo %>% filter(NAME == input$map_ward_select)
      if (nrow(target) > 0) {
        bbox <- st_bbox(target)
        leafletProxy("ward_map") %>%
          flyToBounds(
            lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])
          )
      }
    }
  })

  # ---- TAB 2: HISTORICAL TRENDS ----------------------------------------------

  output$ward_trend_plot <- renderPlotly({

    ward_hist <- ts_data %>% filter(ward_name == input$trend_ward_select)

    p <- ggplot() +
      geom_line(data = ward_hist, aes(x = sale_year, y = avg_price), color = "#0343df", linewidth = 1) +
      geom_point(data = ward_hist, aes(x = sale_year, y = avg_price), color = "#0343df", size = 2)

    if (isTRUE(input$show_uk_avg)) {
      p <- p + geom_line(
        data = uk_national_benchmark, aes(x = sale_year, y = avg_price),
        color = "#e74c3c", linetype = "dashed", linewidth = 1
      )
    }

    if (isTRUE(input$show_bau)) {
      bau <- project_bau(input$trend_ward_select)
      if (!is.null(bau)) {
        bau_future <- bau %>% filter(type == "Business-As-Usual Projection")
        p <- p + geom_line(
          data = bau_future, aes(x = sale_year, y = avg_price),
          color = "#0343df", linetype = "dotted", linewidth = 1
        )
      }
    }

    p <- p +
      scale_y_continuous(labels = scales::label_dollar(prefix = "£")) +
      labs(
        title = paste0(input$trend_ward_select, " — Average House Price (2016-2024)"),
        subtitle = if (isTRUE(input$show_uk_avg)) "Dashed red line = UK National Average" else NULL,
        x = "Year", y = "Average Sale Price (£)"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  output$leaderboard_plot <- renderPlotly({

    leaderboard <- ts_data %>%
      filter(sale_year %in% c(2016, 2024)) %>%
      group_by(ward_name) %>%
      summarise(
        price_2016 = mean(avg_price[sale_year == 2016]),
        price_2024 = mean(avg_price[sale_year == 2024]),
        pct_growth = ((price_2024 - price_2016) / price_2016) * 100,
        .groups = "drop"
      ) %>%
      filter(!is.na(pct_growth)) %>%
      arrange(desc(pct_growth))

    p <- ggplot(
      leaderboard,
      aes(
        x = reorder(ward_name, pct_growth), y = pct_growth, fill = pct_growth,
        text = paste0(ward_name, ": ", round(pct_growth, 1), "%")
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#7bc8f6", high = "#0343df") +
      labs(title = "Total % House Price Growth (2016-2024)", x = NULL, y = "Percentage Growth (%)") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text")
  })

  # ---- TAB 3: MODEL & INDEX ---------------------------------------------------

  output$coef_plot <- renderPlot({

    coef_df <- data.frame(summary(model_linear)$coefficients)
    coef_df$Feature <- rownames(coef_df)

    coef_plot_data <- coef_df %>%
      filter(Feature != "(Intercept)") %>%
      mutate(
        Feature = case_when(
          Feature == "pc_highly_educated" ~ "% Highly Educated (L4+)",
          Feature == "pc_knowledge_jobs"   ~ "% Knowledge Sector Jobs",
          Feature == "total_crimes"        ~ "Total Annual Crimes",
          Feature == "total_schools"       ~ "Total Schools",
          TRUE ~ Feature
        )
      )

    ggplot(coef_plot_data, aes(x = reorder(Feature, Estimate), y = Estimate, fill = Estimate > 0)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0("£", format(round(Estimate, 2), big.mark = ","))),
        hjust = ifelse(coef_plot_data$Estimate > 0, -0.15, 1.15), size = 3.5, fontface = "bold"
      ) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
      scale_y_continuous(expand = expansion(mult = c(0.25, 0.25))) +
      labs(title = "Impact on Property Valuation (£)", x = NULL, y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$model_summary_text <- renderPrint({
    preds <- predict(model_linear)
    actuals <- snapshot_data$avg_price
    cat("R-squared:          ", round(summary(model_linear)$r.squared, 4), "\n")
    cat("Adjusted R-squared: ", round(summary(model_linear)$adj.r.squared, 4), "\n")
    cat("RMSE: £", format(round(sqrt(mean((actuals - preds)^2)), 2), big.mark = ","), "\n")
    cat("MAE:  £", format(round(mean(abs(actuals - preds)), 2), big.mark = ","), "\n")
  })

  output$index_plot <- renderPlot({
    ggplot(
      snapshot_data,
      aes(x = reorder(ward_name, gentrification_index), y = gentrification_index, fill = gentrification_index)
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#ffeaa7", high = "#d63031") +
      labs(title = "Ward Gentrification Risk Score", x = NULL, y = "Index (0-100)") +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # ---- TAB 4: SCENARIO SIMULATOR -----------------------------------------------

  sim_result <- eventReactive(input$run_sim, {
    simulate_ward_scenario(
      target_ward     = input$sim_ward_select,
      pct_edu_shift   = input$sim_edu_shift,
      pct_crime_shift = input$sim_crime_shift,
      pct_jobs_shift  = input$sim_jobs_shift
    )
  }, ignoreNULL = FALSE)

  output$sim_result_box <- renderUI({

    res <- sim_result()
    if (is.null(res)) {
      base <- snapshot_data %>% filter(ward_name == input$sim_ward_select)
      res <- list(
        ward = input$sim_ward_select,
        original_predicted = round(base$predicted_price, 2),
        scenario_predicted = round(base$predicted_price, 2),
        pound_difference = 0,
        pct_change = 0
      )
    }

    change_color <- if (res$pound_difference >= 0) "#2ecc71" else "#e74c3c"

    div(
      style = "background:#f8f9fa; padding:20px; border-radius:8px; margin-bottom:20px;",
      h3(res$ward),
      fluidRow(
        column(4, tags$p(strong("Baseline predicted price")),
               tags$h4(scales::dollar(res$original_predicted, prefix = "£"))),
        column(4, tags$p(strong("Scenario predicted price")),
               tags$h4(scales::dollar(res$scenario_predicted, prefix = "£"))),
        column(4, tags$p(strong("Change")),
               tags$h4(
                 style = paste0("color:", change_color, ";"),
                 paste0(scales::dollar(res$pound_difference, prefix = "£"), " (", res$pct_change, "%)")
               ))
      )
    )
  })

  output$sim_bar_plot <- renderPlot({

    res <- sim_result()
    base <- snapshot_data %>% filter(ward_name == input$sim_ward_select)
    original <- if (!is.null(res)) res$original_predicted else round(base$predicted_price, 2)
    scenario <- if (!is.null(res)) res$scenario_predicted else round(base$predicted_price, 2)

    plot_df <- data.frame(
      Scenario = factor(c("Current Baseline", "Simulated Scenario"),
                         levels = c("Current Baseline", "Simulated Scenario")),
      Price = c(original, scenario)
    )

    ggplot(plot_df, aes(x = Scenario, y = Price, fill = Scenario)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = scales::dollar(Price, prefix = "£")), vjust = -0.5, fontface = "bold") +
      scale_fill_manual(values = c("Current Baseline" = "#74b9ff", "Simulated Scenario" = "#0343df")) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "£"), expand = expansion(mult = c(0, 0.15))) +
      labs(title = paste("Predicted Price:", input$sim_ward_select), x = NULL, y = "Predicted Average Price (£)") +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # ---- TAB 5: DATA TABLE ----------------------------------------------------

  output$ward_data_table <- renderDT({
    snapshot_data %>%
      select(ward_name, avg_price, pc_highly_educated, pc_knowledge_jobs, total_schools, total_crimes,
             predicted_price, gentrification_index) %>%
      arrange(desc(gentrification_index)) %>%
      rename(
        Ward = ward_name,
        `Avg Price (£)` = avg_price,
        `% Highly Educated` = pc_highly_educated,
        `% Knowledge Jobs` = pc_knowledge_jobs,
        `Total Schools` = total_schools,
        `Total Crimes` = total_crimes,
        `Model Predicted Price (£)` = predicted_price,
        `Gentrification Index` = gentrification_index
      ) %>%
      datatable(options = list(pageLength = 15), rownames = FALSE) %>%
      formatCurrency(c("Avg Price (£)", "Model Predicted Price (£)"), currency = "£", digits = 0) %>%
      formatRound(c("% Highly Educated", "% Knowledge Jobs"), 1)
  })
}
