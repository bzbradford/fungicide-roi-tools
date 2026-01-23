# CORN MODULE

# UI function for crop analysis module
corn_ui <- function(id = "corn", programs_df = corn_programs) {
  ns <- NS(id)

  layout_sidebar(
    # --- Sidebar with inputs ---
    sidebar = sidebar(
      # title = "Analysis Parameters",
      title = NULL,
      width = 380,

      # Crop characteristics section
      h5(
        style = "border-bottom: 2px solid var(--bs-secondary, #6c757d);",
        # bsicons::bs_icon("graph-up"),
        "Crop Characteristics"
      ),

      # Disease severity selection
      enhanced_radio_buttons(
        inputId = ns("ds_preset"),
        label = "Disease Severity",
        choices = OPTS$corn_ds_choices,
        inline = TRUE,
        info = "Expected yield loss absent any fungicide program"
      ),

      # Custom disease severity slider (conditional)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'custom'", "ds_preset"),
        ns = ns,
        enhanced_slider_input(
          inputId = ns("ds_slider"),
          label = "Custom Disease Severity:",
          post = "%",
          info = "Expected yield loss absent any fungicide program",
          OPTS$corn_ds_slider
        )
      ),

      # Yield input
      enhanced_numeric_input(
        inputId = ns("yield"),
        label = paste("Expected Yield (bu/ac)"),
        info = "Typical yield, absent any disease pressure",
        OPTS$corn_yield
      ),

      # Price input
      enhanced_numeric_input(
        inputId = ns("price"),
        label = sprintf("Sale Price ($/bushel)"),
        info = "Expected sale price of harvested grain",
        OPTS$corn_price
      ),

      # Treatment costs section
      h5(
        style = "border-bottom: 2px solid var(--bs-secondary, #6c757d);",
        # bsicons::bs_icon("currency-dollar"),
        " Treatment Costs ($/acre)"
      ),

      enhanced_numeric_input(
        inputId = ns("appl_cost"),
        label = paste("Base application cost"),
        info = "Cost per acre to apply, added to all costs",
        OPTS$corn_appl_cost
      ),

      # Cost inputs with grid layout
      div(
        style = "
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
          align-items: end;
          gap: 0.5rem;
          font-size: small;
        ",
        id = ns("cost_grid"),
        lapply(seq_len(nrow(programs_df)), function(i) {
          program <- slice(programs_df, i)
          enhanced_numeric_input(
            inputId = ns(paste0("cost_", program$program_id)),
            label = sprintf(
              "%s (%s)",
              program$program_name,
              program$application_rate
            ),
            value = program$default_cost,
            min = 0,
            max = 500,
            step = 0.01
          )
        })
      )
    ),

    # --- Main content area ---
    navset_card_tab(
      id = ns("results_tabs"),
      full_screen = TRUE,

      # Chart tab
      nav_panel(
        title = "Chart 1",
        icon = bsicons::bs_icon("bar-chart-fill"),
        plotlyOutput(ns("plot"), height = "500px")
      ),

      nav_panel(
        title = "Chart 2",
        icon = bsicons::bs_icon("bar-chart-fill"),
        plotlyOutput(ns("plot2"), height = "500px")
      ),

      nav_panel(
        title = "Chart 3",
        icon = bsicons::bs_icon("bar-chart-fill"),
        plotlyOutput(ns("plot3"), height = "500px")
      ),

      # Table tab
      nav_panel(
        title = "Data Table",
        icon = bsicons::bs_icon("table"),
        DT::DTOutput(ns("table"))
      ),

      # Methodology tab
      nav_panel(
        title = "Methodology",
        icon = bsicons::bs_icon("info-circle"),
        card_body(
          h4("Economic Analysis Methodology"),
          p(
            "This calculator estimates the expected net benefit of fungicide applications based on field trial data and economic modeling."
          ),
          h5("Key Metrics"),
          tags$dl(
            tags$dt("Expected Net Benefit"),
            tags$dd(
              "Estimated profit or loss: (yield × price × yield_benefit) − treatment_cost"
            ),
            tags$dt("95% Confidence Interval"),
            tags$dd("Range of plausible values based on model uncertainty."),
            tags$dt("Breakeven Probability"),
            tags$dd(
              "Probability that applying the fungicide yields positive net benefit."
            ),
            tags$dt("Breakeven Cost"),
            tags$dd("Maximum spend that still expects to break even.")
          ),
          h5("Assumptions"),
          tags$ul(
            tags$li(
              "Yield response based on linear regression of field trial data"
            ),
            tags$li("Disease severity affects yield loss proportionally"),
            tags$li("Base application cost is the same for all treatments"),
          ),
          p(
            class = "text-muted",
            "Note: Estimates are based on historical trial data. Actual results may vary."
          )
        )
      )
    )
  )
}

#' Server function for crop analysis module
#'
#' @param id Module namespace ID
#' @param programs_df Data frame of programs from {crop}_programs.csv
corn_server <- function(id = "corn", programs_df = corn_programs) {
  moduleServer(id, function(input, output, session) {
    program_ids <- programs_df$program_id

    # --- Reactive: Disease severity ---
    disease_severity <- reactive({
      preset <- input$ds_preset
      if (is.null(preset)) {
        return(0.01)
      }

      if (preset == "custom") {
        slider_val <- input$ds_slider
        if (is.null(slider_val)) {
          return(0.05)
        }
        slider_val / 100
      } else {
        as.numeric(preset)
      }
    }) |>
      debounce(250)

    # --- Reactive: Collect all cost inputs ---
    costs <- reactive({
      cost_vals <- sapply(program_ids, function(pid) {
        input[[paste0("cost_", pid)]]
      })
      setNames(cost_vals, as.character(program_ids))
    }) |>
      debounce(250)

    # --- Reactive: Calculate results ---
    results <- reactive({
      yield <- req(input$yield)
      price <- req(input$price)
      ds <- disease_severity()
      appl_cost <- req(input$appl_cost)
      current_costs <- costs()

      req(yield, price, ds)
      req(yield > 0, price > 0, ds >= 0, ds <= 1)

      calculate_all_metrics(
        programs_df = programs_df,
        costs = current_costs,
        yield = yield,
        price = price,
        disease_severity = ds,
        appl_cost = appl_cost
      )
    })

    # --- Output: Plot ---
    output$plot <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_benefit_plot(df, "Corn")
    })

    output$plot2 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_summary_gauge(df)
    })

    output$plot3 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_vertical_bar_plot(df, "Corn")
    })

    # --- Output: Data table ---
    output$table <- DT::renderDT({
      df <- results()
      req(nrow(df) > 0)

      display_df <- df |>
        mutate(
          Fungicide = program_name,
          `Application Rate` = application_rate,
          `Treatment Cost ($/ac)` = total_cost,
          `Breakeven Cost ($/ac)` = breakeven_cost,
          `Expected Net Benefit ($/ac)` = exp_net_benefit,
          `95% CI Lower` = exp_net_benefit_low,
          `95% CI Upper` = exp_net_benefit_high,
          `Breakeven Probability` = breakeven_prob,
          .keep = "none"
        )

      DT::datatable(
        display_df,
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy"),
            list(extend = "csv", filename = "corn_fungicide_roi"),
            list(extend = "excel", filename = "corn_fungicide_roi")
          )
        ),
        rownames = FALSE
      ) |>
        DT::formatCurrency(
          columns = c(
            "Treatment Cost ($/ac)",
            "Breakeven Cost ($/ac)",
            "Expected Net Benefit ($/ac)",
            "95% CI Lower",
            "95% CI Upper"
          ),
          digits = 2
        ) |>
        DT::formatPercentage(columns = "Breakeven Probability", digits = 1) |>
        DT::formatStyle(
          columns = "Expected Net Benefit ($/ac)",
          color = DT::styleInterval(0, c(COLORS$negative, "inherit"))
        )
    })

    # Return results reactive for potential use by parent
    results
  })
}
