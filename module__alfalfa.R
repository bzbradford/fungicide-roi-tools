# Alfalfa fungicide ROI module
# Separate from corn/soy due to fundamentally different statistical model:
# - 4-parameter regression with covariance matrices (vs 2-parameter with individual CIs)
# - Delta method CIs (vs stored CI bounds)
# - MVN simulation for breakeven (vs normal approximation)

#' @param module_id:string module id
#' @param programs:tibble fung program df for alfalfa
#' @param opts:list config for crop
alfalfa_ui <- function(module_id, programs, opts) {
  ns <- NS(module_id)

  # layout_sidebar ----
  layout_sidebar(
    ## sidebar ----
    sidebar = sidebar(
      title = NULL,
      width = 450,
      open = TRUE,

      # Farm conditions
      card(
        card_title("Farm Conditions"),
        card_body(
          # Hay price
          enhanced_numeric_input(
            inputId = ns("hay_price"),
            label = "Hay Price ($/ton)",
            placeholder = "Enter a valid price",
            required = TRUE,
            value = 200,
            min = 100,
            max = 300,
            step = 1,
            info = "Expected sale price of hay"
          ),

          # Cutting duration
          enhanced_radio_buttons(
            inputId = ns("cutting_duration"),
            label = "Cutting Duration",
            inline = TRUE,
            choices = c("30 days" = "30", "40 days" = "40"),
            selected = "30",
            info = "Number of days between cuttings, if first cut use 30"
          ),

          # RFQ
          enhanced_slider_input(
            inputId = ns("rfq"),
            label = "Alfalfa RFQ",
            info = "Expected relative forage quality",
            value = 150,
            min = 135,
            max = 220,
            step = 1
          ),

          # Defoliation
          enhanced_slider_input(
            inputId = ns("defoliation"),
            label = "Defoliation Level",
            info = "Expected defoliation level on the day of cutting",
            value = 5,
            min = 0,
            max = 28,
            step = 1
          )
        )
      ),

      # Treatment costs
      card(
        card_title(costs_ui_title),
        card_body(
          uiOutput(ns("costs_ui"))
        )
      )
    ),

    ## main ----
    navset_card_tab(
      id = ns("results_tabs"),
      full_screen = TRUE,

      # Chart tabs
      nav_panel(
        title = "Cost vs Benefit",
        icon = bsicons::bs_icon("bar-chart-fill"),
        plotlyOutput(ns("plot1"), height = "500px")
      ),
      nav_panel(
        title = "Ranked Programs",
        icon = bsicons::bs_icon("bar-chart-fill"),
        plotlyOutput(ns("plot2"), height = "500px")
      ),

      # Table tab
      nav_panel(
        title = "Data Table",
        icon = bsicons::bs_icon("table"),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

#' @param module_id:string module id
#' @param programs:tibble fung programs df for alfalfa
#' @param opts:list config for crop
alfalfa_server <- function(module_id, programs, opts) {
  moduleServer(module_id, function(input, output, session) {
    ns <- session$ns
    program_ids <- programs$program_id

    rv <- reactiveValues(
      reset_costs = 1
    )

    # Duration dummy reactive
    duration_dummy <- reactive({
      req(input$cutting_duration)
      if (as.numeric(input$cutting_duration) == 40) 1 else 0
    })

    # Costs UI
    output$costs_ui <- renderUI({
      rv$reset_costs
      build_costs_ui(programs, ns)
    })

    # Reset button handler
    observeEvent(input$reset_costs, {
      rv$reset_costs <- runif(1)
    })

    # Costs reactive
    costs <- reactive({
      req(input$ready)
      vals <- sapply(program_ids, function(id) {
        input[[paste0("cost_", id)]] %||% NA_real_
      })
      set_names(vals, as.character(program_ids))
    }) |>
      debounce(250)

    # Results reactive
    results <- reactive({
      hay_price <- input$hay_price
      rfq <- input$rfq
      defoliation <- input$defoliation
      dur <- duration_dummy()
      appl_cost <- input$appl_cost
      current_costs <- costs()

      req(input$ready)
      validate(
        need(hay_price, label = "Hay Price"),
        need(rfq, label = "RFQ"),
        need(!is.null(defoliation), label = "Defoliation"),
        need(appl_cost, label = "Base application cost"),
        need(
          !all(is.na(current_costs)),
          message = "At least one treatment cost must be provided."
        )
      )

      calculate_alfalfa_metrics(
        programs_df = programs,
        costs = current_costs,
        hay_price = hay_price,
        defoliation = defoliation,
        duration_dummy = dur,
        rfq = rfq,
        appl_cost = appl_cost
      )
    })

    # Plots ----

    # cost v benefit plot
    output$plot1 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_cost_benefit_plot(df, opts)
    })

    # ranked programs plot
    output$plot2 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_benefit_plot(df, opts)
    })

    # output$plot3 <- plotly::renderPlotly({
    #   df <- results()
    #   req(nrow(df) > 0)
    #   create_summary_gauge(df)
    # })
    #
    # output$plot4 <- plotly::renderPlotly({
    #   df <- results()
    #   req(nrow(df) > 0)
    #   create_vertical_bar_plot(df, "Alfalfa")
    # })

    # Data table ----
    output$table <- DT::renderDT({
      df <- results()
      req(nrow(df) > 0)
      build_results_dt(df, opts)
    })
  })
}
