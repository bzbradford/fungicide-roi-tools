# General interface for corn and soy

# UI function for crop analysis module
#' @param module_id namespace id for module instance
#' @param programs list of fungicide programs and params
#' @param opts named list of options for inputs etc
crop_ui <- function(module_id, programs, opts) {
  ns <- NS(module_id)

  # layout_sidebar ----
  layout_sidebar(
    ## sidebar ----
    sidebar = sidebar(
      title = NULL,
      width = 450,
      open = TRUE,

      # Yield/price/disease
      card(
        card_title("Yield and Disease Pressure"),
        card_body(
          # Yield input
          enhanced_numeric_input(
            inputId = ns("yield"),
            label = paste("Expected Yield (bu/ac)"),
            info = "Typical yield, absent any disease pressure",
            required = TRUE,
            placeholder = "Enter a valid yield",
            opts$yield_input
          ),

          # Sale price input
          enhanced_numeric_input(
            inputId = ns("price"),
            label = sprintf("Sale Price ($/bushel)"),
            info = "Expected sale price of harvested grain",
            required = TRUE,
            placeholder = "Enter a valid price",
            opts$price_input
          ),

          # Disease severity selection
          enhanced_radio_buttons(
            inputId = ns("ds_preset"),
            label = "Disease Severity",
            choices = opts$disease_severity_choices,
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
              opts$disease_severity_slider
            )
          )
        )
      ),

      # Treatment costs
      card(
        card_title(" Treatment Costs ($/acre)"),
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
      # nav_panel(
      #   title = "Chart 3",
      #   icon = bsicons::bs_icon("bar-chart-fill"),
      #   plotlyOutput(ns("plot3"), height = "500px")
      # ),
      # nav_panel(
      #   title = "Chart 4",
      #   icon = bsicons::bs_icon("bar-chart-fill"),
      #   plotlyOutput(ns("plot4"), height = "500px")
      # ),

      # Table tab
      nav_panel(
        title = "Data Table",
        icon = bsicons::bs_icon("table"),
        DT::DTOutput(ns("table"))
      )
    )
  )
} # end ui

#' Server function for crop analysis module
#'
#' @param module_id module namespace ID
#' @param programs data frame of programs from {crop}_programs.csv
#' @param opts crop-specific configuration
crop_server <- function(module_id, programs, opts) {
  moduleServer(module_id, function(input, output, session) {
    ns <- session$ns
    program_ids <- programs$program_id

    # Reactives ----

    rv <- reactiveValues(
      reset_costs = 1
    )

    ## disease_severity // either preset value or slider input ----
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

    # Costs UI ----

    output$costs_ui <- renderUI({
      rv$reset_costs # will regen ui if this changes
      build_costs_ui(programs, ns)
    })

    ## reset button handler ----
    observeEvent(input$reset_costs, {
      rv$reset_costs <- runif(1)
    })

    ## costs reactive ----
    # collect product costs from inputs as named list
    costs <- reactive({
      req(input$ready)
      vals <- sapply(program_ids, function(id) {
        input[[paste0("cost_", id)]] %||% NA_real_
      })
      set_names(vals, as.character(program_ids))
    }) |>
      debounce(250)

    ## results reactive ----
    # calculate results when inputs are available
    results <- reactive({
      yield <- input$yield
      price <- input$price
      ds <- disease_severity()
      appl_cost <- input$appl_cost
      current_costs <- costs()

      # check required inputs
      req(input$ready)
      validate(
        need(yield, label = "Yield"),
        need(price, label = "Price"),
        need(appl_cost, label = "Base application cost"),
        need(
          !all(is.na(current_costs)),
          message = "At least one product cost must be provided."
        )
      )

      calculate_all_metrics(
        programs = programs,
        costs = current_costs,
        yield = yield,
        price = price,
        disease_severity = ds,
        appl_cost = appl_cost
      )
    })

    # Plots ----

    # cost v benefit plot
    output$plot1 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_cost_benefit_plot(df)
    })

    # ranked programs plot
    output$plot2 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_benefit_plot(df, opts$crop_name)
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
    #   create_vertical_bar_plot(df, opts$crop_name)
    # })

    # Data table ----

    output$table <- DT::renderDT({
      df <- results()
      req(nrow(df) > 0)
      build_results_dt(df, opts$crop_name)
    })
  }) # end server
}
