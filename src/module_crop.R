# Economics functions ----------------------------------------------------------

#' Calculate expected net benefit for all estimate types at once
#'
#' @param inputs list with inputs from interface
#' @param cost cost of treatment
#' @param params row from programs df
#' @return tibble
calc_benefit <- function(
  inputs,
  cost,
  params
) {
  # from programs df
  b0 <- params$b0
  b0_se <- params$b0_se
  b1 <- params$b1
  b1_se <- params$b1_se
  theta <- params$theta

  # from interface
  yield <- inputs$yield
  price <- inputs$price
  ds <- inputs$disease_severity

  # calculate yield change
  calc_yield <- function(b0, b1) {
    yield * (b0 + b1 * (1 - theta) * ds)
  }

  # breakeven probability
  mean_benefit <- b0 + b1 * (1 - theta) * ds

  # Approximate SE using delta method (simplified)
  se_benefit <- sqrt(b0_se^2 + (ds * (1 - theta))^2 * b1_se^2)

  # Generate draws (truncated at 0 for yield benefit)
  be_sims <- 10000
  be_threshold <- cost / (price * yield)
  be_probs <- pmax(0, rnorm(be_sims, mean_benefit, se_benefit))

  tibble(
    yield_benefit = calc_yield(b0, b1),
    yield_benefit_low = calc_yield(b0 - b0_se, b1 - b1_se),
    yield_benefit_high = calc_yield(b0 + b0_se, b1 + b1_se),
    net_benefit = yield_benefit * price - cost,
    net_benefit_low = yield_benefit_low * price - cost,
    net_benefit_high = yield_benefit_high * price - cost,
    breakeven_cost = net_benefit + cost,
    breakeven_prob = mean(be_probs > be_threshold)
  )
}

# test
if (FALSE) {
  calc_benefit(
    inputs = list(
      yield = 180,
      price = 5,
      disease_severity = 0.05
    ),
    cost = 37,
    params = list(
      b0 = 0.044,
      b1 = -0.028,
      b0_se = 0.017,
      b1_se = 0.015,
      theta = 0.455
    )
  )
}


#' Calculate all economic metrics for a programs data frame
#'
#' @param programs programs df
#' @param costs named costs list by program id
#' @param appl_cost base cost to add for each app
#' @param inputs inputs collected from the interface and passed to calc_benefit
#' @return Data frame with all economic results
calc_metrics <- function(
  programs,
  costs,
  appl_cost,
  inputs
) {
  # Filter to active programs with input costs and compute total costs
  results <- programs |>
    filter(enabled) |>
    mutate(product_cost = costs[as.character(program_id)]) |>
    filter(!is.na(product_cost)) |>
    mutate(
      application_cost = appl_cost * n_appl,
      total_cost = product_cost + application_cost
    )

  if (nrow(results) == 0) {
    return(results)
  }

  results |>
    rowwise() |>
    mutate(
      calc_benefit(
        inputs = inputs,
        cost = total_cost,
        params = pick(everything())
      ),
    ) |>
    ungroup()
}

# examples
if (FALSE) {
  test_costs <- setNames(c(37, 28, 34), c("1", "2", "3"))
  calc_metrics(
    programs = PROGRAMS$corn,
    costs = test_costs,
    appl_cost = 10,
    inputs = list(
      yield = 180,
      price = 5,
      disease_severity = 0.05
    )
  ) |>
    glimpse()
}


# Corn and soy interface -------------------------------------------------------

# UI function for crop analysis module
#' @param module_id namespace id for module instance
#' @param opts named list of options for inputs etc
crop_ui <- function(module_id, opts = OPTS[[module_id]]) {
  stopifnot(!is.null(opts))

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
          enhanced_slider_input(
            inputId = ns("yield"),
            label = "Expected Yield (bu/ac)",
            placeholder = "Enter a valid yield",
            opts$yield_numeric_input
          ),
          enhanced_slider_input(
            inputId = ns("price"),
            label = sprintf("Sale Price ($/bushel)"),
            placeholder = "Enter a valid price",
            opts$price_numeric_input
          ),
          enhanced_radio_buttons(
            inputId = ns("ds_preset"),
            label = "Disease Severity",
            inline = TRUE,
            opts$severity_radio_input
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", "ds_preset"),
            ns = ns,
            enhanced_slider_input(
              inputId = ns("ds_slider"),
              label = "Custom Disease Severity:",
              post = "%",
              opts$severity_slider_input
            )
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

      # Cost v benefit scatterplot
      nav_panel(
        title = "Cost vs Benefit",
        icon = bsicons::bs_icon("bar-chart-fill"),
        plotlyOutput(ns("plot1"), height = "500px")
      ),

      # ranked programs plot
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
} # end ui


# Corn and soy server ----------------------------------------------------------

#' @param module_id module namespace ID
#' @param programs data frame of programs from {crop}_programs.csv
#' @param opts crop-specific configuration
crop_server <- function(
  module_id,
  programs = PROGRAMS[[module_id]],
  opts = OPTS[[module_id]]
) {
  stopifnot(!is.null(programs), !is.null(opts))

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
      inputs <- list(
        yield = input$yield,
        price = input$price,
        disease_severity = disease_severity()
      )
      current_costs <- costs()
      appl_cost <- input$appl_cost

      # check required inputs
      validate(
        need(input$ready, label = "Please wait..."),
        need(!any(is.na(inputs)), label = "Missing input values"),
        need(appl_cost, label = "Base application cost"),
        need(
          !all(is.na(current_costs)),
          message = "At least one product cost must be provided."
        )
      )

      calc_metrics(
        programs = programs,
        costs = current_costs,
        appl_cost = appl_cost,
        inputs = inputs
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

    # Data table ----

    output$table <- DT::renderDT({
      df <- results()
      req(nrow(df) > 0)
      build_results_dt(df, opts)
    })
  }) # end server
}
