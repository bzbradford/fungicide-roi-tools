# Alfalfa functions ------------------------------------------------------------

#' Calculate net benefit with delta method CIs for one alfalfa fungicide
#'
#' @param inputs list with inputs from interface
#' @param cost cost of treatment
#' @param params row from programs df
#' @return tibble
calc_benefit_alfalfa <- function(
  inputs,
  cost,
  params
) {
  # capture inputs from list
  hay_price <- inputs$hay_price
  hay_yield <- inputs$hay_yield
  defoliation <- inputs$defoliation
  rfq <- inputs$rfq
  duration_dummy <- inputs$duration_dummy

  # Coefficients must match covariance matrix column order:
  # [intercept, duration_effect, defoliation_effect, rfq_effect]
  cov_matrix <- alfalfa_cov_matrix[[params$program_name]]

  # coefficients
  coefs <- c(
    params$intercept,
    params$duration_effect,
    params$defol_effect,
    params$rfq_effect
  )

  # Gradient for delta method. Order must match coefficient order above
  gradient <- c(
    1,
    duration_dummy,
    (1 - params$defol_theta) * defoliation,
    (params$rfq_theta - 1) * rfq
  )

  # Proportional yield change with program
  benefit <- sum(coefs * gradient)

  # Variance & SE via delta method
  benefit_var <- as.numeric(t(gradient) %*% cov_matrix %*% gradient)
  benefit_se <- sqrt(benefit_var)

  # Critical value (normal approximation for large df)
  ci_alpha <- 0.05 # 95% conf int
  z <- qnorm(1 - ci_alpha / 2)

  # Confidence interval on the linear scale
  ci_low <- benefit - z * benefit_se
  ci_high <- benefit + z * benefit_se

  # breakeven probability
  be_sims <- 10000
  betas <- mvtnorm::rmvnorm(be_sims, mean = coefs, sigma = cov_matrix)
  lin_benefit <- as.vector(betas %*% gradient) +
    rnorm(be_sims, params$residuals_mean, params$residuals_sd)
  be_probs <- hay_price * hay_yield * lin_benefit - cost

  # results
  tibble(
    yield_benefit = hay_yield * benefit,
    yield_benefit_low = hay_yield * ci_low,
    yield_benefit_high = hay_yield * ci_high,
    net_benefit = yield_benefit * hay_price - cost,
    net_benefit_low = yield_benefit_low * hay_price - cost,
    net_benefit_high = yield_benefit_high * hay_price - cost,
    breakeven_cost = net_benefit + cost,
    breakeven_prob = mean(be_probs > 0)
  )
}


#' Calculate all alfalfa metrics, producing output compatible with plotting functions
#'
#' @param programs programs df
#' @param costs named costs list by program id
#' @param appl_cost base cost to add for each app
#' @param inputs inputs collected from the interface and passed to calc_benefit_alfalfa
#' @return Data frame with columns matching corn/soy contract
calc_metrics_alfalfa <- function(
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
      calc_benefit_alfalfa(
        inputs = inputs,
        cost = total_cost,
        params = pick(everything())
      )
    ) |>
    ungroup()
}


# Alfalfa UI -------------------------------------------------------------------

#' @param module_id:string module id
#' @param opts:list config for crop
alfalfa_ui <- function(module_id = "alfalfa", opts = OPTS[[module_id]]) {
  stopifnot(!is.null(opts))

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
        card_title("Yield and Quality"),
        card_body(
          enhanced_slider_input(
            inputId = ns("hay_yield"),
            label = "Hay Yield (dry ton/acre)",
            placeholder = "Enter a valid yield",
            info = "Expected alfalfa dry yield on the next cutting",
            value = 1,
            min = 0.1,
            max = 2,
            step = 0.1
          ),
          enhanced_slider_input(
            inputId = ns("hay_price"),
            label = "Hay Price ($/ton)",
            placeholder = "Enter a valid price",
            info = "Expected sale price of hay",
            value = 150,
            min = 100,
            max = 300,
            step = 1
          ),
          enhanced_slider_input(
            inputId = ns("rfq"),
            label = "Relative Forage Quality",
            info = "Expected RFQ at harvest. RFQ is a measure of how much nutritive value is in the feed. An RFQ of 150 or more is typically considered 'Prime'.",
            value = 150,
            min = 100,
            max = 250,
            step = 25
          ),
          enhanced_radio_buttons(
            inputId = ns("cutting_duration"),
            label = "Cutting Duration",
            info = "Number of days between cuttings, if first cut use 30",
            inline = TRUE,
            choices = c("30 days" = "30", "40 days" = "40"),
            selected = "30"
          ),
          enhanced_slider_input(
            inputId = ns("defoliation"),
            label = "Defoliation Level",
            info = "Expected defoliation due to foliar disease at cutting",
            value = 5,
            min = 0,
            max = 50,
            step = 1
          ),
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

      # scatter plot
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

      # data table
      nav_panel(
        title = "Data Table",
        icon = bsicons::bs_icon("table"),
        DT::DTOutput(ns("table"))
      )
    )
  )
}


# Alfalfa server ---------------------------------------------------------------

#' @param module_id:string module id
#' @param programs:tibble fung programs df for alfalfa
#' @param opts:list config for crop
alfalfa_server <- function(
  module_id = "alfalfa",
  programs = PROGRAMS[[module_id]],
  opts = OPTS[[module_id]]
) {
  stopifnot(!is.null(programs), !is.null(opts))

  moduleServer(module_id, function(input, output, session) {
    ns <- session$ns
    program_ids <- programs$program_id

    rv <- reactiveValues(
      reset_costs = 1
    )

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
      inputs <- list(
        hay_yield = input$hay_yield,
        hay_price = input$hay_price,
        rfq = input$rfq,
        defoliation = input$defoliation,
        duration_dummy = as.numeric(identical(input$cutting_duration, "40"))
      )
      product_costs <- costs()
      appl_cost <- input$appl_cost

      # req(input$ready)

      validate(
        need(input$ready, label = "Please wait..."),
        need(!any(is.null(inputs)), label = "Missing input value"),
        need(appl_cost, label = "Base application cost"),
        need(
          !all(is.na(product_costs)),
          message = "At least one treatment cost must be provided."
        )
      )

      calc_metrics_alfalfa(
        programs = programs,
        costs = product_costs,
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
  })
}
