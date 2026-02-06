# Alfalfa fungicide ROI module
# Separate from corn/soy due to fundamentally different statistical model:
# - 4-parameter regression with covariance matrices (vs 2-parameter with individual CIs)
# - Delta method CIs (vs stored CI bounds)
# - MVN simulation for breakeven (vs normal approximation)

# Calculation functions ---------------------------------------------------

#' Reconstruct 4x4 symmetric covariance matrix from 10 upper-triangle CSV columns
#' Parameter order: [Intercept, defoliation_difference, duration40, rfq_effect_size]
build_cov_matrix <- function(row) {
  matrix(
    c(
      row$cov_1_1, row$cov_1_2, row$cov_1_3, row$cov_1_4,
      row$cov_1_2, row$cov_2_2, row$cov_2_3, row$cov_2_4,
      row$cov_1_3, row$cov_2_3, row$cov_3_3, row$cov_3_4,
      row$cov_1_4, row$cov_2_4, row$cov_3_4, row$cov_4_4
    ),
    nrow = 4,
    byrow = TRUE
  )
}

#' Calculate net benefit with delta method CIs for one alfalfa fungicide
#'
#' @param hay_price Hay price in $/ton
#' @param defoliation Defoliation level (0-28)
#' @param theta_defoliation Theta for defoliation
#' @param rfq Alfalfa RFQ value
#' @param theta_rfq Theta for RFQ
#' @param duration_dummy 0 for 30-day, 1 for 40-day cutting
#' @param cost Treatment cost $/ac
#' @param params Named or positional vector: [Intercept, defoliation_difference, duration40, rfq_effect_size]
#' @param cov_matrix 4x4 covariance matrix matching params order
#' @param alpha Significance level (default 0.05)
#' @return tibble with exp_net_benefit, exp_net_benefit_low, exp_net_benefit_high
calculate_alfalfa_benefit <- function(
  hay_price,
  defoliation,
  theta_defoliation,
  rfq,
  theta_rfq,
  duration_dummy,
  cost,
  params,
  cov_matrix,
  alpha = 0.05
) {
  b0 <- params[1]
  b1 <- params[2]
  b2 <- params[3]
  b3 <- params[4]

  # Point estimate of linear benefit (proportional yield change)
  benefit <- b0 +
    b1 * (1 - theta_defoliation) * defoliation +
    b2 * duration_dummy +
    b3 * (theta_rfq - 1) * rfq

  # Gradient for delta method
  gradient <- c(
    1,
    (1 - theta_defoliation) * defoliation,
    duration_dummy,
    (theta_rfq - 1) * rfq
  )

  # Variance & SE via delta method
  benefit_var <- as.numeric(t(gradient) %*% cov_matrix %*% gradient)
  benefit_se <- sqrt(benefit_var)
  z <- qnorm(1 - alpha / 2)

  ci_lower <- benefit - z * benefit_se
  ci_upper <- benefit + z * benefit_se

  tibble(
    exp_net_benefit = hay_price * benefit - cost,
    exp_net_benefit_low = hay_price * ci_lower - cost,
    exp_net_benefit_high = hay_price * ci_upper - cost
  )
}

#' Calculate breakeven probability via MVN simulation
#'
#' @param n_sims Number of simulations
#' @param hay_price Hay price $/ton
#' @param defoliation Defoliation level
#' @param theta_defoliation Theta for defoliation
#' @param rfq RFQ value
#' @param theta_rfq Theta for RFQ
#' @param duration_dummy Duration dummy (0 or 1)
#' @param cost Treatment cost $/ac
#' @param params 4-element parameter vector
#' @param cov_matrix 4x4 covariance matrix
#' @param resid_mean Residual mean
#' @param resid_sd Residual standard deviation
#' @return Numeric breakeven probability (0-1)
calculate_alfalfa_breakeven <- function(
  n_sims,
  hay_price,
  defoliation,
  theta_defoliation,
  rfq,
  theta_rfq,
  duration_dummy,
  cost,
  params,
  cov_matrix,
  resid_mean = 0,
  resid_sd = 0
) {
  betas <- mvtnorm::rmvnorm(n_sims, mean = params, sigma = cov_matrix)

  lin_benefit <- betas[, 1] +
    betas[, 2] * (1 - theta_defoliation) * defoliation +
    betas[, 3] * duration_dummy +
    betas[, 4] * (theta_rfq - 1) * rfq

  if (resid_sd > 0) {
    lin_benefit <- lin_benefit + rnorm(n_sims, resid_mean, resid_sd)
  }

  net <- hay_price * lin_benefit - cost
  mean(net >= 0)
}

#' Calculate all alfalfa metrics, producing output compatible with plotting functions
#'
#' @param programs_df Data frame from alfalfa_programs.csv
#' @param costs Named vector of costs (names = program_id as character)
#' @param hay_price Hay price $/ton
#' @param defoliation Defoliation level
#' @param duration_dummy 0 for 30-day, 1 for 40-day
#' @param rfq RFQ value
#' @return Data frame with columns matching corn/soy contract
calculate_alfalfa_metrics <- function(
  programs_df,
  costs,
  hay_price,
  defoliation,
  duration_dummy,
  rfq
) {
  results <- programs_df |>
    filter(enabled) |>
    mutate(product_cost = costs[as.character(program_id)]) |>
    filter(!is.na(product_cost)) |>
    mutate(
      application_cost = 0,
      total_cost = product_cost + application_cost
    )

  if (nrow(results) == 0) {
    return(results)
  }

  results |>
    rowwise() |>
    mutate(
      params = list(c(intercept, defoliation_difference, duration40, rfq_effect_size)),
      cov_matrix = list(build_cov_matrix(pick(starts_with("cov_")))),
      calculate_alfalfa_benefit(
        hay_price = hay_price,
        defoliation = defoliation,
        theta_defoliation = theta_defoliation,
        rfq = rfq,
        theta_rfq = theta_rfq,
        duration_dummy = duration_dummy,
        cost = total_cost,
        params = params,
        cov_matrix = cov_matrix
      ),
      breakeven_prob = calculate_alfalfa_breakeven(
        n_sims = 10000,
        hay_price = hay_price,
        defoliation = defoliation,
        theta_defoliation = theta_defoliation,
        rfq = rfq,
        theta_rfq = theta_rfq,
        duration_dummy = duration_dummy,
        cost = total_cost,
        params = params,
        cov_matrix = cov_matrix,
        resid_mean = residuals_mean,
        resid_sd = residuals_std
      ),
      breakeven_cost = exp_net_benefit + total_cost,
      across(
        c(exp_net_benefit, exp_net_benefit_low, exp_net_benefit_high, breakeven_cost),
        ~ round(.x, 2)
      ),
      breakeven_prob = round(breakeven_prob, 3)
    ) |>
    ungroup() |>
    arrange(desc(exp_net_benefit))
}


# UI function -------------------------------------------------------------

alfalfa_ui <- function(id, programs) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = NULL,
      width = 450,
      open = TRUE,

      # Farm conditions
      card(
        card_title("Farm Conditions"),
        card_body(
          enhanced_numeric_input(
            inputId = ns("hay_price"),
            label = "Hay Price ($/ton)",
            info = "Expected sale price of hay",
            required = TRUE,
            placeholder = "Enter a valid price",
            value = 200,
            min = 100,
            max = 300,
            step = 1
          ),

          enhanced_radio_buttons(
            inputId = ns("cutting_duration"),
            label = "Cutting Duration",
            choices = c("30 days" = "30", "40 days" = "40"),
            selected = "40",
            inline = TRUE,
            info = "Number of days between cuttings"
          ),

          enhanced_slider_input(
            inputId = ns("rfq"),
            label = "Alfalfa RFQ",
            info = "Relative forage quality",
            value = 150,
            min = 135,
            max = 220,
            step = 1
          ),

          enhanced_slider_input(
            inputId = ns("defoliation"),
            label = "Defoliation Level",
            info = "Percent defoliation observed (0-28)",
            value = 5,
            min = 0,
            max = 28,
            step = 1
          )
        )
      ),

      # Treatment costs
      card(
        card_title("Treatment Costs ($/acre)"),
        card_body(
          uiOutput(ns("costs_ui"))
        )
      )
    ),

    # Main content - same tab structure as corn/soy
    rlang::exec(
      navset_card_tab,
      id = ns("results_tabs"),
      full_screen = TRUE,
      !!!c(
        lapply(1:4, function(i) {
          nav_panel(
            title = paste("Chart", i),
            icon = bsicons::bs_icon("bar-chart-fill"),
            plotlyOutput(ns(paste0("plot", i)), height = "500px")
          )
        }),
        list(
          nav_panel(
            title = "Data Table",
            icon = bsicons::bs_icon("table"),
            DT::DTOutput(ns("table"))
          )
        )
      )
    )
  )
}


# Server function ---------------------------------------------------------

alfalfa_server <- function(id, programs) {
  moduleServer(id, function(input, output, session) {
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

      tagList(
        div(
          style = "
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
            align-items: end;
            gap: 0.5rem;
            font-size: small;
          ",
          id = ns("cost_grid"),
          lapply(seq_len(nrow(programs)), function(i) {
            program <- slice(programs, i)
            enhanced_numeric_input(
              inputId = ns(paste0("cost_", program$program_id)),
              label = sprintf(
                "%s (%s)",
                program$program_name,
                program$application_rate
              ),
              required = FALSE,
              placeholder = "Excluded",
              value = round(program$default_cost, 2),
              min = 0,
              max = 200,
              step = 0.01
            )
          })
        ),

        actionButton(
          inputId = ns("reset_costs"),
          label = "Reset all costs"
        ),

        div(
          style = "display: none;",
          checkboxInput(
            inputId = ns("ready"),
            label = "Ready",
            value = TRUE
          )
        )
      )
    })

    # Reset button handler
    observeEvent(input$reset_costs, {
      rv$reset_costs <- runif(1)
    })

    # Costs reactive
    costs <- reactive({
      req(input$ready)
      vals <- sapply(program_ids, function(pid) {
        input[[paste0("cost_", pid)]] %||% NA_real_
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
      current_costs <- costs()

      req(input$ready)
      validate(
        need(hay_price, label = "Hay Price"),
        need(rfq, label = "RFQ"),
        need(!is.null(defoliation), label = "Defoliation"),
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
        rfq = rfq
      )
    })

    # Plots - reuse existing plot functions from global.R
    output$plot1 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_cost_benefit_plot(df)
    })

    output$plot2 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_benefit_plot(df, "Alfalfa")
    })

    output$plot3 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_summary_gauge(df)
    })

    output$plot4 <- plotly::renderPlotly({
      df <- results()
      req(nrow(df) > 0)
      create_vertical_bar_plot(df, "Alfalfa")
    })

    # Data table - same formatting as crop_server
    output$table <- DT::renderDT({
      df <- results()
      req(nrow(df) > 0)

      display_df <- df |>
        mutate(
          `Fungicide` = program_name,
          `Appl. Rate` = application_rate,
          `Product Cost ($/ac)` = product_cost,
          `Total Cost ($/ac)` = total_cost,
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
          pagination = FALSE,
          scrollX = TRUE,
          dom = "Bfrti",
          buttons = list(
            list(extend = "copy"),
            list(extend = "csv", filename = paste0(id, "_fungicide_roi")),
            list(extend = "excel", filename = paste0(id, "_fungicide_roi"))
          )
        ),
        rownames = FALSE
      ) |>
        DT::formatCurrency(
          columns = 3:8,
          digits = 2
        ) |>
        DT::formatPercentage(columns = "Breakeven Probability", digits = 1) |>
        DT::formatStyle(
          columns = "Expected Net Benefit ($/ac)",
          color = DT::styleInterval(0, c(COLORS$negative, "inherit"))
        )
    })
  })
}
