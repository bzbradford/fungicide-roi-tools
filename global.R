# Fungicide ROI app

# Dependencies ----

suppressPackageStartupMessages({
  library(rlang)
  library(tidyverse)
  library(markdown)
  library(shiny)
  library(bslib)
  library(bsicons)
  library(plotly)
  library(DT)
  library(mvtnorm) # for alfalfa
})

# renv will record these
if (FALSE) {
  library(air)
}


# Dev ----

# renv::clean()
# renv::snapshot()

# Functions ---------------------------------------------------------------

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

source("enhanced_inputs.R")


# Setup -------------------------------------------------------------------

corn_programs <- read_csv("data/corn_programs.csv", show_col_types = FALSE)
soy_programs <- read_csv("data/soybean_programs.csv", show_col_types = FALSE)
alfalfa_programs <- read_csv(
  "data/alfalfa_programs.csv",
  show_col_types = FALSE
)

crop_config <-
  function(
    crop_name,
    title,
    yield_input,
    price_input,
    disease_severity_choices,
    disease_severity_slider,
    appl_cost_input
  ) {
    as.list(environment())
  }

OPTS <- list(
  corn = crop_config(
    crop_name = "Corn",
    title = "Fungicide ROI Calculator for Tar Spot of Corn",
    yield_input = list(
      value = 180,
      min = 1,
      max = 400,
      step = 1
    ),
    price_input = list(
      value = 5,
      min = 0,
      max = 10
    ),
    disease_severity_choices = list(
      "Low (1%)" = .01,
      "High (5%)" = .05,
      "Custom" = "custom"
    ),
    disease_severity_slider = list(
      value = 5,
      min = 0,
      max = 50,
      step = 1
    )
  ),
  soy = crop_config(
    crop_name = "Soybean",
    title = "Fungicide ROI Calculator for White Mold of Soybean",
    yield_input = list(
      value = 40,
      min = 1,
      max = 80,
      step = 1
    ),
    price_input = list(
      value = 12,
      min = 0,
      max = 18,
      step = .01
    ),
    disease_severity_choices = list(
      "Low (15%)" = .15,
      "High (30%)" = .30,
      "Custom" = "custom"
    ),
    disease_severity_slider = list(
      value = 5,
      min = 0,
      max = 50,
      step = 1
    )
  )
)

# for autocomplete
opts <- OPTS[[1]]


# Economics functions ----------------------------------------------------------
#
# Core economic calculations for fungicide ROI analysis.
# These are pure functions that can be tested independently in the console.
#
# Key design principles:
# - Functions return tibbles where multiple values are needed (for use in mutate)
# - Vectorized operations where possible
# - No Shiny dependencies - can be sourced and tested standalone
#

#' Calculate expected net benefit for all estimate types at once
#'
#' Returns point estimate and confidence interval bounds in a single call.
#' Designed to be used with dplyr::mutate() - returns a tibble that will be
#' automatically unpacked into columns.
#'
#' @param b_0 Intercept coefficient (point estimate)
#' @param b_1 Slope coefficient (point estimate)
#' @param b_0_lower Lower CI bound for b_0
#' @param b_0_upper Upper CI bound for b_0
#' @param b_1_lower Lower CI bound for b_1
#' @param b_1_upper Upper CI bound for b_1
#' @param theta Disease control efficacy parameter
#' @param yield Expected yield
#' @param price Expected sale price per unit
#' @param disease_severity Disease severity proportion (0-1)
#' @param treatment_cost Total treatment cost per acre
#' @return tibble with columns: exp_net_benefit, exp_net_benefit_low, exp_net_benefit_high
calculate_net_benefit <- function(
  b_0,
  b_1,
  b_0_lower,
  b_0_upper,
  b_1_lower,
  b_1_upper,
  theta,
  yield,
  price,
  disease_severity,
  treatment_cost
) {
  # Helper to compute benefit for a given set of coefficients
  compute_benefit <- function(b0, b1) {
    yield_benefit <- b0 + b1 * (1 - theta) * disease_severity
    yield * price * yield_benefit - treatment_cost
  }

  tibble(
    exp_net_benefit = compute_benefit(b_0, b_1),
    exp_net_benefit_low = compute_benefit(b_0_lower, b_1_lower),
    exp_net_benefit_high = compute_benefit(b_0_upper, b_1_upper)
  )
}

# examples
if (FALSE) {
  # Standalone usage:
  calculate_net_benefit(
    b_0 = 0.044,
    b_1 = -0.028,
    b_0_lower = 0.017,
    b_0_upper = 0.071,
    b_1_lower = -0.222,
    b_1_upper = 0.166,
    theta = 0.455,
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    treatment_cost = 37
  )

  # In mutate (returns columns automatically):
  corn_programs |>
    mutate(total_cost = default_cost) |>
    mutate(
      calculate_net_benefit(
        b_0,
        b_1,
        b_0_lower,
        b_0_upper,
        b_1_lower,
        b_1_upper,
        theta,
        yield = 180,
        price = 5,
        disease_severity = 0.05,
        treatment_cost = total_cost
      ),
      .by = program_name,
      .keep = "used"
    )
}


#' Calculate breakeven probability using Monte Carlo simulation
#'
#' STUB implementation using normal approximation. In production, replace with
#' pre-computed MC draws from master_mc_list.
#'
#' @param b_0 Intercept coefficient
#' @param b_1 Slope coefficient
#' @param b_0_se Standard error of b_0
#' @param b_1_se Standard error of b_1
#' @param theta Disease control efficacy parameter
#' @param yield Expected yield
#' @param price Expected sale price
#' @param disease_severity Disease severity proportion (0-1)
#' @param treatment_cost Total treatment cost per acre
#' @param n_sim Number of MC simulations (default 10000)
#' @param mc_draws Optional pre-computed MC draws (overrides simulation)
#' @return Numeric breakeven probability (0-1)
calculate_breakeven_prob <- function(
  b_0,
  b_1,
  b_0_se,
  b_1_se,
  theta,
  yield,
  price,
  disease_severity,
  treatment_cost,
  n_sim = 10000,
  mc_draws = NULL
) {
  # Cost threshold as proportion of revenue
  threshold <- treatment_cost / (price * yield)

  if (!is.null(mc_draws)) {
    # Use pre-computed draws if available
    benefit_draws <- mc_draws
  } else {
    # STUB: Generate approximate MC draws using normal approximation
    # TODO: Replace with actual beta distribution draws when integrating real data
    mean_benefit <- b_0 + b_1 * (1 - theta) * disease_severity

    # Approximate SE using delta method (simplified)
    se_benefit <- sqrt(b_0_se^2 + (disease_severity * (1 - theta))^2 * b_1_se^2)

    # Generate draws (truncated at 0 for yield benefit)
    benefit_draws <- pmax(0, rnorm(n_sim, mean_benefit, se_benefit))
  }

  mean(benefit_draws > threshold)
}

# example
if (FALSE) {
  calculate_breakeven_prob(
    b_0 = 0.044,
    b_1 = -0.028,
    b_0_se = 0.014,
    b_1_se = 0.099,
    theta = 0.455,
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    treatment_cost = 37
  )
}


#' Calculate all economic metrics for a programs data frame
#'
#' Main workhorse function that computes all metrics for display.
#'
#' @param programs_df Data frame with program parameters (from CSV config)
#' @param costs Named vector of user-specified costs (names = program_id as character)
#' @param yield Expected yield
#' @param price Expected sale price
#' @param disease_severity Disease severity proportion (0-1)
#' @param aerial_cost Aerial application cost (added where applicable)
#' @return Data frame with all economic results, sorted by expected net benefit
calculate_all_metrics <- function(
  programs_df,
  costs,
  yield,
  price,
  disease_severity,
  appl_cost
) {
  # echo(costs)
  # echo(appl_cost)

  # Filter to active programs and compute total costs
  # when a user clears a cost field the cost is NA and we exclude it
  results <- programs_df |>
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
      calculate_net_benefit(
        b_0,
        b_1,
        b_0_lower,
        b_0_upper,
        b_1_lower,
        b_1_upper,
        theta,
        yield = yield,
        price = price,
        disease_severity = disease_severity,
        treatment_cost = total_cost
      ),
      breakeven_prob = calculate_breakeven_prob(
        b_0,
        b_1,
        b_0_se,
        b_1_se,
        theta,
        yield = yield,
        price = price,
        disease_severity = disease_severity,
        treatment_cost = total_cost
      ),
      breakeven_cost = exp_net_benefit + total_cost,
      # Round for display
      across(
        c(
          exp_net_benefit,
          exp_net_benefit_low,
          exp_net_benefit_high,
          breakeven_cost
        ),
        ~ round(.x, 2)
      ),
      breakeven_prob = round(breakeven_prob, 3)
    ) |>
    ungroup() |>
    arrange(desc(exp_net_benefit))
}

# examples
if (FALSE) {
  test_costs <- setNames(c(37, 28, 34), c("1", "2", "3"))
  calculate_all_metrics(
    programs_df = corn_programs,
    costs = test_costs,
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    appl_cost = 10
  ) |>
    glimpse()
}


# Alfalfa functions ------------------------------------------------------------

#' Reconstruct 4x4 symmetric covariance matrix from 10 upper-triangle CSV columns
#' Parameter order: [Intercept, defoliation_difference, duration40, rfq_effect_size]
build_cov_matrix <- function(row) {
  matrix(
    c(
      row$cov_1_1,
      row$cov_1_2,
      row$cov_1_3,
      row$cov_1_4,
      row$cov_1_2,
      row$cov_2_2,
      row$cov_2_3,
      row$cov_2_4,
      row$cov_1_3,
      row$cov_2_3,
      row$cov_3_3,
      row$cov_3_4,
      row$cov_1_4,
      row$cov_2_4,
      row$cov_3_4,
      row$cov_4_4
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
#' @param appl_cost base application cost for each program
#' @return Data frame with columns matching corn/soy contract
calculate_alfalfa_metrics <- function(
  programs_df,
  costs,
  hay_price,
  defoliation,
  duration_dummy,
  rfq,
  appl_cost
) {
  results <- programs_df |>
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
      params = list(c(
        intercept,
        defoliation_difference,
        duration40,
        rfq_effect_size
      )),
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
        c(
          exp_net_benefit,
          exp_net_benefit_low,
          exp_net_benefit_high,
          breakeven_cost
        ),
        ~ round(.x, 2)
      ),
      breakeven_prob = round(breakeven_prob, 3)
    ) |>
    ungroup() |>
    arrange(desc(exp_net_benefit))
}


# UI builders -------------------------------------------------------------

#' @param programs fungicide programs df
#' @param ns namespace function from module
build_costs_ui <- function(programs, ns) {
  tagList(
    # base application cost
    enhanced_numeric_input(
      inputId = ns("appl_cost"),
      label = paste("Base application cost"),
      info = "Cost per acre to apply, added to all costs",
      value = 10,
      min = 0,
      max = 50,
      required = TRUE,
      placeholder = "Enter a valid application cost"
    ),

    # product costs
    div(
      class = "cost-grid",
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
          value = round(program$default_cost / 5) * 5, # to nearest $5
          min = 0,
          max = 200,
          step = 0.01
        )
      })
    ),

    # reset
    actionButton(
      inputId = ns("reset_costs"),
      label = "Reset all costs"
    ),

    # hidden input that blocks calculations until the ui is ready
    div(
      style = "display: none;",
      checkboxInput(
        inputId = ns("ready"),
        label = "Ready",
        value = TRUE
      )
    )
  )
}


# Plots -------------------------------------------------------------------
#
# Visualization functions for fungicide ROI analysis.
# These are standalone functions that can be tested in the console.

# Color palette (Okabe-Ito colorblind-friendly)
COLORS <- list(
  primary = "#0072B2",
  secondary = "#999999",
  positive = "#009E73",
  negative = "#D55E00",
  highlight = "#E69F00",
  background = "#F5F5F5"
)


#' Create the main expected net benefit plot
#' Expected benefit on X, program name on Y
#'
#' @param results_df Data frame from calculate_all_metrics()
#' @param crop_name Display name for the crop (used in title)
#' @param top_n Number of top programs to highlight (default 3)
#' @return plotly object
create_benefit_plot <- function(results_df, crop_name, top_n = 3) {
  if (nrow(results_df) == 0) {
    # Return empty plot with message
    return(
      plot_ly() |>
        layout(
          title = "No programs to display",
          annotations = list(
            text = "Enter treatment costs to see results",
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 16)
          )
        )
    )
  }

  # Prepare data for plotting
  plot_df <- results_df |>
    mutate(
      rank = row_number(),
      color_group = if_else(rank <= top_n, "Top 3", "Other"),
      marker_color = if_else(rank <= top_n, COLORS$primary, COLORS$secondary),
      hover_text = sprintf(
        "<b>%s</b> (%s)<br>Expected Benefit: %s<br>95%% CI: [%s, %s]<br>Breakeven Prob: %.1f%%",
        program_name,
        application_rate,
        scales::dollar(exp_net_benefit),
        scales::dollar(exp_net_benefit_low),
        scales::dollar(exp_net_benefit_high),
        breakeven_prob * 100
      ),
      program_name = sprintf(
        "%s %s<br>%s/ac",
        program_name,
        application_rate,
        scales::dollar(total_cost)
      ),
    )

  # Order by benefit (lowest to highest for horizontal bars, so highest is at top)
  plot_df$program_name <- factor(
    plot_df$program_name,
    levels = plot_df$program_name[order(plot_df$exp_net_benefit)]
  )

  # Calculate x-axis range for the shaded negative region
  x_min <- min(plot_df$exp_net_benefit_low, 0, na.rm = TRUE)
  x_max <- max(plot_df$exp_net_benefit_high, 0, na.rm = TRUE)
  x_pad <- (x_max - x_min) * 0.1

  # Build plot
  p <- plot_df |>
    plot_ly(y = ~program_name) |>
    # Points with error bars
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~exp_net_benefit,
      error_x = list(
        type = "data",
        symmetric = FALSE,
        arrayminus = ~ (exp_net_benefit - exp_net_benefit_low),
        array = ~ (exp_net_benefit_high - exp_net_benefit),
        color = ~marker_color,
        thickness = 1.5
      ),
      marker = list(
        size = 12,
        color = ~marker_color
      ),
      text = ~hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    ) |>
    layout(
      title = list(
        text = sprintf(
          "Expected Net Benefit by Fungicide Program (%s)",
          crop_name
        ),
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Expected Net Benefit ($/acre)",
        tickformat = "$,.0f",
        zeroline = TRUE,
        zerolinewidth = 2,
        zerolinecolor = "rgba(0,0,0,0.3)",
        range = c(x_min - x_pad, x_max + x_pad)
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = levels(plot_df$program_name)
      ),
      # Shaded region for negative values
      shapes = list(
        list(
          type = "rect",
          x0 = x_min - x_pad,
          x1 = 0,
          y0 = -0.5,
          y1 = nrow(plot_df) - 0.5,
          fillcolor = "rgba(255, 0, 0, 0.05)",
          line = list(width = 0),
          layer = "below"
        )
      ),
      margin = list(l = 180, r = 50, t = 60, b = 50),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      )
    ) |>
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d")
    )

  p
}

# examples
if (FALSE) {
  calculate_all_metrics(
    programs_df = corn_programs,
    costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    appl_cost = 10
  ) |>
    create_benefit_plot("Corn")
}


#' Create a summary card plot showing key metrics
#' Optional visualization showing top program recommendation.
#'
#' @param results_df Data frame from calculate_all_metrics()
#' @return plotly object (gauge or indicator style)
create_summary_gauge <- function(results_df) {
  if (nrow(results_df) == 0) {
    return(NULL)
  }

  top_program <- results_df[1, ]

  plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = top_program$breakeven_prob * 100,
    title = list(
      text = sprintf(
        "Top Pick: %s<br><span style='font-size:0.8em'>Breakeven Probability</span>",
        top_program$program_name
      )
    ),
    delta = list(reference = 50, suffix = "%"),
    gauge = list(
      axis = list(range = c(0, 100), ticksuffix = "%"),
      bar = list(color = COLORS$primary),
      steps = list(
        list(range = c(0, 50), color = "rgba(255,0,0,0.1)"),
        list(range = c(50, 100), color = "rgba(0,255,0,0.1)")
      ),
      threshold = list(
        line = list(color = "black", width = 2),
        thickness = 0.75,
        value = 50
      )
    ),
    number = list(suffix = "%")
  ) |>
    plotly::layout(
      margin = list(l = 30, r = 30, t = 80, b = 30)
    )
}

# examples
if (FALSE) {
  calculate_all_metrics(
    programs_df = corn_programs,
    costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    appl_cost = 10
  ) |>
    create_summary_gauge()
}


#' Shows programs as vertical bars instead of horizontal.
#' Useful when there are many programs.
#'
#' @param results_df Data frame from calculate_all_metrics()
#' @param crop_name Display name for the crop
#' @return plotly object
create_vertical_bar_plot <- function(results_df, crop_name) {
  if (nrow(results_df) == 0) {
    return(plotly::plot_ly() |> plotly::layout(title = "No data"))
  }

  plot_df <- results_df |>
    mutate(
      rank = row_number(),
      bar_color = if_else(rank <= 3, COLORS$primary, COLORS$secondary),
      label = paste0(program_name, "\n(", application_rate, ")")
    )

  # Order by benefit descending
  plot_df$label <- factor(
    plot_df$label,
    levels = plot_df$label[order(-plot_df$exp_net_benefit)]
  )

  plot_ly(plot_df, x = ~label) |>
    add_trace(
      type = "bar",
      y = ~exp_net_benefit,
      error_y = list(
        type = "data",
        symmetric = FALSE,
        arrayminus = ~ (exp_net_benefit - exp_net_benefit_low),
        array = ~ (exp_net_benefit_high - exp_net_benefit)
      ),
      marker = list(color = ~bar_color),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Expected: %{y:$,.2f}<br>",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = sprintf("Expected Net Benefit (%s)", crop_name),
      xaxis = list(
        title = "",
        tickangle = -45
      ),
      yaxis = list(
        title = "Expected Net Benefit ($/acre)",
        tickformat = "$,.0f",
        zeroline = TRUE,
        zerolinewidth = 2
      ),
      shapes = list(
        list(
          type = "line",
          x0 = -0.5,
          x1 = nrow(plot_df) - 0.5,
          y0 = 0,
          y1 = 0,
          line = list(color = "black", width = 1, dash = "dash")
        )
      ),
      margin = list(b = 120)
    )
}

# examples
if (FALSE) {
  calculate_all_metrics(
    programs_df = corn_programs,
    costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    appl_cost = 10
  ) |>
    create_vertical_bar_plot("Corn")
}


#' Scatter plot with program cost on X and expected benefit on Y
create_cost_benefit_plot <- function(df) {
  req(nrow(df) > 0)

  # Define color palette
  plot_colors <- list(
    high_profit = "#0072B2", # blue
    profitable = "#56B4E9", # sky
    marginal = "#E69F00", # orange
    loss = "#D55E00", # red
    bg_green = "rgba(0, 158, 115, 0.03)",
    bg_red = "rgba(213, 94, 0, 0.03)"
  )

  # Prepare data
  df_plot <- df |>
    mutate(
      # Profit category
      profit_category = case_when(
        exp_net_benefit > 10 ~ "High Profit (>$10)",
        exp_net_benefit > 0 ~ "Profitable ($0-$10)",
        exp_net_benefit > -10 ~ "Marginal (-$10-$0)",
        TRUE ~ "Loss (<-$10)"
      ),
      profit_category = factor(
        profit_category,
        levels = c(
          "High Profit (>$10)",
          "Profitable ($0-$10)",
          "Marginal (-$10-$0)",
          "Loss (<-$10)"
        )
      ),
      # Color mapping
      point_color = case_when(
        exp_net_benefit > 10 ~ plot_colors$high_profit,
        exp_net_benefit > 0 ~ plot_colors$profitable,
        exp_net_benefit > -10 ~ plot_colors$marginal,
        TRUE ~ plot_colors$loss
      ),
      # Size scaling (map breakeven_prob to reasonable point sizes)
      point_size = scales::rescale(breakeven_prob, to = c(12, 40)),
      # Label for plot
      label = paste0(program_name, "<br>", application_rate),
      # Hover text
      hover_text = paste0(
        "<b>",
        program_name,
        " ",
        application_rate,
        "</b><br>",
        "Total Cost: $",
        round(total_cost, 2),
        "<br>",
        "Expected Benefit: $",
        round(exp_net_benefit, 2),
        "<br>",
        "Breakeven Prob: ",
        scales::percent(breakeven_prob, accuracy = 0.1),
        "<br>",
        "95% CI: $",
        round(exp_net_benefit_low, 2),
        " to $",
        round(exp_net_benefit_high, 2)
      )
    )

  # Calculate axis ranges
  x_range <- range(df_plot$total_cost, na.rm = TRUE)
  y_range <- range(df_plot$exp_net_benefit, na.rm = TRUE)
  x_pad <- diff(x_range) * 0.15
  y_pad <- diff(y_range) * 0.15

  # Create plot
  plot_ly() |>
    # # Add error bars for confidence intervals
    # add_trace(
    #   data = df_plot,
    #   x = ~total_cost,
    #   y = ~exp_net_benefit,
    #   type = "scatter",
    #   mode = "markers",
    #   error_y = list(
    #     type = "data",
    #     symmetric = FALSE,
    #     array = ~ (exp_net_benefit_high - exp_net_benefit),
    #     arrayminus = ~ (exp_net_benefit - exp_net_benefit_low),
    #     color = "rgba(100, 100, 100, 0.4)",
    #     thickness = 1,
    #     width = 4
    #   ),
    #   marker = list(size = 0, opacity = 0),
    #   hoverinfo = "skip",
    #   showlegend = FALSE
    # ) |>
    # Add points
    add_trace(
      data = df_plot,
      x = ~total_cost,
      y = ~exp_net_benefit,
      type = "scatter",
      mode = "markers+text",
      marker = list(
        size = ~point_size,
        color = ~point_color,
        line = list(color = "white", width = 2),
        opacity = 0.85
      ),
      text = ~label,
      textposition = "top center",
      textfont = list(size = 10),
      hovertext = ~hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    ) |>
    # Layout
    layout(
      title = list(
        text = "<b>Cost vs. Expected Net Benefit</b><br><sup>Point size = breakeven probability</sup>",
        x = 0.02,
        xanchor = "left"
      ),
      xaxis = list(
        title = "Total Cost per Acre (Product + Application)",
        tickprefix = "$",
        range = c(x_range[1] - x_pad, x_range[2] + x_pad),
        gridcolor = "rgba(200, 200, 200, 0.3)",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Expected Net Benefit per Acre",
        tickprefix = "$",
        range = c(y_range[1] - y_pad, y_range[2] + y_pad),
        gridcolor = "rgba(200, 200, 200, 0.3)",
        zeroline = FALSE
      ),
      # Horizontal line at y=0 (breakeven)
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = 0,
          y1 = 0,
          yref = "y",
          line = list(color = "grey40", width = 1.5, dash = "dash")
        ),
        # Green background for profit zone
        list(
          type = "rect",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = 0,
          y1 = 1e6,
          yref = "y",
          fillcolor = plot_colors$bg_green,
          line = list(width = 0),
          layer = "below"
        ),
        # Red background for loss zone
        list(
          type = "rect",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = -1e6,
          y1 = 0,
          yref = "y",
          fillcolor = plot_colors$bg_red,
          line = list(width = 0),
          layer = "below"
        )
      ),
      # Annotations for zones
      annotations = list(
        list(
          x = 0.02,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = "Profitable",
          showarrow = FALSE,
          font = list(color = "rgba(0, 128, 0, 0.5)", size = 14),
          xanchor = "left",
          yanchor = "top"
        ),
        list(
          x = 0.02,
          y = 0.02,
          xref = "paper",
          yref = "paper",
          text = "Unprofitable",
          showarrow = FALSE,
          font = list(color = "rgba(200, 0, 0, 0.5)", size = 14),
          xanchor = "left",
          yanchor = "bottom"
        )
      ),
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "gray",
        font = list(size = 12)
      ),
      margin = list(t = 80, b = 60, l = 70, r = 20)
    ) |>
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
}

# examples
if (FALSE) {
  calculate_all_metrics(
    programs_df = corn_programs,
    costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    appl_cost = 10
  ) |>
    create_cost_benefit_plot()
}


# Source modules ---------------------------------------------------------------

source("module__crop.R")
source("module__alfalfa.R")
