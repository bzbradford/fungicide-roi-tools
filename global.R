# Fungicide ROI app

library(rlang)
library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(DT)

# renv will record these
if (FALSE) {
  library(styler)
  library(miniUI)
}

# renv::clean()
# renv::snapshot()

source("corn.R")
source("enhanced_inputs.R")


# old data structures

# readRDS("corn_app/beta_params.rds")
# readRDS("corn_app/fung_models.rds")
# readRDS("corn_app/fung_models_df.rds") %>%
#   write_csv("corn_models_df.csv")

# Setup -------------------------------------------------------------------

crop_settings <- read_csv("config/crop_settings.csv", show_col_types = FALSE)
# crop_settings$crop_icon

corn_programs <- read_csv("config/corn_programs.csv", show_col_types = FALSE)

OPTS <- lst(
  # disease severity
  corn_ds_choices = list(
    "Low (1%)" = .01,
    "High (5%)" = .05,
    "Custom" = "custom"
  ),
  corn_ds_slider = list(
    value = 5,
    min = 0,
    max = 50,
    step = 1
  ),
  corn_yield = list(
    value = 180,
    min = 1,
    max = 500,
    step = 1
  ),
  corn_price = list(
    value = 5,
    min = 0,
    max = 10
  ),
  corn_appl_cost = list(
    value = 10,
    min = 0,
    max = 100
  ),
)


# Economics --------------------------------------------------------------------
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
  # Filter to active programs and compute total costs
  results <- programs_df |>
    filter(enabled) |>
    mutate(
      user_cost = costs[as.character(program_id)],
      total_cost = user_cost + appl_cost
    ) |>
    # Filter out programs with NA or zero cost
    filter(!is.na(total_cost), total_cost > 0)

  if (nrow(results) == 0) {
    return(results)
  }

  # Calculate net benefits (returns 3 columns via tibble unpacking)
  results <- results |>
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
      )
    ) |>
    ungroup()

  # Calculate breakeven probability (must be separate due to MC simulation)
  results <- results |>
    rowwise() |>
    mutate(
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
      )
    ) |>
    ungroup()

  # Add derived metrics and round for display
  results |>
    mutate(
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
#'
#' @param results_df Data frame from calculate_all_metrics()
#' @param crop_display Display name for the crop (used in title)
#' @param top_n Number of top programs to highlight (default 3)
#' @return plotly object
create_benefit_plot <- function(results_df, crop_display, top_n = 3) {
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
          crop_display
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
#'
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


#' Create comparison bar chart (alternative visualization)
#'
#' Shows programs as vertical bars instead of horizontal.
#' Useful when there are many programs.
#'
#' @param results_df Data frame from calculate_all_metrics()
#' @param crop_display Display name for the crop
#' @return plotly object
create_vertical_bar_plot <- function(results_df, crop_display) {
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
      title = sprintf("Expected Net Benefit (%s)", crop_display),
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
