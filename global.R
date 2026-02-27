# Fungicide ROI app

# Dependencies -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(rlang)
  library(tidyverse)
  library(markdown)
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
})


# Dev --------------------------------------------------------------------------

if (FALSE) {
  # add to renv
  library(devtools)
  library(testthat)

  # renv
  renv::init()
  renv::update()
  renv::clean()
  renv::snapshot()

  # Run unit tests
  testthat::test_dir("tests/testthat")
}


# Functions --------------------------------------------------------------------

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

# rounds to nearest whole multiple of y
#' @param x number or vector
#' @param d divisor to round to
round_to <- function(x, d = 5) {
  round(x / d) * d
}


# Setup ------------------------------------------------------------------------

# treatment programs with costs and parameters
PROGRAMS <- list(
  corn = read_csv("data/corn-programs.csv", show_col_types = FALSE),
  soy = read_csv("data/soybean-programs.csv", show_col_types = FALSE),
  alfalfa = read_csv("data/alfalfa-programs.csv", show_col_types = FALSE)
)

# covariance matrix for alfalfa calculations
# names must match and be in the same order
source("data/alfalfa_cov_matrix.R")
stopifnot(identical(PROGRAMS$alfalfa$program_name, names(alfalfa_cov_matrix)))

# input configs for each module
OPTS <- list(
  corn = list(
    slug = "corn",
    crop_name = "Corn",
    yield_units = "bu/ac",
    yield_numeric_input = list(
      value = 180,
      min = 1,
      max = 400,
      step = 1,
      info = "Typical yield, absent any disease pressure"
    ),
    price_numeric_input = list(
      value = 5,
      min = 0,
      max = 10,
      step = 0.01,
      info = "Expected sale price of harvested grain"
    ),
    severity_radio_input = list(
      choices = list(
        "Low (1%)" = .01,
        "High (5%)" = .05,
        "Custom" = "custom"
      ),
      info = "Expected disease severity on ear leaf at the end of the season"
    ),
    severity_slider_input = list(
      value = 5,
      min = 0,
      max = 50,
      step = 1,
      info = "Expected disease severity on ear leaf at the end of the season"
    )
  ),
  soy = list(
    slug = "soy",
    crop_name = "Soybean",
    yield_units = "bu/ac",
    yield_numeric_input = list(
      value = 40,
      min = 1,
      max = 80,
      step = 1,
      info = "Typical yield, absent any disease pressure"
    ),
    price_numeric_input = list(
      value = 12,
      min = 0,
      max = 18,
      step = 0.01,
      info = "Expected sale price of harvested grain"
    ),
    severity_radio_input = list(
      choices = list(
        "Low (15%)" = .15,
        "High (30%)" = .30,
        "Custom" = "custom"
      ),
      info = "Expected white mold severity at the end of the season"
    ),
    severity_slider_input = list(
      value = 5,
      min = 0,
      max = 50,
      step = 1,
      info = "Expected white mold severity at the end of the season"
    )
  ),
  alfalfa = list(
    slug = "alfalfa",
    crop_name = "Alfalfa",
    yield_units = "ton/ac"
  )
)

# make sure the ids match
stopifnot(setequal(names(PROGRAMS), names(OPTS)))

# for autocomplete
opts <- OPTS[[1]]


# UI builders ------------------------------------------------------------------

costs_ui_title <- span(
  style = "display: flex; justify-content: space-between; align-items: center;",
  "Treatment Costs ($/acre)",
  bslib::tooltip(
    tags$span(
      class = "enhanced-input-icon info-icon",
      bsicons::bs_icon("info-circle", size = "1em")
    ),
    "All treatment costs default to current U.S. market median prices; these values can be changed to reflect local pricing.",
    placement = "top"
  )
)

#' @param programs fungicide programs df
#' @param ns namespace function from module
build_costs_ui <- function(programs, ns) {
  tagList(
    # base application cost
    enhanced_numeric_input(
      inputId = ns("appl_cost"),
      label = paste("Base application cost"),
      info = "Sprayer application cost for a single foliar application, added to each program based on number of program sprays.",
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
          value = round_to(program$default_cost, 2.5),
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

#' @param df results df from `calc_metrics()` or `calc_metrics_alfalfa()`
#' @param opts list config for the crop
build_results_dt <- function(df, opts) {
  yield_col <- sprintf("Yield Preserved (%s)", opts$yield_units)
  display_df <- df |>
    arrange(desc(net_benefit)) |>
    mutate(
      `Fungicide` = program_name,
      `Appl. Rate` = application_rate,
      `Appl. Cost ($/ac)` = application_cost,
      `Product Cost ($/ac)` = product_cost,
      `Total Cost ($/ac)` = total_cost,
      !!yield_col := round(yield_benefit, 2),
      `Expected Net Benefit ($/ac)` = net_benefit,
      `Benefit Low Est. ($/ac)` = net_benefit_low,
      `Benefit High Est. ($/ac)` = net_benefit_high,
      `Breakeven Cost ($/ac)` = breakeven_cost,
      `Breakeven Probability (%)` = breakeven_prob,
      .keep = "none"
    )

  dollar_cols <- which(str_detect(names(display_df), fixed("$")))
  pct_cols <- which(str_detect(names(display_df), fixed("%")))

  DT::datatable(
    display_df,
    extensions = "Buttons",
    selection = "none",
    options = list(
      pagination = FALSE,
      scrollX = TRUE,
      dom = "Bfrti",
      buttons = list(
        list(extend = "copy"),
        list(extend = "csv", filename = paste0(opts$slug, "_fungicide_roi")),
        list(
          extend = "excel",
          filename = paste0(opts$slug, "_fungicide_roi")
        )
      ),
      columnDefs = list(
        list(className = "dt-center", targets = c(3:(ncol(display_df) - 1)))
      )
    ),
    rownames = FALSE
  ) |>
    DT::formatCurrency(columns = dollar_cols, digits = 2) |>
    DT::formatPercentage(columns = pct_cols, digits = 1) |>
    DT::formatStyle(
      columns = dollar_cols,
      color = DT::styleInterval(0, c(COLORS$negative, "inherit"))
    )
}


# Source files -----------------------------------------------------------------

list.files("src", "*.R", full.names = TRUE) |> lapply(source)
