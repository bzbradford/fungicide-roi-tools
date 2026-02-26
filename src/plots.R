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

# Define color palette
plot_colors <- list(
  high_profit = "#0072B2", # blue
  profitable = "#56B4E9", # sky
  marginal = "#E69F00", # orange
  loss = "#D55E00", # red
  bg_green = "rgba(0, 158, 115, 0.03)",
  bg_red = "rgba(213, 94, 0, 0.03)"
)

# format data for plots with hover text
build_extra_plotly_data <- function(df, yield_units) {
  df |>
    mutate(
      marker_color = case_when(
        net_benefit > 10 ~ plot_colors$high_profit,
        net_benefit > 0 ~ plot_colors$profitable,
        net_benefit > -10 ~ plot_colors$marginal,
        TRUE ~ plot_colors$loss
      ),
      # Hover text
      hover_text = paste0(
        sprintf("<b>%s %s</b>", program_name, application_rate),
        if (yield_units == "%") {
          sprintf("<br>Yield: %+.1f%%", yield_benefit * 100)
        } else {
          sprintf("<br>Yield: %+.1f %s", yield_benefit, yield_units)
        },
        sprintf("<br>Total Cost: $%.2f", total_cost),
        sprintf("<br>Expected Benefit: $%.2f", net_benefit),
        sprintf("<br>Breakeven Prob: %.1f%%", breakeven_prob * 100),
        sprintf(
          "<br>95%% CI: $%.2f to $%.2f",
          net_benefit_low,
          net_benefit_high
        )
      )
    )
}

#' Scatter plot with program cost on X and expected benefit on Y
#' @param df from calculate_all_metrics()
#' @param opts config for crop. needs $yield_units
create_cost_benefit_plot <- function(df, opts = list()) {
  req(nrow(df) > 0)

  # Prepare data
  df_plot <- df |>
    mutate(
      point_size = scales::rescale(breakeven_prob, to = c(12, 40)),
      label = paste0(program_name, "<br>", application_rate)
    ) |>
    build_extra_plotly_data(opts$yield_units)

  # Calculate axis ranges
  x_range <- range(df_plot$total_cost, na.rm = TRUE)
  y_range <- range(df_plot$net_benefit, na.rm = TRUE)
  x_pad <- diff(x_range) * 0.15
  y_pad <- diff(y_range) * 0.15

  # Create plot
  plot_ly() |>
    # Add points
    add_trace(
      data = df_plot,
      x = ~total_cost,
      y = ~net_benefit,
      type = "scatter",
      mode = "markers+text",
      marker = list(
        size = ~point_size,
        color = ~marker_color,
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
      # margin = list(t = 80, b = 60, l = 70, r = 20),
      margin = list(t = 50, b = 10, l = 10, r = 10),
      title = list(
        text = sprintf(
          "<b>Cost vs. Expected Net Benefit (%s)</b><br><sup>Hover mouse of individual points for more information</sup>",
          opts$crop_name
        ),
        font = list(size = 18),
        x = 0.02,
        xanchor = "left"
      ),
      xaxis = list(
        title = "Total Cost per Acre (Product + Application)",
        tickprefix = "$",
        range = c(x_range[1] - x_pad, x_range[2] + x_pad),
        gridcolor = "rgba(200, 200, 200, 0.3)",
        zeroline = FALSE,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "Expected Net Benefit per Acre",
        tickprefix = "$",
        range = c(y_range[1] - y_pad, y_range[2] + y_pad),
        gridcolor = "rgba(200, 200, 200, 0.3)",
        zeroline = FALSE,
        fixedrange = TRUE
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
          text = "Not profitable",
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
      )
    ) |>
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "lasso2d",
        "select2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      displaylogo = FALSE
    )
}

# examples
if (FALSE) {
  calculate_all_metrics(
    programs_df = PROGRAMS$corn,
    costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
    yield = 180,
    price = 5,
    disease_severity = 0.05,
    appl_cost = 10
  ) |>
    create_cost_benefit_plot()
}


#' Create the main expected net benefit plot
#' Expected benefit on X, program name on Y
#'
#' @param df Data frame from calculate_all_metrics()
#' @param opts config for crop. needs $yield_units
create_benefit_plot <- function(df, opts = list()) {
  req(nrow(df) > 0)

  # Prepare data for plotting
  plot_df <- df |>
    mutate(
      label = sprintf(
        "%s %s<br>%s/ac",
        program_name,
        application_rate,
        scales::dollar(total_cost)
      ) |>
        fct_reorder(net_benefit)
    ) |>
    build_extra_plotly_data(opts$yield_units)

  # Calculate x-axis range for the shaded negative region
  x_min <- min(plot_df$net_benefit_low, 0, na.rm = TRUE)
  x_max <- max(plot_df$net_benefit_high, 0, na.rm = TRUE)
  x_pad <- (x_max - x_min) * 0.1

  # Build plot
  p <- plot_df |>
    plot_ly() |>
    # Points with error bars
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~net_benefit,
      y = ~label,
      error_x = list(
        type = "data",
        symmetric = FALSE,
        arrayminus = ~ (net_benefit - net_benefit_low),
        array = ~ (net_benefit_high - net_benefit),
        color = ~marker_color,
        thickness = 1.5
      ),
      marker = list(
        size = 15,
        color = ~marker_color,
        line = list(color = "black", width = 1),
        opacity = 1
      ),
      text = ~hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    ) |>
    layout(
      margin = list(t = 50, b = 10, l = 10, r = 10),
      title = list(
        text = sprintf(
          "<b>Expected Net Benefit by Fungicide Program (%s)</b><br><sup>Hover mouse of individual points for more information</sup>",
          opts$crop_name
        ),
        font = list(size = 18),
        x = 0.02,
        xanchor = "left"
      ),
      xaxis = list(
        title = "Expected Net Benefit ($/acre)",
        tickformat = "$,.0f",
        zeroline = TRUE,
        zerolinewidth = 2,
        zerolinecolor = "rgba(0,0,0,0.3)",
        range = c(x_min - x_pad, x_max + x_pad),
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = levels(plot_df$program_name),
        fixedrange = TRUE
      ),
      # Shaded region for negative values
      shapes = list(
        list(
          type = "rect",
          x0 = x_min - x_pad,
          x1 = 0,
          y0 = -1,
          y1 = nrow(plot_df),
          fillcolor = "rgba(255, 0, 0, 0.05)",
          line = list(width = 0),
          layer = "below"
        )
      ),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12)
      )
    ) |>
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "lasso2d",
        "select2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  p
}

# examples
if (FALSE) {
  calculate_all_metrics(
    programs_df = PROGRAMS$corn,
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
# create_summary_gauge <- function(results_df) {
#   if (nrow(results_df) == 0) {
#     return(NULL)
#   }

#   top_program <- results_df[1, ]

#   plot_ly(
#     type = "indicator",
#     mode = "gauge+number+delta",
#     value = top_program$breakeven_prob * 100,
#     title = list(
#       text = sprintf(
#         "Top Pick: %s<br><span style='font-size:0.8em'>Breakeven Probability</span>",
#         top_program$program_name
#       )
#     ),
#     delta = list(reference = 50, suffix = "%"),
#     gauge = list(
#       axis = list(range = c(0, 100), ticksuffix = "%"),
#       bar = list(color = COLORS$primary),
#       steps = list(
#         list(range = c(0, 50), color = "rgba(255,0,0,0.1)"),
#         list(range = c(50, 100), color = "rgba(0,255,0,0.1)")
#       ),
#       threshold = list(
#         line = list(color = "black", width = 2),
#         thickness = 0.75,
#         value = 50
#       )
#     ),
#     number = list(suffix = "%")
#   ) |>
#     plotly::layout(
#       margin = list(l = 30, r = 30, t = 80, b = 30)
#     )
# }

# # examples
# if (FALSE) {
#   calculate_all_metrics(
#     programs_df = PROGRAMS$corn,
#     costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
#     yield = 180,
#     price = 5,
#     disease_severity = 0.05,
#     appl_cost = 10
#   ) |>
#     create_summary_gauge()
# }

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
    levels = plot_df$label[order(-plot_df$net_benefit)]
  )

  plot_ly(plot_df, x = ~label) |>
    add_trace(
      type = "bar",
      y = ~net_benefit,
      error_y = list(
        type = "data",
        symmetric = FALSE,
        arrayminus = ~ (net_benefit - net_benefit_low),
        array = ~ (net_benefit_high - net_benefit)
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

# # examples
# if (FALSE) {
#   calculate_all_metrics(
#     programs_df = PROGRAMS$corn,
#     costs = rnorm(12, mean = 20, sd = 10) |> set_names(1:12),
#     yield = 180,
#     price = 5,
#     disease_severity = 0.05,
#     appl_cost = 10
#   ) |>
#     create_vertical_bar_plot("Corn")
# }
