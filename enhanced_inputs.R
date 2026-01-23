#' Enhanced Shiny Inputs with Info Tooltips and Reset Buttons
#'
#' Provides enhanced versions of numericInput, sliderInput, and radioButtons
#' with optional info tooltips and reset functionality.

# --- Shared Internal Functions ---

.build_input_header <- function(
  inputId,
  label,
  info,
  show_reset,
  reset_tooltip,
  default_value
) {
  icons <- list()

  if (!is.null(info)) {
    icons <- c(
      icons,
      list(
        bslib::tooltip(
          tags$span(
            class = "enhanced-input-icon info-icon",
            bsicons::bs_icon("info-circle", size = "1em")
          ),
          info,
          placement = "top"
        )
      )
    )
  }

  if (show_reset) {
    # Convert default value to JSON for complex types (e.g., vectors for sliders)
    default_json <- jsonlite::toJSON(default_value, auto_unbox = TRUE)

    reset_btn <- tags$button(
      type = "button",
      class = "enhanced-input-icon reset-icon btn btn-link p-0 border-0",
      id = paste0(inputId, "_reset"),
      `data-input-id` = inputId,
      `data-default-value` = as.character(default_json),
      `aria-label` = reset_tooltip,
      bsicons::bs_icon("arrow-counterclockwise", size = "1em")
    )

    icons <- c(
      icons,
      list(
        bslib::tooltip(
          reset_btn,
          reset_tooltip,
          placement = "top"
        )
      )
    )
  }

  tags$div(
    class = "enhanced-input-header",
    tags$span(class = "enhanced-input-label", label),
    if (length(icons) > 0) tags$div(class = "enhanced-input-icons", icons)
  )
}

.wrap_input <- function(header, input_element, width = NULL) {
  tags$div(
    class = "enhanced-input-container",
    style = if (!is.null(width)) paste0("width: ", width, ";") else NULL,
    header,
    input_element
  )
}

dots2env <- function(dots, env) {
  env_bind(env, !!!list_flatten(dots))
}

# --- Enhanced Numeric Input ---

enhanced_numeric_input <- function(
  inputId,
  ...,
  label,
  value = NULL,
  min = NA,
  max = NA,
  step = NA,
  width = NULL,
  info = NULL,
  show_reset = TRUE,
  reset_tooltip = paste("Reset to", value)
) {
  env_bind(current_env(), !!!list_flatten(list(...)))

  header <- .build_input_header(
    inputId,
    label,
    info,
    show_reset,
    reset_tooltip,
    value
  )

  input_el <- numericInput(
    inputId = inputId,
    label = NULL,
    value = value,
    min = min,
    max = max,
    step = step,
    width = "100%"
  )

  .wrap_input(header, input_el, width)
}


# --- Enhanced Slider Input ---

enhanced_slider_input <- function(
  inputId,
  ...,
  label,
  value = NULL,
  min = NULL,
  max = NULL,
  step = NULL,
  round = FALSE,
  ticks = TRUE,
  animate = FALSE,
  width = NULL,
  sep = ",",
  pre = NULL,
  post = NULL,
  timeFormat = NULL,
  timezone = NULL,
  dragRange = TRUE,
  info = NULL,
  show_reset = TRUE,
  reset_tooltip = "Reset to default"
) {
  env_bind(current_env(), !!!list_flatten(list(...)))

  header <- .build_input_header(
    inputId,
    label,
    info,
    show_reset,
    reset_tooltip,
    value
  )

  input_el <- sliderInput(
    inputId = inputId,
    label = NULL,
    value = value,
    min = min,
    max = max,
    step = step,
    round = round,
    ticks = ticks,
    animate = animate,
    width = "100%",
    sep = sep,
    pre = pre,
    post = post,
    timeFormat = timeFormat,
    timezone = timezone,
    dragRange = dragRange
  )

  .wrap_input(header, input_el, width)
}


# --- Enhanced Radio Buttons ---

enhanced_radio_buttons <- function(
  inputId,
  ...,
  label,
  choices,
  selected = NULL,
  inline = FALSE,
  width = NULL,
  choiceNames = NULL,
  choiceValues = NULL,
  info = NULL,
  show_reset = TRUE,
  reset_tooltip = "Reset to default"
) {
  env_bind(current_env(), !!!list_flatten(list(...)))

  # Determine the default selected value
  if (!is.null(selected)) {
    default_selected <- selected
  } else if (!is.null(choiceValues)) {
    default_selected <- choiceValues[[1]]
  } else if (!is.null(choices)) {
    default_selected <- if (is.list(choices)) choices[[1]] else choices[1]
    if (!is.null(names(choices))) {
      default_selected <- unname(choices[1])
    }
  } else {
    default_selected <- NULL
  }

  header <- .build_input_header(
    inputId,
    label,
    info,
    show_reset,
    reset_tooltip,
    default_selected
  )

  input_el <- radioButtons(
    inputId = inputId,
    label = NULL,
    choices = choices,
    selected = selected,
    inline = inline,
    width = "100%",
    choiceNames = choiceNames,
    choiceValues = choiceValues
  )

  .wrap_input(header, input_el, width)
}


# --- Enhanced Select Input ---

enhanced_select_input <- function(
  inputId,
  ...,
  label,
  choices,
  selected = NULL,
  multiple = FALSE,
  selectize = TRUE,
  width = NULL,
  size = NULL,
  info = NULL,
  show_reset = TRUE,
  reset_tooltip = "Reset to default"
) {
  env_bind(current_env(), !!!list_flatten(list(...)))

  # Determine default selected value
  if (!is.null(selected)) {
    default_selected <- selected
  } else if (!multiple) {
    default_selected <- if (is.list(choices)) choices[[1]] else choices[1]
    if (!is.null(names(choices))) {
      default_selected <- unname(choices[1])
    }
  } else {
    default_selected <- NULL
  }

  header <- .build_input_header(
    inputId,
    label,
    info,
    show_reset,
    reset_tooltip,
    default_selected
  )

  input_el <- selectInput(
    inputId = inputId,
    label = NULL,
    choices = choices,
    selected = selected,
    multiple = multiple,
    selectize = selectize,
    width = "100%",
    size = size
  )

  .wrap_input(header, input_el, width)
}


# --- Enhanced Text Input ---

enhanced_text_input <- function(
  inputId,
  ...,
  label,
  value = "",
  width = NULL,
  placeholder = NULL,
  info = NULL,
  show_reset = TRUE,
  reset_tooltip = "Reset to default"
) {
  env_bind(current_env(), !!!list_flatten(list(...)))

  header <- .build_input_header(
    inputId,
    label,
    info,
    show_reset,
    reset_tooltip,
    value
  )

  input_el <- textInput(
    inputId = inputId,
    label = NULL,
    value = value,
    width = "100%",
    placeholder = placeholder
  )

  .wrap_input(header, input_el, width)
}


# --- Enhanced Checkbox Input ---
enhanced_checkbox_input <- function(
  inputId,
  ...,
  label,
  value = FALSE,
  width = NULL,
  info = NULL,
  show_reset = TRUE,
  reset_tooltip = "Reset to default"
) {
  env_bind(current_env(), !!!list_flatten(list(...)))

  header <- .build_input_header(
    inputId,
    label,
    info,
    show_reset,
    reset_tooltip,
    value
  )

  # Checkbox needs special handling - we create a checkbox without its label
  # and use our header instead
  input_el <- tags$div(
    class = "form-check",
    tags$input(
      type = "checkbox",
      class = "form-check-input shiny-input-checkbox",
      id = inputId,
      checked = if (value) "checked" else NULL
    )
  )

  .wrap_input(header, input_el, width)
}
