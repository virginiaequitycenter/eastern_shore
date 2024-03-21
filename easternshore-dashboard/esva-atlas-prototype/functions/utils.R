library(shinyWidgets)

prettyRadioButtons <- function (
    inputId, label, choices = NULL, selected = NULL, status = "primary", 
    shape = c("round", "square", "curve"), outline = FALSE, 
    fill = FALSE, thick = FALSE, animation = NULL, icon = NULL, 
    plain = FALSE, bigger = FALSE, inline = FALSE, width = NULL, 
    choiceNames = NULL, choiceValues = NULL) 
{
  status <- match.arg(status, c("default", "primary", "success", 
                                "info", "danger", "warning"))
  shape <- match.arg(shape)
  if (is.null(choices) && is.null(choiceNames) && is.null(choiceValues)) {
    choices <- character(0)
  }
  args <- shinyWidgets:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- shiny::restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) {
    args$choiceValues[[1]]
  }
  else {
    as.character(selected)
  }
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  options1 <- shinyWidgets:::generatePretty(
    inputId = inputId, selected = selected, 
    inline = inline, type = "radio", choiceNames = args$choiceNames[1:3], 
    choiceValues = args$choiceValues[1:3], status = status, shape = shape, 
    outline = outline, fill = fill, thick = thick, animation = animation, 
    icon = icon, plain = plain, bigger = bigger
  )
  options2 <- shinyWidgets:::generatePretty(
    inputId = inputId, selected = selected, 
    inline = inline, type = "radio", choiceNames = args$choiceNames[4:5], 
    choiceValues = args$choiceValues[4:5], status = status, shape = shape, 
    outline = outline, fill = fill, thick = thick, animation = animation, 
    icon = icon, plain = plain, bigger = bigger
  )
  options <- tags$div(
    tags$div(
      tags$fieldset(
        tags$legend(style = "margin-bottom:0;", "Sea Level Rise"),
        options1
      )
    ),
    tags$div(
      tags$fieldset(
        tags$legend(style = "margin-bottom:0;", "Storm Surge"),
        tags$div(
          style = "display: inline-block;",
          options2
        )
      )
    )
  )
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  radioTag <- htmltools::tags$div(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
    tags$label(class = "control-label", `for` = inputId, 
               class = if (is.null(label)) 
                 "shiny-label-null", label), options)
  shinyWidgets:::attachShinyWidgetsDep(radioTag, "pretty")
}