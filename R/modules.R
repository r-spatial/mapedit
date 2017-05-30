#' Shiny Module UI for Geo Selection
#'
#' @param id \code{character} id for the the Shiny namespace
#' @param ... other arguments to \code{leafletOutput()}
#'
#' @return ui for Shiny module
#' @export
selectModUI <- function(id, ...) {
  ns <- shiny::NS(id)
  leafletOutput(ns("map"), ...)
}


#' Shiny Module Server for Geo Selection
#'
#' @param input Shiny server function input
#' @param output Shiny server function output
#' @param session Shiny server function session
#' @param leafmap leaflet map to use for Selection
#' @param styleFalse named \code{list} of valid \code{CSS} for non-selected features
#' @param styleTrue named \code{list} of valid \code{CSS} for selected features
#' @param targetGroups \code{character} for groups to use with selection
#'
#' @return server function for Shiny module
#' @export
selectMod <- function(
  input, output, session,
  leafmap,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7),
  targetGroups = NULL
) {

  output$map <- renderLeaflet({
    mapedit:::add_select_script(
      leafmap,
      styleFalse = styleFalse,
      styleTrue = styleTrue,
      targetGroups = targetGroups,
      ns = session$ns(NULL)
    )
  })

  id = "mapedit"
  select_evt = paste0(id, "_selected")

  df <- data.frame()

  # a container for our selections
  selections <- reactive({
    if(nrow(df) == 0) {
      df <<- data.frame(
        group = input[[select_evt]]$group,
        selected = input[[select_evt]]$selected,
        stringsAsFactors = FALSE
      )
    } else {
      # see if already exists
      loc <- which(df$group == input[[select_evt]]$group)

      if(length(loc) > 0) {
        df[loc, "selected"] <<- input[[select_evt]]$selected
      } else {
        df[nrow(df) + 1, ] <<- c(input[[select_evt]]$group, input[[select_evt]]$selected)
      }
    }

    return(df)
  })

  return(selections)

}
