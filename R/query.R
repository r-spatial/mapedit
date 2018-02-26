#' Interactively Select Map Features
#'
#' @param x \code{leaflet} or \code{mapview} map to use for selection
#' @param ... other arguments
#' @example ./inst/examples/examples_select.R
#' @export
selectMap <- function(x, ...) {
  UseMethod("selectMap")
}

#' @name selectMap
#' @param styleFalse,styleTrue names \code{list} of CSS styles used
#'          for selected (\code{styleTrue}) and deselected (\code{styleFalse})
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#' @export
selectMap.leaflet <- function(
  x = NULL,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7),
  ns = "mapedit-select",
  viewer = shiny::paneViewer(),
  ...
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      selectModUI(id = ns, height = "97%"),
      height=NULL, width=NULL
    ),
    miniUI::gadgetTitleBar("Select Features on Map", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)),
    tags$script(HTML(
"
// close browser window on session end
$(document).on('shiny:disconnected', function() {
  // check to make sure that button was pressed
  //  to avoid websocket disconnect caused by some other reason than close
  if(
    Shiny.shinyapp.$inputValues['cancel:shiny.action'] ||
    Shiny.shinyapp.$inputValues['done:shiny.action']
  ) {
    window.close()
  }
})
"
    ))
  )

  server <- function(input, output, session) {
    selections <- callModule(
      selectMod,
      ns,
      x,
      styleFalse = styleFalse,
      styleTrue = styleTrue
    )

    observe({selections()})

    shiny::observeEvent(input$done, {
      shiny::stopApp(
        selections()
      )
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }


  shiny::runGadget(
    ui,
    server,
    viewer = viewer,
    stopOnCancel = FALSE
  )
}

#' @keywords internal
add_select_script <- function(lf, styleFalse, styleTrue, ns="") {
  ## check for existing onRender jsHook?

  htmlwidgets::onRender(
    lf,
    sprintf(
"
function(el,x) {
  var lf = this;
  var style_obj = {
    'false': %s,
    'true': %s
  }

  // define our functions for toggling
  function toggle_style(layer, style_obj) {
    layer.setStyle(style_obj);
  };

  function toggle_state(layer, selected, init) {
    if(typeof(selected) !== 'undefined') {
      layer._mapedit_selected = selected;
    } else {
      selected = !layer._mapedit_selected;
      layer._mapedit_selected = selected;
    }
    if(typeof(Shiny) !== 'undefined' && Shiny.onInputChange && !init) {
      Shiny.onInputChange(
        '%s-mapedit_selected',
        {
          'group': layer.options.group,
          'id': layer.options.layerId,
          'selected': selected
        }
      )
    }
    return selected;
  };

  // set up click handler on each layer with a group name
  lf.eachLayer(function(lyr){
    if(lyr.on && lyr.options && lyr.options.layerId) {
      // start with all unselected ?
      toggle_state(lyr, false, init=true);
      toggle_style(lyr, style_obj[lyr._mapedit_selected]);

      lyr.on('click',function(e){
        var selected = toggle_state(e.target);
        toggle_style(e.target, style_obj[String(selected)]);
      });
    }
  });
}
",
      jsonlite::toJSON(styleFalse, auto_unbox=TRUE),
      jsonlite::toJSON(styleTrue, auto_unbox=TRUE),
      ns
    )
  )
}
