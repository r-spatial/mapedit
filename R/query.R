#' Interactively Select Map Features
#'
#' @param x map to use
#'
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
selectMap <- function(x, ...) {
  UseMethod("selectMap")
}

#' @export
selectMap.leaflet <- function(
  x = NULL,
  styleFalse = list(fillOpacity = 0.2, weight = 1, opacity = 0.4),
  styleTrue = list(fillOpacity = 0.7, weight = 3, opacity = 0.7),
  targetGroups = NULL,
  ns = "mapedit-select"
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(selectModUI(ns), height=NULL, width=NULL),
    miniUI::gadgetTitleBar("Select Features on Map", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE))
  )

  server <- function(input, output, session) {
    selections <- callModule(
      selectMod,
      ns,
      x,
      styleFalse = styleFalse,
      styleTrue = styleTrue,
      targetGroups = targetGroups
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
    stopOnCancel = FALSE
  )
}

#' @keywords internal
add_select_script <- function(lf, styleFalse, styleTrue, targetGroups, ns=NULL) {
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

  function toggle_state(layer, selected) {
    if(typeof(selected) !== 'undefined') {
      layer._mapedit_selected = selected;
    } else {
      selected = !layer._mapedit_selected;
      layer._mapedit_selected = selected;
    }
    if(typeof(Shiny) !== 'undefined' && Shiny.onInputChange) {
debugger;
      Shiny.onInputChange('%s-mapedit_selected', {'group': layer.groupname, 'selected': selected})
    }
    return selected;
  };

  // set up click handler on each layer with a group name
  lf.eachLayer(function(lyr){
    if(lyr.on && lyr.groupname) {
      // start with all unselected ?
      toggle_state(lyr, false);
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
