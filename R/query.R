#' Interactively Select Map Features
#'
#' @param x map to use
#'
#' @param ... other arguments
#'
#' @example ./inst/examples/examples_select.R
#' @export
select_map <- function(x, ...) {
  UseMethod("select_map")
}

#' @export
select_map.leaflet <- function(
  x = NULL,
  style_false = list(fillOpacity = 0.2),
  style_true = list(fillOpacity = 0.7),
  target_groups = NULL
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  # add the script to handle selection state to leaflet
  x <- add_select_script(
    lf = x,
    style_false = style_false,
    style_true = style_true,
    target_groups = target_groups)

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(x, height=NULL, width=NULL),
    miniUI::gadgetTitleBar("Select Features on Map", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE))
  )

  server <- function(input, output, session) {
    id = "mapedit"
    select_evt = paste0(id, "_selected")

    # a container for our selections
    selections <- data.frame()

    observeEvent(input[[select_evt]], {
      #print(input[[select_evt]])
      if(nrow(selections) == 0) {
        selections <<- data.frame(
          group = input[[select_evt]]$group,
          selected = input[[select_evt]]$selected,
          stringsAsFactors = FALSE
        )
      } else {
        # see if already exists
        loc <- which(selections$group == input[[select_evt]]$group)

        if(length(loc) > 0) {
          selections[loc, "selected"] <<- input[[select_evt]]$selected
        } else {
          selections[nrow(selections) + 1, ] <<- c(input[[select_evt]]$group, input[[select_evt]]$selected)
        }
      }
    })


    shiny::observeEvent(input$done, {
      shiny::stopApp(
        selections
      )
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }

  shiny::runGadget(
    ui,
    server,
    viewer =  shiny::dialogViewer("Select"),
    stopOnCancel = FALSE
  )
}

#' @keywords internal
add_select_script <- function(lf, style_false, style_true, target_groups) {
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
      Shiny.onInputChange('mapedit_selected', {'group': layer.groupname, 'selected': selected})
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
      jsonlite::toJSON(style_false, auto_unbox=TRUE),
      jsonlite::toJSON(style_true, auto_unbox=TRUE)
    )
  )
}
