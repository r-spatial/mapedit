#' Draw (simple) features on a map
#'
#' @name drawFeatures
#'
#' @param map a background \code{leaflet} or \code{mapview} map
#'          to be used for editing. If \code{NULL} a blank
#'          mapview canvas will be provided.
#' @param sf \code{logical} return simple features.  The default is \code{TRUE}.
#'          If \code{sf = FALSE}, \code{GeoJSON} will be returned.
#' @param record \code{logical} to record all edits for future playback.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param title \code{string} to customize the title of the UI window.
#' @param editor \code{character} either "leaflet.extras" or "leafpm"
#' @param editorOptions \code{list} of options suitable for passing to
#'     either \code{leaflet.extras::addDrawToolbar} or
#'     \code{leafpm::addPmToolbar}.
#' @param ... additional arguments passed on to \code{\link{editMap}}.
#'
#' @details
#'   When setting \code{viewer = browserViewer(browser = getOption("browser"))} and
#'   the systems default browser is Firefox, the browser window will likely not
#'   automatically close when the app is closed (by pressing "done" or "cancel").
#'   To enable automatic closing of tabs/windows in Firefox try the following:
#'   \itemize{
#'     \item{input "about:config " to your firefox address bar and hit enter}
#'     \item{make sure your "dom.allow_scripts_to_close_windows" is true}
#'   }
#'
#' @export
drawFeatures = function(map = NULL,
                        sf = TRUE,
                        record = FALSE,
                        viewer = shiny::paneViewer(),
                        title = "Draw Features",
                        editor = c("leaflet.extras", "leafpm"),
                        editorOptions = list(),
                        ...) {
  res = editMap(x = map,
                sf = sf,
                record = record,
                viewer = viewer,
                title = title,
                editor = editor,
                editorOptions = editorOptions,
                ...)
  if (!inherits(res, "sf") && is.list(res)) res = res$finished
  return(res)
}
