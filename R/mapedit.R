#' mapedit: interactive editing and selection for geospatial data
#'
#' mapedit, a RConsortium funded project, provides interactive
#' tools to incorporate in geospatial workflows that require editing or selection
#' of spatial data.
#'
#' @section Edit:
#' \itemize{
#' \item{\code{\link{editMap}}}
#' \item{\code{\link{editFeatures}}}
#' \item{Shiny edit module \code{\link{editModUI}}, \code{\link{editMod}}}
#' }
#'
#' #' @section Edit:
#' \itemize{
#' \item{\code{\link{selectMap}}}
#' \item{\code{\link{selectFeatures}}}
#' \item{Shiny edit module \code{\link{selectModUI}}, \code{\link{selectMod}}}
#' }
"_PACKAGE"


## usethis namespace: start
#' @importFrom cli cli_abort
#' @importFrom htmltools browsable HTML tags save_html
#' @importFrom leaflet leaflet addProviderTiles leafletOptions leafletCRS
#'   leafletSizingPolicy providerTileOptions `%>%`
#' @importFrom mapview mapview mapviewGetOption
#' @importFrom methods slot slotNames as
#' @importFrom raster nrow ncol extent ncell
#' @importFrom rlang check_installed check_required caller_env
#' @importFrom scales rescale
#' @importFrom shinyWidgets actionBttn show_alert useSweetAlert
#' @importFrom sp bbox coordinates
## usethis namespace: end
NULL
