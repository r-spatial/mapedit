# start toward a Shiny gadget for Leaflet and Leaflet.Draw
#   still missing many features but hopefully serves
#   as proof of concept

#' Leaflet Draw Shiny Gadget
#'
#' @param lf leaflet map currently with \code{addDrawToolbar} already
#'             added.
#' @param width,height valid \code{CSS} size for the gadget

drawonme <- function(lf = NULL, height = NULL, width = NULL) {
  # modeled after chemdoodle gadget
  #  https://github.com/zachcp/chemdoodle/blob/master/R/chemdoodle_sketcher_gadgets.R
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))
  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(lf, height=NULL, width=NULL),
    miniUI::gadgetTitleBar("Draw Something", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE))
  )

  server <- function(input, output, session) {
    drawn <- list()
    edited <- list()

    shiny::observeEvent(input$undefined_draw_new_feature, {
      # we can clean this up
      drawn <<- c(drawn, list(input$undefined_draw_new_feature))
    })

    shiny::observeEvent(input$undefined_draw_edited_features, {
      edited <<- input$undefined_draw_edited_features
      # find the edited features and update drawn
      # start by getting the leaflet ids to do the match
      ids <- unlist(lapply(drawn, function(x){x$properties$`_leaflet_id`}))
      # now modify drawn to match edited
      lapply(edited$features, function(x){
        loc <- match(x$properties$`_leaflet_id`, ids)
        drawn[loc] <<- list(x)
      })
    })

    shiny::observeEvent(input$done, { shiny::stopApp(drawn) })
    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }

  shiny::runGadget(
    ui,
    server,
    viewer =  shiny::dialogViewer("Draw and Edit"),
    stopOnCancel = FALSE
  )
}


# example use
library(leaflet)
library(leaflet.extras)
library(mapview)

lf <- mapview(breweries91)@map %>%
  addTiles() %>%
  addDrawToolbar(editOptions = editToolbarOptions())

drawn <- drawonme(lf)
drawn

Reduce(
  function(x,y) {
    x %>% addGeoJSON(y)
  },
  drawn,
  init = lf
)

library(lawn)
l_pts <- lawn_featurecollection(
  as.list(unname(apply(breweries91@coords,MARGIN=1,lawn_point)))
)

l_poly <- lawn_featurecollection(
  list(lawn_polygon(drawn[[1]]$geometry$coordinates))
)

l_in <- lawn_within(l_pts, l_poly)
l_out <- lawn_featurecollection(Filter(
  function(pt) {
    !lawn_inside(pt, lawn_polygon(drawn[[1]]$geometry$coordinates))
  },
  as.list(unname(apply(breweries91@coords,MARGIN=1,lawn_point)))
))

view(l_in) %>%
  addGeoJSON(drawn[[1]])

view(l_out) %>%
  addGeoJSON(drawn[[1]])


# try with sf
library(sf)
library(purrr)

map(drawn, function(feat) {
  str(feat,max.level=1)
  eval(
    call(
      paste0("st_",tolower(feat$geometry$type)),
      list(
        matrix(
          unlist(feat$geometry$coordinates),
          byrow=TRUE,
          ncol=2
        )
      )
    )
  )
})



# use example polygon from ?st_polygon
#  I don't think Leaflet.Draw handles holes
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
pl1 <- st_polygon(pts)
# make it geojson so we can use with drawonme
pl1_g <- geojsonio::geojson_json(pl1)
lf_pg <- leaflet() %>%
  addGeoJSONv2(pl1_g, layerId = "polygon") %>%
  addTiles() %>%
  addDrawToolbar(targetLayer="polygon", editOptions=editToolbarOptions())
drawonme(lf_pg)

# just for fun demo geojson to sf
jsonlite::fromJSON(pl1_g,simplifyVector=FALSE)$coordinates %>%
  lapply(
    function(x) unlist(x) %>% matrix(byrow=TRUE, ncol=2)
  ) %>%
  st_polygon()
