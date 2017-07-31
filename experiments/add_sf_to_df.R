library(leaflet)
library(mapview)
library(mapedit)
library(sf)
library(DT)
library(shiny)
library(htmltools)

script_zoom <- tags$script(
  HTML(
"
function findleaf() {
  return HTMLWidgets.find('.leaflet').getMap();
}

function zoom(layerid) {
  var map = findleaf();
  map.fitBounds(map._layers[layerid].getBounds());
}

Shiny.addCustomMessageHandler(
  'zoomselected',
  function(layerid) {
debugger;
    zoom(layerid);
  }
)
"
  )
)

make_an_sf <- function(dat) {
  ui <- tagList(
    script_zoom,
    fluidPage(
      fluidRow(
        column(6,DT::dataTableOutput("tbl",width="100%", height="400px")),
        column(6,editModUI("map"))
      ),
      fluidRow(actionButton("donebtn", "Done"))
    )
  )

  server <- function(input, output, session) {
    data_copy <- st_as_sf(
      dat,
      geometry = st_sfc(lapply(seq_len(nrow(dat)),function(i){st_point()}))
    )

    # add column for leaflet id, since we will need to track layer id
    #   to offer zoom to
    data_copy$leaflet_id <- NA

    edits <- callModule(
      editMod,
      leafmap = mapview()@map,
      id = "map"
    )

    output$tbl <- DT::renderDataTable({
      DT::datatable(
        dat,
        options = list(scrollY="400px"),
        # could support multi but do single for now
        selection = "single"
      )
    })

    # unfortunately I did not implement last functionality
    #  for editMap, so do it the hard way
    # last seems useful, so I might circle back and add that
    EVT_DRAW <- "map_draw_new_feature"
    EVT_EDIT <- "map_draw_edited_features"
    EVT_DELETE <- "map_draw_deleted_features"

    nsm <- function(event="", id="map") {
      paste0(session$ns(id), "-", event)
    }

    addDrawObserve <- function(event) {
      observeEvent(
        input[[nsm(event)]],
        {
          evt <- input[[nsm(event)]]
          # for now if edit, just consider, first feature
          #   of the FeatureCollection
          if(event == EVT_DELETE) {
            evt <- evt$features[1]
          }

          # get selected row
          selected <- isolate(input$tbl_rows_selected)

          skip = FALSE
          # ignore if selected is null
          #  not great but good enough for poc
          if(is.null(selected)) {skip = TRUE}

          # ignore if no event
          #if(length(evt) == 0) {skip = TRUE}

          # replace if draw or edit
          if(skip==FALSE) {
            sf::st_geometry(data_copy[selected,]) <<- sf::st_geometry(
              mapedit:::st_as_sfc.geo_list(evt)
            )
            data_copy[selected,]$leaflet_id <<- evt$properties$`_leaflet_id`
          }
      })
    }

    addDrawObserve(EVT_DRAW)
    addDrawObserve(EVT_EDIT)

    observeEvent(
      input[[nsm(EVT_DELETE)]],
      {
        evt <- input[[nsm(EVT_DELETE)]]
        # get selected row
        selected <- isolate(input$tbl_rows_selected)

        skip = FALSE
        # ignore if selected is null
        #  not great but good enough for poc
        if(is.null(selected)) {skip = TRUE}

        # ignore if no event
        #if(length(last) == 0) {skip = TRUE}

        # remove if delete
        if(skip==FALSE) {
          sf::st_geometry(data_copy[selected,]) <<- st_geometry(sf::st_sfc(st_point()))
          data_copy[selected,]$leaflet_id <<- NA
        }
      }
    )


    # zoom to if feature available on selected row
    observeEvent(
      input$tbl_rows_selected,
      {
        selected <- input$tbl_rows_selected
        if(!is.null(selected)) {
          rowsel <- data_copy[selected, ]
          # simple check to see if feature available
          #   and leaflet id populated
          if(
            all(!is.na(sf::st_coordinates(sf::st_geometry(rowsel)[[1]]))) &&
            !is.na(rowsel$leaflet_id)
          ) {
            print(rowsel)
            session$sendCustomMessage("zoomselected", rowsel$leaflet_id)
          }
        }
      }
    )

    # provide mechanism to return after all done
    observeEvent(input$donebtn, {
      # convert to sf

      stopApp(st_sf(data_copy,crs=4326))
    })
  }

  return(runApp(shinyApp(ui,server)))
}


# let's act like breweries does not have geometries
brewsub <- breweries[,1:4,drop=TRUE]

brewpub <- make_an_sf(brewsub)

mapview(brewpub)
