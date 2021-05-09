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
        column(6,editModUI("map")),
        tags$hr()
      ),
      fluidRow(column(4,
                      h3('Add New Row'),
                      textInput('brewery','Brewery Name', width = '100%'),
                      textInput('address','Address', width = '100%'),
                      fluidRow(column(12,
                                      switchInput(inputId = "new", value = FALSE))
                      )),
               column(4),
               column(4,
                      actionButton("donebtn", "Done")),

      )
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

      n <- ncol(data_copy) # used to hide geometry and leaflet_id columns

      DT::datatable(
        data_copy,
        options = list(scrollY="400px",
                       pageLength = 5,
                       columnDefs = list(list(visible=FALSE, targets=(n-1):n))),
        # could support multi but do single for now
        selection = "single",
        editable = TRUE
      )
    })

    proxy = dataTableProxy('tbl')

    # unfortunately I did not implement last functionality
    # for editMap, so do it the hard way
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

          # replace if draw or edit
          if(skip==FALSE) {
            sf::st_geometry(data_copy[selected,]) <<- sf::st_geometry(
              mapedit:::st_as_sfc.geo_list(evt)
            )
            data_copy[selected,]$leaflet_id <<- evt$properties$`_leaflet_id`
          } else {

            if (input$new) {

              ng <<- sf::st_geometry(mapedit:::st_as_sfc.geo_list(evt)) # new geom object

              new_row <<- data.frame(brewery = input$brewery,
                                    address = input$address,
                                    leaflet_id = evt$properties$`_leaflet_id`) %>%
                sf::st_set_geometry(ng) %>%
                st_set_crs(4326)

              print('new row success')

              # add to data_copy data.frame and update visible table
              data_copy <<- data_copy %>%
                sf::st_set_crs(4326) %>%
                rbind(new_row)

              print('bind row success')

              replaceData(proxy, data_copy, resetPaging = FALSE)  # important

              showNotification('Added New Row')

              # reset input table
              updateTextInput(session, 'brewery', value = NA)
              updateTextInput(session, 'address', value = NA)
              updateSwitchInput(session, 'new', value = FALSE)

            }

          }
      })
    }

    addDrawObserve(EVT_DRAW)
    addDrawObserve(EVT_EDIT)

    # ensure no rows are selected when adding new row
    observeEvent(input$new, {
      if (input$new){
        proxy %>% selectRows(NULL)
      }
    })

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

    # update table cells with double click on cell
    observeEvent(input$tbl_cell_edit, {
      data_copy <<- editData(data_copy, input$tbl_cell_edit, 'tbl')
    })

    # provide mechanism to return after all done
    observeEvent(input$donebtn, {
      # ensure export is sf
      stopApp(st_sf(data_copy,crs=4326))
    })
  }

  return(runApp(shinyApp(ui,server)))

}


# let's act like breweries does not have geometries and select the top 2
brewsub <- breweries[1:2, 1:2,drop=TRUE]

brewpub <- make_an_sf(brewsub)

mapview(brewpub)
