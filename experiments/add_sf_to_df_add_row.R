library(leaflet)
library(mapview)
library(mapedit)
library(sf)
library(DT)
library(shiny)
library(shinyWidgets)
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






make_an_sf <- function(dat){

  ui <- tagList(
    script_zoom,
    fluidPage(
      fluidRow(
        column(12, editModUI("map"))
      ),
      tags$hr(),
      fluidRow(
        column(6,
               DT::dataTableOutput("tbl",width="100%", height=300)),
        column(3,
                      h3('Add New Row'),
                      uiOutput('dyn_form'),
                      actionButton("row_add", "Add Row")
               ),
        column(3,
               h3('Add New Column'),
               shiny::textInput('new_name', 'New Column Name', width = '100%'),
               shiny::selectInput('new_type', 'Column Type', choices = c('character', 'numeric', 'integer')),
               actionButton("col_add", "Add Column")
               )
      ),
      fluidRow(tags$hr(),
               actionButton("donebtn", "Done"))

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


    df <- reactiveValues(types = sapply(dat, class),
                         data = data_copy)



    observeEvent(input$col_add, {

                  #TODO: add checks for missing inputs
                  add_col <- df$data

                  add_col[[input$new_name]] <- do.call(paste0('as.', input$new_type), list(NA))

                  df$data <- add_col

                  #replaceData(proxy, data_copy, resetPaging = TRUE, clearSelection = 'all')  # important

                  # add input$new_name to df$type
                  ntype <- input$new_type
                  names(ntype) <- input$new_name

                  df$types <- c(df$types, ntype)

                  updateTextInput(session, 'new_name', value = NA)


                  })

    observeEvent(input$row_add, {

      # creates first column and row (must be more elegant way)
      new_row <- data.frame(X = input[[names(df$types[1])]])
      colnames(new_row) <- names(df$types[1])

      # remaining columns will be correct size
      for (i in 2:length(df$types)) {
        new_row[names(df$types[i])] <- input[[names(df$types[i])]]
      }

      new_row$leaflet_id <- NA
      new_row <- st_as_sf(new_row, geometry = st_sfc(st_point()))

      # add to data_copy data.frame and update visible table
      df$data <- df$data %>%
        rbind(new_row)

      showNotification('Added New Row')

      # reset input table (TODO: adjust to variable input names)
      updateTextInput(session, 'brewery', value = NA)
      updateTextInput(session, 'address', value = NA)
      updateSwitchInput(session, 'new', value = FALSE)

    })


    edits <- callModule(
      editMod,
      leafmap = mapview()@map,
      id = "map"
    )

    observe({

      output$dyn_form <- renderUI({

        tagList(
          lapply(1:length(df$types), function(n){
            if (df$types[n] == 'character') {
              textInput(names(df$types[n]), names(df$types[n]),
                        width = '100%')
            } else if (df$types[n] == 'factor') {
              selectInput(names(df$types[n]), names(df$types[n]),
                          choices = levels(dat[[names(df$types[n])]]),
                          selected = NULL,
                          selectize = TRUE,
                          width = '100%')
            } else if (df$types[n] == 'numeric') {
              numericInput(names(df$types[n]), names(df$types[n]),
                           value = 0,
                           width = '100%')
            }
          })

        )


      })

    })

    output$tbl <- DT::renderDataTable({

      n <- grep('geom|leaflet', colnames(df$data)) # used to hide geometry and leaflet_id columns

      DT::datatable(
        df$data,
        options = list(scrollY="300px",
                       pageLength = 5,
                       columnDefs = list(list(visible=FALSE, targets=n))),
        # could support multi but do single for now
        selection = "single",
        height = 300,
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
            sf::st_geometry(df$data[selected,]) <- sf::st_geometry(
              mapedit:::st_as_sfc.geo_list(evt)
            )
            df$data[selected,]$leaflet_id <- evt$properties$`_leaflet_id`
          }
        })
    }

    addDrawObserve(EVT_DRAW)
    addDrawObserve(EVT_EDIT)


    # ensure no rows are selected when adding new row
    # observeEvent(input$new, {
    #   if (input$new){
    #     proxy %>% selectRows(NULL)
    #   }
    # })

    # zoom to if feature available on selected row
    observeEvent(
      input$tbl_rows_selected,
      {
        selected <- input$tbl_rows_selected
        if(!is.null(selected)) {
          rowsel <- df$data[selected, ]
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
      df$data <- editData(df$data, input$tbl_cell_edit, 'tbl')
    })

    # provide mechanism to return after all done
    observeEvent(input$donebtn, {

      stopApp({
        # ensure export is sf and correct crs
        out <- st_sf(df$data,crs=4326)

        # clean bounding box just in case
        attr(st_geometry(out), "bbox") <- st_bbox(st_union(out$geometry))

        out
      })
    })

  }

  return(runApp(shinyApp(ui,server), launch.browser = TRUE))
}



# let's act like breweries does not have geometries and select the top 2
data <- data.frame(
  name = c('SiteA', 'SiteB'),
  type = factor(c('park', 'zoo'), levels = c('park', 'factory', 'zoo', 'warehouse')),
  size = c(35, 45)
)

data_sf <- make_an_sf(data)

mapview(data_sf)




