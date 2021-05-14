library(leaflet)
library(mapview)
library(mapedit)
library(sf)
library(DT)
library(shiny)
library(shinyWidgets)
library(htmltools)
library(tmaptools)




make_an_sf <- function(dat, zoomto = NULL){

  if (missing(dat)) {
    dat <- data.frame(id = 'CHANGE ME', comments = 'ADD COMMENTS...')
  }

  APP_CRS <- 4326

  if (!('sf' %in% class(dat))) {
    assertthat::assert_that(!(is.null(zoomto)),
                            msg = 'If your input is a non-spatial data.frame you must define a zoomto location')
  }

  if (!is.null(zoomto)) {
    zoomto_area <- tmaptools::geocode_OSM(zoomto)
    zoomto <- st_as_sfc(zoomto_area$bbox) %>% st_sf() %>% st_set_crs(APP_CRS)
  }


  ui <- tagList(
    script_zoom,
    useSweetAlert(),
    fluidPage(
      fluidRow(
        column(12, editModUI("map"))
      ),
      tags$hr(),
      fluidRow(
        column(6,
               DT::dataTableOutput("tbl",width="100%", height=200)),
        column(3,
               wellPanel(
                 h3('Add New Row'),
                 uiOutput('dyn_form'),
                 actionBttn("row_add", "Row",
                              icon = icon('plus'),
                              style = 'material-flat',
                              block = TRUE,
                              color = 'primary',
                              size = 'md'))
               ),
        column(3,
               wellPanel(
                 h3('Add New Column'),
                 shiny::textInput('new_name', 'New Column Name', width = '100%'),
                 shiny::radioButtons('new_type', 'Column Type', choices = c('character', 'numeric', 'integer', 'Date')),
                 actionBttn("col_add", "Column",
                            icon = icon('plus'),
                            style = 'material-flat',
                            block = TRUE,
                            color = 'primary',
                            size = 'md'))
               )
      ),
      fluidRow(tags$hr(),
               div(style = 'padding: 20px',
                 actionBttn("donebtn", "Done",
                          icon = icon('check-circle'),
                          style = 'material-flat',
                          block = TRUE,
                          color = 'success',
                          size = 'lg')))

    )
  )

  server <- function(input, output, session) {

    if (class(dat) == 'data.frame') {

      data_copy <- st_as_sf(
        dat,
        geometry = st_sfc(lapply(seq_len(nrow(dat)),function(i){st_point()}))
      ) %>% st_set_crs(APP_CRS)

    } else if ('sf' %in% class(dat)) {

      data_copy <- dat # TODO check orig crs and transform to 4326

    }

    df <- reactiveValues(types = sapply(dat, class),
                         data = data_copy,
                         zoom_to = zoomto)



    observeEvent(input$col_add, {

                  #TODO: add checks for missing inputs
                  add_col <- df$data

                  add_col[[input$new_name]] <- do.call(paste0('as.', input$new_type), list(NA))

                  df$data <- add_col

                  # add input$new_name to df$type
                  ntype <- input$new_type
                  names(ntype) <- input$new_name

                  df$types <- c(df$types, ntype)

                  updateTextInput(session, 'new_name', value = NA)
                  showNotification('Added New Column')

                  })

    observeEvent(input$row_add, {

      # creates first column and row (must be more elegant way)
      new_row <- data.frame(X = input[[names(df$types[1])]])
      colnames(new_row) <- names(df$types[1])

      # remaining columns will be correct size
      for (i in 2:length(df$types)) {
        new_row[names(df$types[i])] <- input[[names(df$types[i])]]
      }

      new_row <- st_as_sf(new_row, geometry = st_sfc(st_point()), crs = APP_CRS)

      # add to data_copy data.frame and update visible table
      df$data <- df$data %>%
        rbind(new_row)

      showNotification('Added New Row')


      # reset input table
      for (i in 1:length(df$types)) {
        typ <- df$types[i]
        nm <- names(typ)

        if (typ == 'character') {
          updateTextInput(session, nm, value = NA)
        } else if (typ %in% c('numeric','integer')) {
          updateNumericInput(session, nm, value = NA)
        } else if (typ == 'Date') {
          updateDateInput(session, nm, value = NA)
        }

      }
    })


    observe({
      edits <- callModule(
      editMod,
      leafmap = {if (is.null(df$zoom_to)){
                   mapv <- mapview(df$data)@map
                 } else {
                   mapv <- mapview(df$zoom_to)@map %>%
                     leaflet::hideGroup('df$zoom_to')
                 }
                 mapv
        },
      id = "map"
      )
      })

    proxy_map <- leaflet::leafletProxy('map-map', session)

    observe({

      output$dyn_form <- renderUI({

        tagList(
          lapply(1:length(df$types), function(n){
            name <- names(df$types[n])
            label <- paste0(names(df$types[n]), ' (', df$types[n], ')')
            if (df$types[n] == 'character') {
              textInput(name, label, width = '100%')
            } else if (df$types[n] == 'factor') {
              selectInput(name, label, width = '100%',
                          choices = levels(dat[[names(df$types[n])]]),
                          selected = NULL,
                          selectize = TRUE)
            } else if (df$types[n] %in% c('numeric','integer')) {
              numericInput(name, label, width = '100%', value = NA)
            } else if (df$types[n] == 'Date') {
              dateInput(name, label, width = '100%', value = NA)
            }
          })

        )


      })

    })

    output$tbl <- DT::renderDataTable({

      n <- grep('geom', colnames(df$data)) # used to hide geometry column

      DT::datatable(
        df$data,
        options = list(scrollY="200px",
                       pageLength = 5,
                       columnDefs = list(list(visible=FALSE, targets=n))),
        # could support multi but do single for now
        selection = "single",
        height = 200,
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
          }
        })
    }

    addDrawObserve(EVT_DRAW)
    addDrawObserve(EVT_EDIT)

    # zoom to if feature available on selected row
    observeEvent(
      input$tbl_rows_selected,
      {
        selected <- input$tbl_rows_selected
        if(!is.null(selected)) {
          rowsel <- df$data[selected, ]
          # simple check to see if feature available
          #   and leaflet id populated
          if (all(!is.na(sf::st_coordinates(sf::st_geometry(rowsel)[[1]])))) {

            if (st_geometry_type(rowsel) == 'POINT') {

              pnt <- st_coordinates(rowsel) %>% as.data.frame()
              proxy_map %>%
                leaflet::setView(lng = pnt$X, lat = pnt$Y, zoom = 16)

            } else {

              bb <- st_bbox(sf::st_geometry(rowsel))
              proxy_map %>%
                flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])

            }

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

      if (any(st_is_empty(df$data$geometry))) {
        shinyWidgets::show_alert('Missing Geometry',
                                 'some features are missing geometry, these must be entered before saving',
                                 type = 'warning')
      } else {
        stopApp({
          # ensure export is sf and correct crs
          out <- st_sf(df$data,crs=APP_CRS)

          # clean bounding box just in case
          attr(st_geometry(out), "bbox") <- st_bbox(st_union(out$geometry))

          out
        })
      }



    })

  }

  return(runApp(shinyApp(ui,server)))
}



# let's act like breweries does not have geometries and select the top 2
data <- data.frame(
  name = c('SiteA', 'SiteB'),
  type = factor(c('park', 'zoo'), levels = c('park', 'factory', 'zoo', 'warehouse')),
  size = c(35, 45)
)

data_sf <- make_an_sf(data, zoomto = 'germany')

mapview(data_sf)




