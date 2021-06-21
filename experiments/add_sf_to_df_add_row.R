library(leaflet)
library(mapview)
library(mapedit)
library(sf)
library(DT)
library(shiny)
library(shinyWidgets)
library(htmltools)
library(tmaptools)
library(dplyr)



#' @title Geo Attributes
#'
#' @description Geo Attributes launches a `shiny` application where you can add and edit spatial geometry
#' and attributes. Starting with a `data.frame` or an `sf data.frame`, a list of `sf data.frames` or nothing
#' at all. You can add columns, and rows and geometry for each row. Clicking on a row with geometry you can
#' zoom across the map between features.
#'
#' When you are done, your edits are saved to an `sf data.frame` for
#' use in R or to be saved to formats such as `geojson`.
#'
#' The application can dynamically handle: character, numeric, integer, factor and date fields.
#'
#' When the input data set is an `sf data.frame` the map automatically zooms to the extent of the `sf` object.
#' When the input has no spatial data, you must tell the function where to zoom. The function uses
#' \link{\code{tmaptools::geocode_OSM}} to identify the coordinates of your area of interest.
#'
#' @param dat input data source, can be a `data.frame` or an `sf data.frame`, or it can be left empty.
#' When nothing is passed to `dat` a basic `data.frame` is generated with `id` and `comment` fields.
#' @param zoomto character area of interest. The area is defined using \link{\code{tmaptools::geocode_OSM}},
#' which uses \link{OSM Nominatim}{https://nominatim.org/}. The area can be as ambiguous as a country, or
#' as specific as a street address. You can test the area of interest using the application or the example
#' code below.
#' @param col_add boolean option to add columns. Set to false if you don't want to allow a user to modify
#' the data structure.
#' @param reset boolean option to reset attribute input. Set to false if you don't want the attribute input to
#' reset to NA after each added row.
#' @import sf
#' @import leaflet
#' @import mapview
#' @import mapedit
#' @import dplyr
#' @import DT
#' @import shiny
#' @import htmltools
#' @importFrom shinyWidgets actionButtn show_alert useSweetAlert
#' @importFrom tmaptools geocode_OSM
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # with no input
#' data_sf <- geo_attributes(zoomto = 'germany')
#'
#' # a data.frame input
#' dat <- data.frame(name = c('SiteA', 'SiteB'),
#'                   type = factor(c('park', 'zoo'), levels = c('park', 'factory', 'zoo', 'warehouse')),
#'                   size = c(35, 45))
#'
#' data_sf <- geo_attributes(dat, zoomto = 'berlin')
#'
#' # an sf data.frame input
#' data_sf <- geo_attributes(data_sf)
#'
#' # test zoomto area of interest
#' zoomto_area <- tmaptools::geocode_OSM('paris')
#' mapview(st_as_sfc(zoomto_area$bbox))
#'
#' }
geo_attributes <- function(dat, zoomto = NULL, col_add = TRUE, reset = TRUE){


  if (missing(dat)) {
    dat <- data.frame(id = 'CHANGE ME', comments = 'ADD COMMENTS...') %>% mutate(leaf_id = 1)
  }

  APP_CRS <- 4326

  # trying to accept list of sf data.frames with multiple geom types
  # (TODO: works but original geom continues to display. method works nicely except if editing replacing existing geoms)
  original_sf <- NULL
  if (all(class(dat) == 'list')) {
    original_sf <- lapply(dat, function(df){
      df %>% mutate(leaf_id = 1:nrow(df))
    })
    dat <- bind_rows(dat) %>% mutate(leaf_id = 1:nrow(.))
  }

  if (!('sf' %in% class(dat))) {
    assertthat::assert_that(!(is.null(zoomto)),
                            msg = 'If your input is a non-spatial data.frame you must define a zoomto location')
  }

  if (!is.null(zoomto)) {
    zoomto_area <- tmaptools::geocode_OSM(zoomto)
    zoomto <- st_as_sfc(zoomto_area$bbox) %>% st_sf() %>% st_set_crs(APP_CRS)
  }


  ui <- tagList(
    # script_zoom,
    useSweetAlert(),
    fluidPage(
      fluidRow(
        column(12, editModUI("map"))
      ),
      tags$hr(),
      fluidRow(
        column(ifelse(col_add, 6, 9),
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
        {if (col_add) {
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
               )} else {
                 NULL
               }
               }
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

    if (all(class(dat) == 'data.frame')) {
      dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
      data_copy <- st_as_sf(
        dat,
        geometry = st_sfc(lapply(seq_len(nrow(dat)),function(i){st_point()}))
      ) %>% st_set_crs(APP_CRS)

    } else if ('sf' %in% class(dat)) {

      dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
      data_copy <- dat # TODO check orig crs and transform to 4326

    }

    df <- reactiveValues(types = sapply(dat, class),
                         data = data_copy,
                         zoom_to = zoomto)

    #need to somehow update the list 'original_sf' on the fly
    # making the original_sf list reactive to the df$data seems to work
    # still testing...

    sf_reac <- reactive(df$data)


    observe({
      edits <- callModule(
        editMod,
        leafmap = {
          if ('sf' %in% class(dat)){
          print('sf')
          print(df$data)
          mapv <- leaflet() %>%
            addProviderTiles("OpenStreetMap") %>%
            leafem::addFeatures(data = sf_reac(),
                                layerId = sf_reac()$leaf_id,
                                group = 'editLayer',
                                popup = leafpop::popupTable(sf_reac()))
        } else {
          mapv <- mapview(df$zoom_to)@map %>%
            leaflet::hideGroup('df$zoom_to')
        }
          mapv
        },
        id = "map",
        targetLayerId = 'editLayer',
        sf = TRUE
      )
    })


    proxy_map <- leaflet::leafletProxy('map-map', session)

    observeEvent(input$col_add, {

      if (nchar(input$new_name)==0) {

        shinyWidgets::show_alert('Missing Column Name',
                                 'this column is missing a name, this must be entered before adding a column',
                                 type = 'warning')
      } else {
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
      }
                  })


    #create a vector input for 'row_add'
    EVT_ADD_ROW <- "row_add"

    # determines whether to use 'row_add' or 'map_draw_feature'
    # also, if rows are selected then it won't trigger the 'map_draw_feature'

    addRowOrDrawObserve <- function(event, id) {
      observeEvent(
        if(is.na(id)){

          input[[event]]

        } else {

        input[[nsm(event, id = id)]]},{

          if(!is.null(input$tbl_rows_selected)){

          } else {

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
      if(isTRUE(reset)){
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
      }
          }
    })
    }

    addRowOrDrawObserve(EVT_ADD_ROW, id = NA)
    addRowOrDrawObserve(EVT_DRAW, id = 'map')



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

      n <- grep('leaf_id|geom', colnames(df$data)) # used to hide geometry/leaf_id column

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

           # get selected row section
          # pretty nasty if/else going on... not sure how to clean-up?


          # this allows the user to edit geometries or delete and then save without selecting row.
          # you can also select row and edit/delete as well but this gives the ability to not do so.
          # a for-loop maybe not so good for unpacking large sf/df but for now it's fine for poc

          if(event == EVT_DELETE) {

            ids <- vector()
            if('sf' %in% class(dat)){

              for(i in 1:length(evt$features)){

                iter <- evt$features[[i]]$properties[['layerId']]

                ids <- append(ids, iter)
              }
            } else {

            for(i in 1:length(evt$features)){

              iter <- evt$features[[i]]$properties[['_leaflet_id']]

              ids <- append(ids, iter)
            }

            }

            df$data <- filter(df$data, !df$data$leaf_id %in% ids)

          } else if (event == EVT_EDIT) {

            for(i in 1:length(evt$features)){

               evt_type <- evt$features[[i]]$geometry$type
               leaf_id <- evt$features[[i]]$properties[['_leaflet_id']]
               geom <- unlist(evt$features[[i]]$geometry$coordinates)

               if (evt_type == 'Point') {

                 sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- st_sfc(st_point(geom))

               } else if (evt_type == 'Polygon'){

                 geom <- matrix(geom, ncol = 2, byrow = T)
                 sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- st_sfc(st_polygon(list(geom)))

               } else if (evt_type == 'LineString'){

                 geom <- matrix(geom, ncol = 2, byrow = T)

                 sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- st_sfc(st_linestring(geom))
               }

            }

          } else {

            # below just determines whether to use 'row_add' or 'map_draw_feature' for adding geometries

          if(!is.null(input$tbl_rows_selected)) {

            selected <- isolate(input$tbl_rows_selected)


          }  else if (event == EVT_DRAW){

            selected <- length(input$tbl_rows_all) + 1

          }

          skip = FALSE
          # ignore if selected is null
          #  not great but good enough for poc
          if(is.null(selected)) {skip = TRUE}

          # replace if draw or edit
          if(skip==FALSE) {
            sf::st_geometry(df$data[selected,]) <- sf::st_geometry(
              mapedit:::st_as_sfc.geo_list(evt))

              #adding the leaf_id when we draw or row_add
              df$data[selected, 'leaf_id'] <- as.integer(evt$properties[['_leaflet_id']])
          }



          }
        })
    }

    addDrawObserve(EVT_DRAW)
    addDrawObserve(EVT_EDIT)
    addDrawObserve(EVT_DELETE)

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
                leaflet::flyTo(lng = pnt$X, lat = pnt$Y, zoom = 14)

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

      if(grepl(class(df$data$geometry)[[1]], "sfc_GEOMETRY")){

        if (any(st_is_empty(df$data$geometry))) {
          shinyWidgets::show_alert('Missing Geometry',
                                   'some features are missing geometry, these must be entered before saving',
                                   type = 'warning')
        } else {
          stopApp({

        out <- df$data %>% dplyr::select(-leaf_id) %>%
          dplyr::mutate(geo_type = as.character(st_geometry_type(.)))

        out <- st_sf(out, crs = APP_CRS)
        out <- split(out , f = out$geo_type)

        # clean bounding box just in case
        for(i in 1:length(out)){
          attr(st_geometry(out[[i]]), "bbox") <- st_bbox(st_union(out[[i]]$geometry))
        }

        out

        })
        }

      } else {

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

          out %>% dplyr::select(-leaf_id)
        })
      }
   }
    })

  }

  return(runApp(shinyApp(ui,server)))
}



# let's create a fake site list
data <- data.frame(
  name = c('SiteA', 'SiteB'),
  type = factor(c('park', 'zoo'), levels = c('park', 'factory', 'zoo', 'warehouse')),
  size = c(35, 45),
  stringsAsFactors = FALSE
)

data_sf2 <- geo_attributes(data, zoomto = 'Montana', col_add = T)
sf_pts <- geo_attributes(data_sf2, zoomto = 'Montana', col_add = T)

mapview(data_sf2)
mapview(sf_pts)
mapview(geom_save)
