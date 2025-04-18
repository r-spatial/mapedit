#' @title Edit Feature Attributes
#'
#' @description Launches a `shiny` application where you can add and edit spatial geometry
#' and attributes. Geometry is created or edited within the interactive map, while feature attributes
#' can be added to and edited within the editable table.
#'
#' Starting with a `data.frame` or an `sf data.frame`, a list of `sf data.frames` or nothing
#' at all. You can add columns, and rows and geometry for each row. Clicking on a row with geometry you can
#' zoom across the map between features.
#'
#' When you are done, your edits are saved to an `sf data.frame` for
#' use in R or to be saved to anyformat you wish via \link[sf]{st_write}.
#'
#' The application can dynamically handle: character, numeric, integer, factor and date fields.
#'
#' When the input data set is an `sf data.frame` the map automatically zooms to the extent of the `sf` object.
#'
#' When the input has no spatial data, you must tell the function where to zoom. The function uses
#' \link[tmaptools]{geocode_OSM} to identify the coordinates of your area of interest.
#'
#' @param dat input data source, can be a `data.frame` or an `sf data.frame`, or it can be left empty.
#' When nothing is passed to `dat` a basic `data.frame` is generated with `id` and `comment` fields.
#' @param zoomto character area of interest. The area is defined using \link[tmaptools]{geocode_OSM},
#' which uses \href{https://nominatim.org/}{OSM Nominatim}. The area can be as ambiguous as a country, or
#' as specific as a street address. You can test the area of interest using the application or the example
#' code below.
#' @param col_add boolean option to enable add columns form. Set to false if you don't want to allow a user to modify
#' the data structure.
#' @param reset boolean option to reset attribute input. Set to false if you don't want the attribute input to
#' reset to NA after each added row. Use this option when features share common attributes
#' @param provider A character string indicating the provider tile of choice, e.g. 'Esri.WorldImagery' (default)
#'
#' @note Editing of feature geometries does not work for multi-geometry inputs. For this use case it is advisable to
#' split the data set by geometry type and edit separately
#'
#' @import sf
#' @import leaflet
#' @import mapview
#' @import leafem
#' @import leafpop
#' @import dplyr
#' @import shiny
#' @import htmltools
#' @import DT
#' @importFrom shinyWidgets actionBttn show_alert useSweetAlert
#' @importFrom tmaptools geocode_OSM
#'
#' @return sf data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # with no input
#' data_sf <- editAttributes(zoomto = 'germany')
#'
#' # a data.frame input
#' dat <- data.frame(name = c('SiteA', 'SiteB'),
#'                   type = factor(c('park', 'zoo'), levels = c('park', 'factory', 'zoo', 'warehouse')),
#'                   size = c(35, 45))
#'
#' data_sf <- editAttributes(dat, zoomto = 'berlin')
#'
#' # an sf data.frame input
#' data_sf <- editAttributes(data_sf)
#'
#' # test zoomto area of interest
#' zoomto_area <- tmaptools::geocode_OSM('paris')
#' mapview(st_as_sfc(zoomto_area$bbox))
#'
#' }
editAttributes <- function(dat, zoomto = NULL, col_add = TRUE, reset = TRUE, provider = 'Esri.WorldImagery', testing = FALSE){

  DEFAULT_ZOOM <- 'africa'
  MSG <- 'When neither sf object nor zoomto is default, map will zoom to Africa'

  #create base df if dat missing
  if (missing(dat)) {
    dat <- data.frame(id = 'CHANGE ME', comments = 'ADD COMMENTS...') %>% mutate(leaf_id = 1)
    if (is.null(zoomto)) {
      message(MSG)
      zoomto <- DEFAULT_ZOOM
    }
  }

  APP_CRS <- 4326

  # Need to parse out spatial objects if input data is spatial
  type <- c('sf', 'SpatVector')

  # accept list of sf data.frames with multiple geom types
  original_sf <- NULL
  if (all(class(dat) == 'list')) {
    original_sf <- lapply(dat, function(df){
      df %>% mutate(leaf_id = 1:nrow(df))
    })
    dat <- bind_rows(dat) %>% mutate(leaf_id = 1:nrow(.))
  }

  if (all(class(dat) == 'data.frame')) {
    dat <- dat %>% mutate(leaf_id = 1:nrow(dat))
    data_copy <- sf::st_as_sf(
      dat,
      geometry = sf::st_sfc(lapply(seq_len(nrow(dat)),function(i){sf::st_point()}))
    ) %>% sf::st_set_crs(APP_CRS)

    user_crs <- APP_CRS
    le = TRUE

    if (is.null(zoomto)) {
      message(MSG)
      zoomto <- DEFAULT_ZOOM
    }

   } else if (any(type %in% class(dat))) {

    dat <- dat %>% mutate(leaf_id = 1:nrow(dat)) %>% sf::st_transform(APP_CRS)
    data_copy <- dat # TODO check orig crs and transform to 4326

    if(is.na(sf::st_crs(dat))){dat <- dat %>% sf::st_set_crs(APP_CRS)}
    if(class(dat)[[1]] == 'SpatVector'){dat <- sf::st_as_sf(dat)}
    #if(class(dat)[[1]] == 'sf'){class(dat) <- c('sf', 'data.frame')}

    user_crs <- sf::st_crs(dat)

    # this is used to make sure the edit toolbar is disabled when these are inputs
    # if not, then the app will hang and requires ending task.
    le <- !any(sf::st_geometry_type(dat) %in% c('MULTILINESTRING', 'MULTIPOLYGON'))

  } else if (!any(type %in% class(dat))) {
    assertthat::assert_that(!(is.null(zoomto)),
                            msg = 'If your input is a non-spatial data.frame you must define a zoomto location')
  }

  # if data or empty (dat) need a zoom to place
  if (!is.null(zoomto)) {
    zoomto_area <- tmaptools::geocode_OSM(zoomto)
    zoomto <- sf::st_as_sfc(zoomto_area$bbox) %>% sf::st_sf() %>% sf::st_set_crs(APP_CRS)
  }


  ui <- tagList(
    shinyWidgets::useSweetAlert(),
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(12, editModUI("map"))
      ),
      tags$hr(),
      shiny::fluidRow(
        shiny::column(ifelse(col_add, 6, 9),
               DT::dataTableOutput("tbl",width="100%", height=200)),
        shiny::column(3,
                      shiny::wellPanel(
                        shiny::h3('Add New Row'),
                        shiny::uiOutput('dyn_form'),
                        shinyWidgets::actionBttn("row_add", "Row",
                                                 icon = shiny::icon('plus'),
                                                 style = 'material-flat',
                                                 block = TRUE,
                                                 color = 'primary',
                                                 size = 'md'))
        ),
        {if (col_add) {
          shiny::column(3,
                        shiny::wellPanel(
                          shiny::h3('Add New Column'),
                          shiny::textInput('new_name', 'New Column Name', width = '100%'),
                          shiny::radioButtons('new_type', 'Column Type', choices = c('character', 'numeric', 'integer', 'Date')),
                          shinyWidgets::actionBttn("col_add", "Column",
                                                   icon = shiny::icon('plus'),
                                                   style = 'material-flat',
                                                   block = TRUE,
                                                   color = 'primary',
                                                   size = 'md'))
          )} else {
            NULL
          }
        }
      ),
      shiny::fluidRow(tags$hr(),
                      shiny::div(style = 'padding: 20px',
                                 shinyWidgets::actionBttn("donebtn", "Done",
                                                          icon = shiny::icon('check-circle'),
                                                          style = 'material-flat',
                                                          block = TRUE,
                                                          color = 'success',
                                                          size = 'lg')))

    )
  )

  server <- function(input, output, session) {

    # gather all data into reactiveValues
    df <- shiny::reactiveValues(types = sapply(dat, class),
                                data = data_copy,
                                zoom_to = zoomto,
                                edit_logic = le)

    # mapedit module
    shiny::observe({

      edits <- shiny::callModule(
        module = editMod,
        leafmap = {

          if (any(type %in% class(dat))){

            mapv <- leaflet::leaflet() %>%
              leaflet::addProviderTiles(provider = provider,
                                        group = provider) %>%
              leaflet::addLayersControl(baseGroups = provider,
                                        position = 'topleft') %>%
              leafem::addFeatures(data = df$data,
                                  layerId = df$data$leaf_id,
                                  group = 'editLayer',
                                  popup = leafpop::popupTable(df$data))
          } else {
            mapv <- mapview::mapview(df$zoom_to,
                                     map.types = provider)@map %>%
              leaflet::hideGroup('df$zoom_to') %>%
              leafem::addFeatures(data = df$data,
                                  layerId = df$data$leaf_id,
                                  group = 'editLayer',
                                  popup = leafpop::popupTable(df$data))
          }
          mapv
        },
        id = "map",
        targetLayerId = 'editLayer',
        sf = TRUE,
        editorOptions = list(editOptions = leaflet.extras::editToolbarOptions(edit = df$edit_logic)),
      )
    })

    #make a proxy map
    proxy_map <- leaflet::leafletProxy('map-map', session)

    # watch for NEW COLUMN button clicks
    shiny::observeEvent(input$col_add, {

      if (nchar(input$new_name)==0) {
        shinyWidgets::show_alert('Missing Column Name',
                                 'this column is missing a name, this must be entered before adding a column',
                                 type = 'warning')
      } else {

        add_col <- df$data
        add_col[[input$new_name]] <- do.call(paste0('as.', input$new_type), list(NA))

        df$data <- add_col
        ntype <- input$new_type
        names(ntype) <- input$new_name
        df$types <- c(df$types, ntype)

        shiny::updateTextInput(session, 'new_name', value = NA)
        shiny::showNotification('Added New Column')
      }
    })


    # render new row form based on the existing data structure
    shiny::observe({

      output$dyn_form <- shiny::renderUI({

        shiny::tagList(
          lapply(1:length(df$types), function(n){
            name <- names(df$types[n])
            label <- paste0(names(df$types[n]), ' (', df$types[n], ')')
            if (df$types[n] == 'character') {
              shiny::textInput(name, label, width = '100%')
            } else if (df$types[n] == 'factor') {
              shiny::selectInput(name, label, width = '100%',
                                 choices = levels(dat[[names(df$types[n])]]),
                                 selected = NULL,
                                 selectize = TRUE)
            } else if (df$types[n] %in% c('numeric','integer')) {
              shiny::numericInput(name, label, width = '100%', value = NA)
            } else if (df$types[n] == 'Date') {
              shiny::dateInput(name, label, width = '100%', value = NA)
            }
          }),
          # we don't want to see this element but it is needed to form data structure
          htmltools::tags$script("document.getElementById('leaf_id-label').hidden = true; document.getElementById('leaf_id').style.visibility = 'hidden';")
        )

      })
    })

    # render editable data table
    output$tbl <- DT::renderDataTable({

      n <- grep('leaf_id|geom', colnames(df$data)) # used to hide geometry/leaf_id column

      DT::datatable(
        df$data,
        options = list(scrollY="200px",
                       pageLength = 50,
                       scrollX = TRUE,
                       columnDefs = list(list(visible=FALSE, targets=n))),
        # could support multi but do single for now
        selection = "single",
        height = 200,
        editable = TRUE,
      )
    })

    proxy = DT::dataTableProxy('tbl')


    # modify namespace to get map ID
    nsm <- function(event="", id="map") {
      paste0(session$ns(id), "-", event)
    }

    # unfortunately I did not implement last functionality
    # for editMap, so do it the hard way
    # last seems useful, so I might circle back and add that
    EVT_DRAW <- "map_draw_new_feature"
    EVT_EDIT <- "map_draw_edited_features"
    EVT_DELETE <- "map_draw_deleted_features"

    #create a vector input for 'row_add'
    EVT_ADD_ROW <- "row_add"

    # determines whether to use 'row_add' or 'map_draw_feature'
    # also, if rows are selected then it won't trigger the 'map_draw_feature'
    addRowOrDrawObserve <- function(event, id) {
      shiny::observeEvent(
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

              new_row <- sf::st_as_sf(new_row, geometry = sf::st_sfc(sf::st_point()), crs = APP_CRS)

              suppressWarnings({
                # add to data_copy data.frame and update visible table
                df$data <- df$data %>%
                  rbind(new_row)
              })

              # reset input table
              if(isTRUE(reset)){
                for (i in 1:length(df$types)) {
                  typ <- df$types[i]
                  nm <- names(typ)

                  if (typ == 'character') {
                    shiny::updateTextInput(session, nm, value = NA)
                  } else if (typ %in% c('numeric','integer')) {
                    shiny::updateNumericInput(session, nm, value = NA)
                  } else if (typ == 'Date') {
                    shiny::updateDateInput(session, nm, value = NA)
                  }

                }
              }
            }
          })
    }

    addRowOrDrawObserve(EVT_ADD_ROW, id = NA)
    addRowOrDrawObserve(EVT_DRAW, id = 'map')

    addDrawObserve <- function(event) {
      shiny::observeEvent(
        input[[nsm(event)]],
        {
          evt <- input[[nsm(event)]]

          # this allows the user to edit geometries or delete and then save without selecting row.
          # you can also select row and edit/delete as well but this gives the ability to not do so.
          if(event == EVT_DELETE) {

            ids <- vector()

            for(i in 1:length(evt$features)){
              iter <- evt$features[[i]]$properties[['layerId']]
              ids <- append(ids, iter)
            }

            df$data <- dplyr::filter(df$data, !df$data$leaf_id %in% ids)
            df$ids <- ids

          } else if (event == EVT_EDIT) {

            for(i in 1:length(evt$features)){

              evt_type <- evt$features[[i]]$geometry$type
              leaf_id <- evt$features[[i]]$properties[['layerId']]
              geom <- unlist(evt$features[[i]]$geometry$coordinates)

              if (evt_type == 'Point') {
                sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_point(geom))
              } else if (evt_type == 'Polygon'){
                geom <- matrix(geom, ncol = 2, byrow = T)
                sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_polygon(list(geom)))
              } else if (evt_type == 'LineString'){
                geom <- matrix(geom, ncol = 2, byrow = T)
                sf::st_geometry(df$data[df$data$leaf_id %in% leaf_id,]) <- sf::st_sfc(sf::st_linestring(geom))
              }
            }

          } else {

            # below determines whether to use 'row_add' or 'map_draw_feature' for adding geometries
            if(!is.null(input$tbl_rows_selected)) {
              selected <- shiny::isolate(input$tbl_rows_selected)
            }  else if (event == EVT_DRAW){
              selected <- length(input$tbl_rows_all) + 1
            }

            skip = FALSE

            # ignore if selected is null
            if(is.null(selected)) {skip = TRUE}

            # replace if draw or edit
            if(skip==FALSE) {
              sf::st_geometry(df$data[selected,]) <- sf::st_geometry(
                st_as_sfc.geo_list(evt))

              #adding the leaf_id when we draw or row_add
              df$data[selected, 'leaf_id'] <- as.integer(evt$properties[['_leaflet_id']])

            }
          }
        })
    }

    addDrawObserve(EVT_DRAW)
    addDrawObserve(EVT_EDIT)
    addDrawObserve(EVT_DELETE)

    # this is used to keep the zoom of leaflet relevant
    shiny::observeEvent(input[[nsm(EVT_DRAW)]],{

      click <- input[[nsm('map_draw_new_feature')]]

      if (click$geometry$type == 'Point') {

        clat <- click$geometry$coordinates[[2]]
        clng <- click$geometry$coordinates[[1]]
        proxy_map  %>%
          leaflet::setView(lng = clng, lat = clat, zoom = input[[nsm('map_zoom')]])

      } else {

        click_mat <- matrix(unlist(click$geometry$coordinates),ncol=2, byrow=TRUE)

        if(click$geometry$type == 'LineString'){
          clat <- click_mat[[1,2]]
          clng <- click_mat[[1,1]]
          proxy_map %>%
            leaflet::setView(lng = clng, lat = clat, zoom = input[[nsm('map_zoom')]])
        } else {
          bb <- sf::st_bbox(sf::st_geometry(sf::st_polygon(x = list(click_mat))))
          proxy_map %>%
            leaflet::fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])

        }
      }
    })

    # zoom to if feature available on selected row, same as above but with DT selected rows
    shiny::observeEvent(
      input$tbl_rows_selected,
      {
        selected <- input$tbl_rows_selected

        if(!is.null(selected)) {
          rowsel <- df$data[selected, ]
          # simple check to see if feature available
          #   and leaflet id populated
          if (all(!is.na(sf::st_coordinates(sf::st_geometry(rowsel)[[1]])))) {

            if (sf::st_geometry_type(rowsel) == 'POINT') {
              pnt <- sf::st_coordinates(rowsel) %>% as.data.frame()
              proxy_map %>%
                leaflet::flyTo(lng = pnt$X, lat = pnt$Y, zoom = input[[nsm('map_zoom')]])
            } else {
              bb <- st_bbox(sf::st_geometry(rowsel))
              proxy_map %>%
                leaflet::flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
            }

          }
        }
      }
    )

    # update table cells with double click on cell
    shiny::observeEvent(input$tbl_cell_edit, {

      df$data <- DT::editData(df$data, input$tbl_cell_edit, 'tbl', resetPaging = F)
      DT::replaceData(proxy, df$data, rownames = FALSE, resetPaging = FALSE)

    })

    # provide mechanism to return after all done
    shiny::observeEvent(input$donebtn, {

      if (testing) shiny::stopApp()

      if(grepl(class(df$data$geometry)[[1]], "sfc_GEOMETRY")){

        if (any(sf::st_is_empty(df$data$geometry))) {
          shinyWidgets::show_alert('Missing Geometry',
                                   'some features are missing geometry, these must be entered before saving',
                                   type = 'warning')
        } else {
          shiny::stopApp({
            out <- df$data %>% dplyr::select(-leaf_id) %>%
              dplyr::mutate(geo_type = as.character(sf::st_geometry_type(.)))
            out <- sf::st_sf(out, crs = user_crs)
            out <- split(out , f = out$geo_type)

            # clean bounding box just in case
            for(i in 1:length(out)){
              attr(sf::st_geometry(out[[i]]), "bbox") <- sf::st_bbox(sf::st_union(out[[i]]$geometry))
            }

            out

          })
        }

      } else {

        if (any(sf::st_is_empty(df$data$geometry))) {
          shinyWidgets::show_alert('Missing Geometry',
                                   'some features are missing geometry, these must be entered before saving',
                                   type = 'warning')
        } else {
          shiny::stopApp({
            # ensure export is sf and same as input crs
            out <- sf::st_sf(df$data,crs=user_crs)

            # clean bounding box just in case
            attr(sf::st_geometry(out), "bbox") <- sf::st_bbox(sf::st_union(out$geometry))
            out %>% dplyr::select(-leaf_id)
          })
        }
      }
    })

  }

  # this allows shinytest to record
  if (testing) {
    return(shiny::shinyApp(ui,server))
  } else {
    return(shiny::runApp(shiny::shinyApp(ui,server)))
  }


}



