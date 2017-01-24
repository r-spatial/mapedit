library(maps)
library(sp)
library(maptools)
library(sf)
library(dplyr)

library(leaflet)
library(plotly)
library(htmltools)
library(crosstalk)
library(shiny)

usa <- map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# convert to simple features
usa_sf <- st_as_sf(usa)
usa_sf$state <- unlist(unique(lapply(usa@polygons,function(x){x@ID})))
# add area from R builtin data state.area
usa_sf$area <- left_join(
  usa_sf,
  data.frame(
    state = tolower(state.name),
    area = state.area,
    stringsAsFactors = FALSE
  )
)$area

# check our simple features
plot(usa_sf)

# map states with leaflet
lf <- leaflet(usa_sf) %>%
  addPolygons(group = ~state)

# modify Shiny leaflet click event to include shift and ctrl key
tgs <- tagList(
  lf,
  htmlwidgets::onStaticRenderComplete(
"
var lf = HTMLWidgets.find('.leaflet').getMap();
var toggle_selected = function(group) {

};

var toggle_opacity = function(group) {
  // change opacity in a crude way
  //   assuming user just stuck with the defaults
  var layer = lf.layerManager.getLayerGroup(group);
  var opaqueness = 0.2;
  lf.layerManager
    .getLayerGroup(group)
    .eachLayer(function(x){opaqueness = Math.round(+$(x._path).css('fillOpacity')*10)/10})
  opaqueness = opaqueness === 0.2 ? 0.6 : 0.2;
  layer.setStyle({fillOpacity:opaqueness});
};

$(document).on(
  'shiny:inputchanged',
  function(e){
    // filter for click events
    if(/click/.test(e.name)) {
      toggle_opacity(e.value.group);
      e.value = Object.assign( e.value, {
        ctrlKey: event.ctrlKey,
        shiftKey: event.shiftKey
      })
    }
  }
)
"
  )
)

# set this up in .GlobalEnv for now
#   but would be better in a gadget
selections <- data.frame(
  state = usa_sf$state,
  selected = FALSE
)

shinyApp(
  tgs,
  function(input, output){
    # make sure we start with selected = FALSE
    selections$selected <<- FALSE

    id = "undefined"
    click_evt = paste0(id, "_shape_click")

    observeEvent(input[[click_evt]], {
      print(input[[click_evt]])
      selections[which(selections$state==input[[click_evt]]$group),"selected"] <<- !selections[which(selections$state==input[[click_evt]]$group),"selected"]
    })
  }
)


# ugly hack to try to achieve crosstalk support
#   crosstalk and leaflet don't support addPolygons

shinyApp(
  fluidPage(
    fluidRow(
      column(6,leafletOutput("leafmap")),
      column(6,plotlyOutput("plotly"))
    )
  ),
  function(input, output, session) {
    # use crosstalk SharedData with sf
    sd <- SharedData$new(usa_sf, key=~state, group="states")
    output$plotly <- renderPlotly({
      plot_ly(sd, x=~area, y=~state) %>% add_markers()
    })

    output$leafmap <- renderLeaflet({
      leaflet(sd) %>%
        addPolygons(group = ~state) %>%
        htmlwidgets::onRender(
"
function(el,x) {
  var ct_select = new crosstalk.SelectionHandle('states');

  ct_select.on('change', function(val){console.log(val)});

  var lf = this;

  // define our functions for toggling
  function toggle_opacity(group) {
    // change opacity in a crude way
    //   assuming user just stuck with the defaults
    var layer = lf.layerManager.getLayerGroup(group);
    var opaqueness = 0.2;
    if(layer.eachLayer) {
      layer.eachLayer(function(x){opaqueness = Math.round(+$(x._path).css('fillOpacity')*10)/10})
    }
    opaqueness = opaqueness === 0.2 ? 0.6 : 0.2;
    layer.setStyle({fillOpacity:opaqueness});
    return opaqueness;
  };

  function toggle_state(group) {
    var selected = ct_select.value;
    if(Array.isArray(selected) && selected.length > 0) {
      var new_selection = selected.slice();
      var loc = new_selection.indexOf(group);
      if(loc >= 0) {
        new_selection.splice(loc,1);
        //if(new_selection.length === 0) {
        //  ct_select.clear();
        //} else {
          ct_select.set(new_selection);
        //}
      } else {
        new_selection.push(group);
        ct_select.set(new_selection);
      }
    } else {
      ct_select.set([group]);
    }
  };

  // set up click handler on each layer
  lf.eachLayer(function(lyr){
    if(lyr.on && lyr.groupname) {
      lyr.on('click',function(e){
        var group = this.groupname;
        var selected = toggle_opacity(group) === 0.6;
        toggle_state(group, selected);
      })
    }
  });
}
"
        )
    })
  }
)
