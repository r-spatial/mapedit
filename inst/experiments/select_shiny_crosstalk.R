library(maps)
library(sp)
library(maptools)
library(purrr)
library(htmltools)
library(crosstalk)

usa <- map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# convert to simple features
usa_sf <- st_as_sf(usa)
usa_sf$state <- unique(lapply(usa@polygons,function(x){x@ID}))

# use crosstalk SharedData with sf
sd <- SharedData$new(usa_sf, key=~state, group="states")

# map states with leaflet
lf <- leaflet(sd) %>%
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
    .eachLayer(function(x){opaqueness = +$(x._path).css('fillOpacity')})
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

library(shiny)
# set this up in .GlobalEnv for now
#   but would be better in a gadget
selections <- list()
shinyApp(
  tgs,
  function(input, output){
    id = "undefined"
    if(!is.null(lf$elementId)) {
      id = "lf$elementId"
    }
    click_evt = paste0(id, "_shape_click")

    observeEvent(input[[click_evt]], {
      print(input[[click_evt]])
      selections <<- c(selections, list(input[[click_evt]]))
    })
  })


# ugly hack to try to achieve crosstalk support
#   crosstalk and leaflet don't support addPolygons
tgs <- tagList(
  lf,
  htmlwidgets::onStaticRenderComplete(
"
debugger;
var ct_select = new crosstalk.SelectionHandle('states');

ct_select.on('change', function(val){console.log(val)});

var lf = HTMLWidgets.find('.leaflet').getMap();

// define our functions for toggling
function toggle_opacity(group) {
  // change opacity in a crude way
  //   assuming user just stuck with the defaults
  var layer = lf.layerManager.getLayerGroup(group);
  var opaqueness = 0.2;
  if(layer.eachLayer) {
    layer.eachLayer(function(x){opaqueness = +$(x._path).css('fillOpacity')})
  }
  opaqueness = opaqueness === 0.2 ? 0.6 : 0.2;
  layer.setStyle({fillOpacity:opaqueness});
  return opaqueness;
};

function toggle_state(group) {
  var selected = ct_select.value;
  debugger;
  if(Array.isArray(selected) && selected.length > 0) {
    var new_selection = selected.slice();
    var loc = new_selection.indexOf(group);
    if(loc >= 0) {
      new_selection.splice(loc,1);
      ct_select.set(new_selection);
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
"
  )
) %>% browsable()
