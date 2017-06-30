library(sf)
library(mapview)
library(mapedit)
library(geojsonio)
library(htmltools)

#ed <- editMap()
#rec <- attr(ed, "recorder")

#bbox <- unclass(
#  st_bbox(
#    mapedit:::combine_list_of_sf(
#      lapply(rec,function(x){x$feature})
#    )
#  )
#)

bbox_rect <- geojson_json(
  st_polygon(
    list(matrix(
      c(
        c(bbox[1], bbox[4]),
        c(bbox[3], bbox[4]),
        c(bbox[3], bbox[2]),
        c(bbox[1], bbox[2]),
        c(bbox[1], bbox[4])
      ),
      ncol=2,
      byrow=TRUE
    ))
  )
)

tl <- tagList(
  tags$head(tags$script(src="https://unpkg.com/flubber")),
  d3r::d3_dep_v4(offline = FALSE),
  tags$script(HTML(
sprintf(
"
var feat = %s;
var bbox = %s;
var feat_lookup = {};

var proj = d3.geoMercator().fitSize(
  [800,400],
  bbox
);
var path = d3.geoPath().projection(proj);

var svg = d3.select('body').append('svg')
  .style('height', 400)
  .style('width', 800)
  .classed('map', true);


function draw(ed, delay) {
  var path_f = svg
    .append('path')
    .datum(ed)
    .style('fill', 'none')
    .style('stroke', 'black')
    .style('opacity', 0.0001)
    .transition(2000)
    .delay(delay * 1000)
    .style('opacity', 1)
    .attr('d', path(ed.feature));
  feat_lookup[ed.feature.features[0].properties.X_leaflet_id] = {
    pathd : path(ed.feature),
    pathsvg: path_f.node(0)
  }
};

function edit(ed, delay) {
  var path_f = feat_lookup[ed.feature.features[0].properties.X_leaflet_id];
  var interpolator = flubber.interpolate(
    path_f.pathd,
    path(ed.feature)
  );

  d3.select(path_f.pathsvg)
    .transition(2000)
    .delay(delay * 1000)
    .attrTween('d', function(d) {return interpolator});
};

function del(ed, delay) {
  var path_f = feat_lookup[ed.feature.features[0].properties.X_leaflet_id];
  d3.select(path_f.pathsvg)
    .transition(2000)
    .delay(delay * 1000)
    .style('opacity', 0.0001)
    .remove();
};

var actions = {
  'map_draw_new_feature' : draw,
  'map_draw_edited_features': edit,
  'map_draw_deleted_features': del
};

feat.forEach(function(ed, i) {
  actions[ed.evt](ed, i)
});
/*
var fpath = svg.append('g')
  .selectAll('path')
  .data([feat[0]])
  .enter()
  .append('path')
  .attr('d', path)
  .style('stroke', 'black')
  .style('fill', 'none')
  .style('pointer-events', 'all');

feat.slice(1).reduce(
function(left,right,i) {
var interpolator = flubber.interpolate(
path(feat[i]),
path(right)
);

return left
.transition()
.duration(2000)
.attrTween('d', function(d) {return interpolator});
},
fpath
);
*/
",
jsonlite::toJSON(
  Map(
    function(x){
      x$feature <-geojson_list(x$feature);
      x
    },
    rec
  ),
  auto_unbox=TRUE,
  force=TRUE
),
bbox_rect
    )
  ))
  )

browsable(tl)
