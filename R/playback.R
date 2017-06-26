#' Playback a Recorded 'mapedit' Session
#'
#' @param x a recorded mapedit session from \code{editFeatures(..., record=TRUE)}
#' @keywords internal

playback <- function(x) {

  if(!requireNamespace("geojsonio")) {
    stop("Playback requires geojsonio.  Please install.packages('geojsonio') and try again.", .call = FALSE)
  }

  rec <- attr(x, "recorder")

  # will evntually move this to JavaScript
  bbox <- unclass(
    sf::st_bbox(
      combine_list_of_sf(
        lapply(rec,function(x){x$feature})
      )
    )
  )

  bbox_rect <- geojsonio::geojson_json(
    sf::st_polygon(
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

  tl <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$script(src="https://unpkg.com/d3@4.9.1/build/d3.min.js"),
      htmltools::tags$script(src="https://unpkg.com/flubber@0.3.0")
    ),
    htmltools::tags$script(htmltools::HTML(
  sprintf(
  "
  var feat = %s;
  var bbox = %s;
  var feat_lookup = {};

  d3.selectAll('body,html').style('height', '100%%');

  var width = document.body.getBoundingClientRect().width;
  var height = document.body.getBoundingClientRect().height;

  var proj = d3.geoMercator().fitSize(
    [width, height],
    bbox
  );
  var path = d3.geoPath().projection(proj);

  var svg = d3.select('body').append('svg')
    .style('height', '100%%')
    .style('width', '100%%')
    .attr('viewBox', '0,0,' + width + ',' + height)
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

    delete feat_lookup[ed.feature.features[0].properties.X_leaflet_id];
  };

  var actions = {
    'map_draw_new_feature' : draw,
    'map_draw_edited_features': edit,
    'map_draw_deleted_features': del
  };

  feat.forEach(function(ed, i) {
    actions[ed.evt](ed, i)
  });
  ",
  jsonlite::toJSON(
    Map(
      function(x){
        x$feature <- geojsonio::geojson_list(x$feature);
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

  htmltools::browsable(tl)
}



#' Playback a Recorded 'mapedit' Session on Leaflet Map
#'
#' @param x a recorded mapedit session from \code{editFeatures(..., record=TRUE)}
#' @import htmltools
#' @keywords internal

playback_lf <- function(x) {
  rec <- attr(x, "recorder")

  sf_all <- combine_list_of_sf(
    lapply(rec,function(x){x$feature})
  )

  x = mapview:::checkAdjustProjection(sf_all)

  map = mapview::mapview()@map
  map$height = "100%"
  ext = mapview:::createExtent(sf_all)
  map = leaflet::fitBounds(
    map,
    lng1 = ext[1],
    lat1 = ext[3],
    lng2 = ext[2],
    lat2 = ext[4]
  )

  scr <-  sprintf(
"
function(el, x) {
  debugger;
  var map = this;
  var feat = %s;
  var feat_lookup = {};

  function draw(ed, delay) {
    function featOverlay(feature) {
      return L.d3SvgOverlay(function(sel, proj) {
        var upd = sel.selectAll('path').data(feature.features);
        upd_new = upd.enter()
          .append('path')
          .attr('d', proj.pathFromGeojson)
          .style('fill', 'none')
          .style('stroke', 'black')
          .style('opacity', 0.0001)
        upd_new
          .transition(2000)
          .delay(delay * 1000)
          .style('opacity', 1)
        upd = upd.merge(upd_new)
        upd.attr('stroke-width', 1 / proj.scale);

        feat_lookup[ed.feature.features[0].properties.X_leaflet_id] = {
          pathd: proj.pathFromGeojson(feature.features[0]),
          pathsvg: upd.node(0),
          pathfun: proj.pathFromGeojson
        }
      });
    }

    featOverlay(ed.feature).addTo(map);
  };

  function edit(ed, delay) {
    var path_f = feat_lookup[ed.feature.features[0].properties.X_leaflet_id];
    var interpolator = flubber.interpolate(
      path_f.pathd,
      path_f.pathfun(ed.feature)
    );

    d3.select(path_f.pathsvg)
      .transition(2000)
      .delay(delay * 1000)
      .attrTween('d', function(d) {return interpolator});

    path_f.pathd = path_f.pathfun(ed.feature);
  };

  function del(ed, delay) {
    var path_f = feat_lookup[ed.feature.features[0].properties.X_leaflet_id];
    d3.select(path_f.pathsvg)
      .transition(2000)
      .delay(delay * 1000)
      .style('opacity', 0.0001)
      .remove();

    delete feat_lookup[ed.feature.features[0].properties.X_leaflet_id];
  };

var actions = {
  'map_draw_new_feature' : draw,
  'map_draw_edited_features': edit,
  'map_draw_deleted_features': del
};

feat.forEach(function(ed, i) {
  actions[ed.evt](ed, i)
});
}
    ",
      jsonlite::toJSON(
        Map(
          function(x){
            x$feature <- geojsonio::geojson_list(x$feature);
            x
          },
          rec
        ),
        auto_unbox=TRUE,
        force=TRUE
      )
  )

  browsable(
    tagList(
      tags$head(
        tags$script(src="https://unpkg.com/d3"),
        tags$script(src="https://unpkg.com/flubber@0.3.0"),
        tags$script(src="https://cdn.rawgit.com/manubb/Leaflet.D3SvgOverlay/patch/L.D3SvgOverlay.js")
      ),
      tags$script(HTML("d3.selectAll('html,body').style('height','100%')")),
      tags$div(
        style="height:100%;",
        htmlwidgets::onRender(
          map,
          scr
        )
      )
    )
  )
}
