#' Playback a Recorded 'mapedit' Session on Leaflet Map
#'
#' @param x a recorded mapedit session from \code{editFeatures(..., record=TRUE)}
#' @import htmltools
#' @keywords internal

playback <- function(x, origsf = NULL) {
  if(is.null(x)) {
    stop("x is NULL.  Please provide x for playback.", call. =  FALSE)
  }

  if(!requireNamespace("geojsonio")) {
    stop("Playback requires geojsonio.  Please install.packages('geojsonio') and try again.", .call = FALSE)
  }

  view_orig <- getOption("viewer")
  on.exit(options(viewer=view_orig))
  options(viewer = NULL)

  rec <- attr(x, "recorder", exact=TRUE)
  if(is.null(rec)) {
    stop("Did not find recorder.  Please use record=TRUE with edit functions.", call. = FALSE)
  }
  # check for original in recorder
  #  and use that if origsf not provided
  if(is.null(origsf)) {
    origsf <- attr(x, "original", exact=TRUE)
  }

  if(!is.null(origsf)) {
    map = mapview::mapview(
      origsf,
      alpha.regions=0.4,
      dashArray="5,5"
    )@map
  } else {
    map = mapview::mapview()@map
  }

  map$height = "100%"

  sf_all <- sf::st_geometry(combine_list_of_sf(
    lapply(rec,function(x){x$feature})
  ))

  x = mapview:::checkAdjustProjection(sf_all)

  if(!is.null(origsf)) {
    sf_all = c(
      sf::st_geometry(sf_all),
      sf::st_geometry(origsf)
    )
  }

  ext = mapview:::createExtent(sf_all)
  map = leaflet::fitBounds(
    map,
    lng1 = ext[1],
    lat1 = ext[3],
    lng2 = ext[2],
    lat2 = ext[4]
  )

  orig_gj <- NULL
  if(!is.null(origsf)) {
    origsf = mapview:::checkAdjustProjection(origsf)
    origsf$edit_id = as.character(1:nrow(origsf))
    orig_gj = geojsonio::geojson_list(origsf)
  }

  scr <-  sprintf(
"
function(el, x) {
  debugger;
  var map = this;
  var feat = %s;
  var feat_lookup = {};
  var orig = %s;

  function get_id(feature) {
    if(feature.properties.edit_id) {
      return 'edit' + feature.properties.edit_id;
    }

    if(feature.properties.layerId) {
      return 'edit' + feature.properties.layerId
    }

    if(feature.properties.X_leaflet_id) {
      return 'leaf' + feature.properties.X_leaflet_id;
    }
  }

  function feat_overlay(feature, delay, trans) {
    return L.d3SvgOverlay(function(sel, proj) {
      var upd = sel.selectAll('path').data(feature.features);
      upd_new = upd.enter()
        .append('path')
        .attr('d', proj.pathFromGeojson)
        .style('fill', 'none')
        .style('stroke', 'black')
        .style('opacity', 0.0001)
      upd_new
        .transition(trans)
        .delay(delay)
        .style('opacity', 1)
      upd = upd.merge(upd_new)
      upd.attr('stroke-width', 1 / proj.scale);

      upd.each(function(f) {
        feat_lookup[get_id(f)] = {
          pathd: proj.pathFromGeojson(f),
          pathsvg: d3.select(this).node(),
          pathfun: proj.pathFromGeojson
        }
      });
    });
  }

  function draw(ed, delay) {
    feat_overlay(ed.feature, delay, 2000).addTo(map);
  };

  function edit_polygon(f, path_f, ed, delay) {
    var interpolator = flubber.interpolate(
      path_f.pathd,
      path_f.pathfun(f)
    );

    d3.select(path_f.pathsvg)
      .transition(2000)
      .delay(delay)
      .attrTween('d', function(d) {return interpolator});

    path_f.pathd = path_f.pathfun(f);
    return f;
  }

  function edit_point(f, path_f, ed, delay) {
    d3.select(path_f.pathsvg)
      .transition(2000)
      .delay(delay)
      .attr('d', path_f.pathfun(f));

    path_f.pathd = path_f.pathfun(f);
    return f;
  }

  function edit(ed, delay) {
    ed.feature.features.forEach(function(f) {
      var path_f = feat_lookup[get_id(f)];

      if(f.geometry.type.toLowerCase() === 'point') {
        return edit_point(f, path_f, ed, delay);
      }

      return edit_polygon(f, path_f, ed, delay);
    })
  };

  function del(ed, delay) {
    ed.feature.features.forEach(function(f) {
      var path_f = feat_lookup[get_id(f)];
      d3.select(path_f.pathsvg)
        .transition(2000)
        .delay(delay)
        .style('opacity', 0.0001)
        .remove();

      delete feat_lookup[get_id(f)];
    });
  };

  // add original features
  if(orig !== null) {
    feat_overlay(orig, 0, 0).addTo(map);
  }

  var actions = {
    'map_draw_new_feature' : draw,
    'map_draw_edited_features': edit,
    'map_draw_deleted_features': del
  };

  feat.forEach(function(ed, i) {
    if(orig !== null) { i = i + 1 }
    actions[ed.evt](ed, i * 1000)
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
),
jsonlite::toJSON(
  orig_gj,
  auto_unbox=TRUE,
  force=TRUE,
  null="null"
)
  )

  print(browsable(tagList(
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
  ))
  )
}
