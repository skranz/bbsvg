examples.bb_area = function() {
  
bb = bb_pane(xrange=c(0,8),yrange=c(0,8), show.ticks=FALSE, org.width = 210,  org.height=200,scale=1, margin=c(left=50)) %>%
  bb_xaxis(label="Gut 1") %>%
  bb_yaxis(label="Gut 2") %>%
  bb_curve(id="indi", eq="y_ = 12/(1+x_)") %>%
  bb_area_above_curve(curve.id="indi", xrange=c(2,4))

view.bb(bb)

}

#' Add a rectangular area
#'
#' @param bb A `bb` graphic object.
#' @param x1,y1 Coordinates of one corner.
#' @param x2,y2 Coordinates of the opposite corner.
#' @param ... Additional arguments passed to [bb_area()].
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area_rect(1, 1, 3, 2)
#' }
bb_area_rect = function(bb, x1,y1,x2,y2,...) {
  bb_area(bb,x=c(x1,x1,x2,x2),y=c(y1,y2,y2,y1),...)
}

#' Add a polygonal area
#'
#' @param bb A `bb` graphic object.
#' @param x,y Vectors containing the polygon coordinates.
#' @param fill Fill color.
#' @param alpha Fill opacity.
#' @param stroke Stroke color.
#' @param style A list of SVG style properties.
#' @param level Drawing level; lower values are drawn first.
#' @param ... Additional SVG style properties.
#' @param id A unique object identifier.
#' @param tooltip Optional tooltip text.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area(x = c(1, 1, 3), y = c(1, 3, 1))
#' }
bb_area = function(bb, x,y, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  obj = nlist(id, type="area", x,y, style, eval.fields=c("x","y"), tooltip=tooltip, level)
  bb_object(bb, obj)
}

#' Add an area above a curve
#'
#' @inheritParams bb_area_beside_curve
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area_above_curve(eq = "y_ = x_")
#' }
bb_area_above_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="above", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}

#' Add an area below a curve
#'
#' @inheritParams bb_area_beside_curve
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area_below_curve(eq = "y_ = x_")
#' }
bb_area_below_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="below", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}

#' Add an area to the left of a curve
#'
#' @inheritParams bb_area_beside_curve
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area_left_of_curve(eq = "x_ = 2")
#' }
bb_area_left_of_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="left", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}


#' Add an area to the right of a curve
#'
#' @inheritParams bb_area_beside_curve
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area_right_of_curve(eq = "x_ = 2")
#' }
bb_area_right_of_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="right", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}

#' Add an area beside a curve
#'
#' Shades the part of a plotting pane on a selected side of a curve.
#'
#' @param bb A `bb` graphic object.
#' @param eq Optional curve equation as a character string.
#' @param curve.id Identifier of an existing curve. Used when `eq` is `NULL`.
#' @param fill Fill color.
#' @param alpha Fill opacity.
#' @param stroke Stroke color.
#' @param style A list of SVG style properties.
#' @param direction One of `"above"`, `"below"`, `"left"`, or `"right"`.
#' @param level Drawing level; lower values are drawn first.
#' @param ... Additional arguments passed to [bb_curve()].
#' @param id A unique object identifier.
#' @param tooltip Optional tooltip text.
#' @param xmin,xmax,ymin,ymax Bounds used to construct the shading region.
#' @param xrange,yrange Two-element coordinate ranges.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_area_beside_curve(eq = "y_ = x_", direction = "below")
#' }
bb_area_beside_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke, ...), direction=c("above","below","left","right")[1],level=-10, ..., id=random.string(), tooltip=NULL, xmin=bb$xrange[[1]],xmax=bb$xrange[[2]],ymin=bb$yrange[[1]],ymax=bb$yrange[[2]], xrange=c(xmin,xmax), yrange=c(ymin,ymax)) {
  restore.point("bb_area_beside_curve")
  
  if (!is.null(eq)) {
    if (is.null(curve.id)) {
      curve.id = paste0(id,"--curve")
    } 
    bb = bb_curve(bb, id=curve.id,eq=eq, no.draw=TRUE, xrange=xrange, yrange=yrange)
  } else {
    if (!isTRUE(curve.id %in% names(bb$objs))) {
      stop("For an area beside a curve you must either specify the curve equation with the argument eq or an idea of an existing curve via curve.id")
    }
    # redraw curve if xrange or yrange differs
    curve = bb$objs[[curve.id]]    
    if (!all(curve$xrange==xrange & curve$yrange==yrange)) {
      curve.id = paste0(id,"--", random.string(1,5))
      bb = bb_curve(bb, id=curve.id,eq=curve$eq, no.draw=TRUE, xrange=xrange, yrange=yrange)
    }      
  }
  obj = nlist(id, type="area_beside_curve", curve.id=curve.id, style,  tooltip=tooltip, level=level, direction=direction,xrange=xrange, yrange=yrange)
  bb_object(bb, obj)
}


draw.svg.area_beside_curve = function(svg,obj, level=first.non.null(obj[["level"]],-1), display=NULL,bb) {
  restore.point("draw.svg.area_beside_curve")
  #display = init.geom.display(geom, display)
  
  geom = bb$objs[[obj$curve.id]]$geom
  x = geom$x
  y = geom$y
  
  n = length(x)
  y.max = max(obj$yrange)
  x.max = max(obj$xrange)
  y.min = min(obj$yrange)
  x.min = min(obj$xrange)
  
  if (obj$direction=="left") {
    x = c(x.min, x, x.min)
    y = c(y[1],y,y[n])
  } else if (obj$direction=="right") {
    x = c(x.max, x, x.max)
    y = c(y[1],y,y[n])
  } else if (obj$direction == "above") {
    y = c(y.max, y, y.max)
    x = c(x[1],x,x[n])
  } else if (obj$direction == "below") {
    y = c(y.min, y, y.min)
    x = c(x[1],x,x[n])
  }
  
  res = domain.to.range(x=x,y=y, svg=svg)
  points = paste0(res$x,",",res$y, collapse=" ")

  el = svg_tag("polygon", args=nlist(id=geom$id, points=points, style=obj$style, level=level, class="area", display=display),tooltip = geom$tooltip)
  svg_add(svg, el, id=obj$id)
}

draw.svg.area = function(svg,obj, level=-1, display=NULL,bb) {
  restore.point("draw.svg.area")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  res = domain.to.range(x=geom$x, y=geom$y, svg=svg)  
  
  points = paste0(res$x,",",res$y, collapse=" ")

  el = svg_tag("polygon", args=nlist(id=geom$id, points=points, style=obj$style, level=level, class="area", display=display),tooltip = geom$tooltip)
  svg_add(svg, el, id=obj$id)
}
