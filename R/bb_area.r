examples.bb_area = function() {
  
bb = bb_pane(xrange=c(0,8),yrange=c(0,8), show.ticks=FALSE, org.width = 210,  org.height=200,scale=1, margin=c(left=50)) %>%
  bb_xaxis(label="Gut 1") %>%
  bb_yaxis(label="Gut 2") %>%
  bb_curve(id="indi", eq="y_ = 12/(1+x_)") %>%
  bb_area_above_curve(curve.id="indi", xrange=c(2,4))

view.bb(bb)

}


bb_area = function(bb, x,y, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  obj = nlist(id, type="area", x,y, style, eval.fields=c("x","y"), tooltip=tooltip, level)
  bb_object(bb, obj)
}

bb_area_above_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="above", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}

bb_area_below_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="below", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}

bb_area_left_of_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="left", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}


bb_area_right_of_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), level=-10, ..., id=random.string(), tooltip=NULL) {
  bb_area_beside_curve(bb=bb, direction="right", eq=eq, curve.id=curve.id, style=style, id=id, tooltip=tooltip, level=level,...)
}

bb_area_beside_curve = function(bb, eq=NULL, curve.id = NULL, fill="#8888ff", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke, ...), direction=c("above","below","left","right")[1],level=-10, ..., id=random.string(), tooltip=NULL, xrange=bb$xrange, yrange=bb$yrange) {
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
  obj = nlist(id, type="area_beside_curve", curve.id=curve.id, style,  tooltip=tooltip, level=level, direction=direction)
  bb_object(bb, obj)
}


draw.svg.area_beside_curve = function(svg,obj, level=first.non.null(obj[["level"]],-1), display=NULL,bb) {
  restore.point("draw.svg.area_beside_curve")
  #display = init.geom.display(geom, display)
  
  geom = bb$objs[[obj$curve.id]]$geom
  x = geom$x
  y = geom$y
  
  n = length(x)
  if (obj$direction=="left") {
    x = c(bb$x.min, x, bb$x.min)
    y = c(y[1],y,y[n])
  } else if (obj$direction=="right") {
    x = c(bb$x.max, x, bb$x.max)
    y = c(y[1],y,y[n])
  } else if (obj$direction == "above") {
    y = c(bb$y.max, y, bb$y.max)
    x = c(x[1],x,x[n])
  } else if (obj$direction == "below") {
    y = c(bb$y.min, y, bb$y.min)
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