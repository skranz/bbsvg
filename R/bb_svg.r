bb_to_svg = function(bb, id = first.non.null(bb$id, random.string()), css=bb$css, width=first.non.null(bb$width,bb$org.width,480), height=first.non.null(bb$height,bb$org.height,320), return.svg.object = FALSE,latexsvg=isTRUE(bb$use.latex),outfile=NULL, ...) {
  restore.point("bb_to_svg")
  
  xrange = bb$xrange
  yrange = bb$yrange
  
  
  margins = compute_bb_margins(bb)
  
  
  svg = new_svg(id=id,width=width, height=height, xlim=bb$xrange, ylim=bb$yrange,css=css, margins=margins)

  bb = bb_compute_objs(bb)
  for (obj in bb$objs) {
    draw.svg.obj(svg, obj)
  }  

      

  
  
  for (obj in bb$labels) {
    obj = compute_bb_label(bb, obj)
    draw.svg.label(svg, obj)
  }

  do.call(svg_xaxis, c(list(svg=svg), bb$xaxis))
  do.call(svg_yaxis, c(list(svg=svg), bb$yaxis))
  
  
  if (return.svg.object) return(svg)
  
  ssvg=svg_string(svg)
  if (latexsvg)
    ssvg = latexsvg::latexsvg(ssvg)
  
  Encoding(ssvg) = "UTF-8"
  if (!is.null(outfile)) {
    writeLines(ssvg, outfile,useBytes = TRUE)
  }
  invisible(ssvg)
}

compute_bb_margins = function(bb) {
  restore.point("compute_bb_margins")
  margins = list(
    bottom=ifelse(isTRUE(bb$xaxis$show.ticks),60,40),
    left=ifelse(isTRUE(bb$yaxis$show.ticks),60,40),
    top=30,
    right=40
  )
  margins = copy.non.null.fields(dest=margins,source=bb$margins)
  unlist(margins)
}

draw.svg.obj = function(svg,obj,display=NULL,...) {
  restore.point("draw.svg.obj")
  
  if (obj$type=="curve") {
    draw.svg.curve(svg,obj, display=display)
  } else if (obj$type=="marker") {
    draw.svg.marker(svg,obj,  display=display)
  } else if (obj$type=="point") {
    draw.svg.point(svg,obj, display=display)
  } else {
    
    if (isTRUE(obj[["no.draw"]])) return(svg)
    restore.point("draw.svg.type")
    fun = paste0("draw.svg.",obj$type)
    do.call(fun, list(svg=svg, obj, display=display))
  }
  svg
}

draw.svg.area = function(svg,obj, level=-1, display=NULL) {
  restore.point("draw.svg.area")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  res = domain.to.range(x=geom$x, y=geom$y, svg=svg)  
  
  points = paste0(res$x,",",res$y, collapse=" ")

  el = svg_tag("polygon", args=nlist(id=geom$id, points=points, style=obj$style, level=level, class="area", display=display),tooltip = geom$tooltip)
  svg_add(svg, el, id=obj$id)
}


draw.svg.point = function(svg,obj, level=0, display=NULL) {
  restore.point("draw.svg.point")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  range = domain.to.range(x=geom$x, y=geom$y, svg=svg)
  
  el = svg_tag("circle", c(nlist(cx=range$x,cy=range$y,r = geom$r, style=obj$style, class=obj$class)))

  svg_add(svg, el, id=obj$id)
}


draw.svg.segment = function(svg,obj, level=0, display=NULL) {
  restore.point("draw.svg.segment")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  r1 = domain.to.range(x=geom$x1, y=geom$y1, svg=svg)  
  r2 = domain.to.range(x=geom$x2, y=geom$y2, svg=svg)  
  
  el = svg_tag("line", c(nlist(x1=r1$x,x2=r2$x,y1=r1$y,y2=r2$y, style=obj$style, class=obj$class, "stroke-dasharray"=obj[["stroke-dasharray"]])))

  svg_add(svg, el, id=obj$id)
}

draw.svg.arrow = function(svg,obj, level=-1, display=NULL) {
  restore.point("draw.svg.area")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  r1 = domain.to.range(x=geom$x1, y=geom$y1, svg=svg)  
  r2 = domain.to.range(x=geom$x2, y=geom$y2, svg=svg)  
  
  svg_def_small_arrow_head(svg)
  arrow.li = list("marker-end"="url(#small_arrow_head)")
  
  
  el = svg_tag("line", c(nlist(x1=r1$x,x2=r2$x,y1=r1$y,y2=r2$y, style=obj$style, class=obj$class), arrow.li))

  svg_add(svg, el, id=obj$id)
}

svg_def_small_arrow_head =  function(svg,id="small_arrow_head", class="arrow_head") {
  svg_add_def(svg=svg,id=id,
    paste0('
    <marker id="',id,'" class="',class,'" markerWidth="10" markerHeight="10" refx="0" refy="3" orient="auto" markerUnits="userSpaceOnUse">
      <path d="M0,0 L0,6 L9,3 z" style ="fill: black;"/>
    </marker>
    '
    )
  )
}


svg_def_arrow_head =  function(svg,id="arrow_head", class="arrow_head") {
  svg_add_def(svg=svg,id=id,
    paste0('
    <marker id="',id,'" class="',class,'" markerWidth="10" markerHeight="10" refx="0" refy="3" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L0,6 L9,3 z" style ="fill: black;"/>
    </marker>
    '
    )
  )
}

svg_def_label_box =  function(svg,id="label_box", class="label_box") {
  svg_add_def(svg=svg,id=id,
    paste0(
    '
      <filter x="0" y="0" width="1" height="1" id="', id,'">
        <feFlood flood-color="white" flood-opacity="0.85"/>
        <feComposite in="SourceGraphic"/>
      </filter>
    '
    )
  )
#<text filter="url(#solid)" x="20" y="50" font-size="50">solid background
} 
