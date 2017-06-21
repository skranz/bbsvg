examples.bb_seris = function() {
  setwd("D:/lehre/vwl_einf")
  d = readRDS("mh_de.rds")

  #bb = bb_pane(show.ticks = TRUE, org.width=800) %>%
  #  bb_series(x=1:5,y=1:5)
  #view.bb(bb)
    
  bb = bb_pane(show.ticks = TRUE, org.width=800, yrange=c(-30,20)) %>%
    bb_hline(y=0) %>%
    bb_series(x=d$year,y=d$growth, color="blue") %>%
    bb_xaxis(num.ticks=15,label="Jahr") %>%
    bb_period(1914,1918,"1. Weltkrieg") %>%
    bb_period(1923,NULL,"Hyperinflation") %>%
    bb_period(1929,1933,"Weltwirtschaftskrise") %>%
    bb_period(1939,1945,"2. Weltkrieg") %>%
    bb_period(1973,NULL,"1. Oelpreisschock") %>%
    bb_period(1979,NULL,"2. Oelpreisschock") %>%
    bb_period(1990,NULL,"Wiedervereinigung") %>%
    bb_period(2007,2009+0.5,"Finanzkrise")
  view.bb(bb)
  
  bb = bb_pane(show.ticks = TRUE, org.width=800,yrange=c(-10,150)) %>%
    bb_hline(y=0) %>%
    bb_series(id="series", x=d$year,y=pmin(d$inflation,2000), color="blue") %>%
    bb_xaxis(num.ticks=15,label="Jahr") %>%
    bb_period(1914,1918,"1. Weltkrieg") %>%
    bb_period(1923,NULL,"Hyperinflation") %>%
    bb_period(1929,1932,"Weltwirtschaftskrise") %>%
    bb_period(1939,1945,"2. Weltkrieg") %>%
    bb_period(1973,NULL,"1. Oelpreisschock") %>%
    bb_period(1979,NULL,"2. Oelpreisschock") %>%
    bb_period(1990,NULL,"Wiedervereinigung") %>%
    bb_period(2007,2009+0.5,"Finanzkrise")
  view.bb(bb)
  
  bb$yrange = c(-10,15)
  view.bb(bb)
  
  range(d$inflation)
}

bb_period = function(bb, from,to=NULL,label=NULL, shade="#333333", alpha=0.3, lwd=1, linetype="dashed", tooltip=label, area.tooltip = tooltip, latex=NULL, font_size=11) {
  restore.point("bb_period")
  if (!is.null(to))
    bb = bb_area(bb,x = c(from, from,to,to),y=c(bb$y.min,bb$y.max,bb$y.max,bb$y.min),fill = shade, alpha=alpha, tooltip=area.tooltip)
  bb = bb_segment(bb,x1=from,x2=from,y1=bb$y.min,y2=bb$y.max, linetype=linetype, lwd=lwd, tooltip=tooltip)
  bb = bb_text(bb, x= from, y=bb$y.min, label=label, latex=latex,font_size = font_size, vertical=TRUE, align="left", valign = "bottom",y.offset=2)
  bb
}

bb_series = function(bb, x=data[[xvar]],y=data[[yvar]],data=bb$data, xvar=1,yvar=2,alpha=NULL,color=NULL, class="series",linetype="solid", lwd=NULL, plot_type="l",line.style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), point.style = list(fill=color, "fill-opacity"=alpha),
  dasharray = linetype.to.dasharry(linetype),...,id=paste0("series_",random.string()), level=10) {
  restore.point("bb_series")
  
  na.rows = is.na(x) | is.na(y)
  x = x[!na.rows]
  y = y[!na.rows]
  
  obj = nlist(id, type="series", class=class, x=x,y=y, line.style,point.style,"stroke-dasharray"=dasharray, eval.fields=c("x","y"), level=level)

  if (is.null(bb$xrange)) {
    bb$xrange = range(x, na.rm=TRUE)
    bb$x.min = min(bb$xrange)
    bb$x.max = max(bb$xrange)
  }
    
  if (is.null(bb$yrange)) {
    bb$yrange = range(y, na.rm = TRUE)
    bb$y.min = min(bb$yrange)
    bb$y.max = max(bb$yrange)
  }
  
  bb_object(bb, obj)
  
}


draw.svg.series = function(svg,obj, level=0, display=NULL) {
  restore.point("draw.svg.series")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  #r = domain.to.range(x=geom$x, y=geom$y, svg=svg)  
 
  svg = svg_polyline(svg=svg,x=geom$x,y=geom$y, style=obj$line.style,level = level, id=obj$id)
  
  svg = svg_tooltip_circles(svg, x=geom$x,y=geom$y, alpha=0)
  svg
}

svg_tooltip_circles = function(svg, x, y, tooltip=paste0(round(x,2),",",round(y,2)), r=5, alpha=0.5, color="black", id=paste0("tooltips_", random.string()), style=list(fill=color, "fill-opacity"=alpha)) {
  restore.point("svg_tooltip_circles")
  
  ra = domain.to.range(x=x, y=y, svg=svg)
  
  style = make_style_arg(style)
  txt = paste0('<circle cx="',ra$x,'" cy="',ra$y,'" r="',r,'" style="',style,'"> <title>',tooltip,'</title></circle>')
  txt = paste0('<g id="', id,'">', paste0(txt, collapse="\n"),"</g>")
  svg_add(svg, txt, id)
}