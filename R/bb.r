..bb..env = new.env()

bb_pane = function(bb=NULL, id=NULL,  data=NULL, xvar=xy[1], yvar=xy[2], xy=c("x_","y_"), xrange=NULL, yrange=NULL, show.ticks=FALSE, arrow.axis=NULL, xlen=201,ylen=201, org.width = 420, org.height=300, margins=NULL,  show=".all", hide=NULL, init.data=FALSE, dataenv=parent.frame(), css=bb_svg_css(), values = ifelse(!is.null(data), data[data.row,,drop=FALSE],list()), data.row = 1, enclos=parent.frame(), scale=1,... ) {
  restore.point("bb_pane")
  
  bb = first.non.null(bb, list())

    
  org.width = org.width * scale
  org.height = org.height * scale
  
  bb = copy.non.null.fields(bb,source=nlist(id,data,values, data.row,enclos, xvar,yvar,xrange,yrange,show, hide,xlen,ylen, org.width, org.height, margins, dataenv,css))
  
  bb$xrange = compute_bb_field(bb$xrange, bb=bb)
  bb$yrange = compute_bb_field(bb$yrange, bb=bb)
  bb$x0 = min(bb$xrange)
  bb$y0 = min(bb$yrange)
  

  
  bb$defaults = copy.non.null.fields(bb$defaults, nlist(show.ticks,arrow.axis,...))
  
  bb = copy.into.null.fields(bb, nlist(objs=list(),labels=list(), geoms=list()))
  
  class(bb) = c("bb_pane","list")

  restore.point("bb_pane_2")
  
  if (is.null(bb[["xaxis"]]))
    bb = bb %>% bb_xaxis()
  if (is.null(bb[["yaxis"]]))
    bb = bb %>% bb_yaxis()
  
  bb
}

bb_xaxis = function(bb,
  label=latex,
  latex = NULL,
  labelpos = c("bottom","right","center")[1],
  show.ticks=first.non.null(defaults$show.ticks, TRUE),
  arrow.axis = first.non.null(defaults$arrow.axis, !isTRUE(show.ticks)),
  defaults=bb$defaults, y.offset = NULL, x.offset = NULL, align=NULL,...
) {
  restore.point("bb_xaxis")  
  bb$xaxis = nlist(type="xaxis", show.ticks, arrow.axis)
  if (!is.null(label)) {
    if (labelpos == "bottom") {
      y = bb$y0
      align = first.non.null(align, "center")
      x = max(bb$xrange)
      y.offset = first.non.null(y.offset, -20)
      x.offset = first.non.null(x.offset, 15)
      
    } else if (labelpos == "right") {
      y = bb$y0
      align = first.non.null(align, "left")
      x = max(bb$xrange)
      y.offset = first.non.null(y.offset, 0)
      x.offset = first.non.null(x.offset, 15)
    } else {
      y = bb$y0
      align = first.non.null(align, "center")
      x = max(bb$xrange)
      y.offset = first.non.null(y.offset, -50)
      x.offset = first.non.null(x.offset,0)
    }
    bb = bb_text(bb,label=label, latex=latex, x=x, y=y, x.offset=x.offset, y.offset=y.offset, align=align)
  }
  
  bb
}

bb_yaxis = function(bb,
  label=latex,
  latex = NULL,
  labelpos = c("left","top","center")[1],
  show.ticks=first.non.null(defaults$show.ticks, TRUE),
  arrow.axis = first.non.null(defaults$arrow.axis, !isTRUE(show.ticks)),
  defaults=bb$defaults, y.offset = NULL, x.offset = NULL, align=NULL,...
) {
  restore.point("bb_yaxis")  
  
  if (!is.null(label)) {
    if (labelpos == "left") {
      y = max(bb$yrange)
      align = first.non.null(align, "right")
      x = bb$x0
      y.offset = first.non.null(y.offset, 5)
      x.offset = first.non.null(x.offset, -5)
      
    } else if (labelpos == "top") {
      y = max(bb$yrange)
      align = first.non.null(align, "center")
      x = bb$x0
      y.offset = first.non.null(y.offset, 20)
      x.offset = first.non.null(x.offset, 0)
    } else {
      y = mean(bb$yrange)
      align = first.non.null(align, "right")
      x = bb$x0
      y.offset = first.non.null(y.offset, 0)
      x.offset = first.non.null(x.offset,-5)
    }
    bb = bb_text(bb,label=label, latex=latex, x=x, y=y, x.offset=x.offset, y.offset=y.offset, align=align)
  }
  
  bb$yaxis = nlist(type="yaxis", show.ticks, arrow.axis)
  bb
}

bb_xmarker = function(bb,x=NULL,y2=y,y=NULL,...,linetype="dashed",label=x,latex=NULL, align="center", y.offset=-20, id = random.string()) {
  restore.point("bb_xmarker")
  y1=bb$y0
  y2=first.non.null(y2,max(bb$yrange))
  
  bb=bb_xtick(bb,latex=latex,label=label,  align=align, y.offset=y.offset,x=x, ..., id=paste0(id,"_text"))
  bb = bb_segment(bb,class="marker_line", x1=x, y1=y1,y2=y2, linetype=linetype, ..., id=paste0(id,"_line"))
}


bb_ymarker = function(bb,y=NULL,x2=x,x=NULL,...,linetype="dashed",label=y,latex=NULL, align="right", id = random.string()) {
  restore.point("bb_ymarker")
  x1=bb$y0
  x2=first.non.null(x2,max(bb$xrange))
  
  bb=bb_ytick(bb,latex=latex,label=label,  align=align, y=y, ..., id=paste0(id,"_text"))
  bb = bb_segment(bb,class="marker_line",linetype=linetype, x1=x1, x2=x2,y1=y,y2=y, ..., id=paste0(id,"_line"))
}



#' Specify margins in pixels for bb object
#' @export
bb_margins = function(bb, bottom=NULL,left=NULL, top=NULL, right=NULL,...) {
  margins = nlist(bottom, left, top, right)
  bb$margins = copy.non.null.fields(bb[["margins"]], margins)
  bb
}

bb_area = function(bb, x,y, fill="#ffff33", alpha=0.3,stroke="none", style=nlist(fill=fill, "fill-opacity"=alpha,stroke=stroke,...), ..., id=random.string()) {
  obj = nlist(id, type="area", x,y, style, eval.fields=c("x","y"))
  bb_object(bb, obj)
}


bb_point = function(bb, x,y,r=4, alpha=NULL,color=fill, fill=NULL, class="point", style=list(stroke=color, "fill-color"=fill, "stroke-opacity"=alpha, "fill-opacity"=alpha,...), ..., id=paste0("point_",random.string())) {
  restore.point("bb_point")
  obj = nlist(id, type="point", class, x,y,r, style, eval.fields=c("x","y","r"))
  bb_object(bb, obj)
}



bb_object = function(bb, obj=NULL,..., id = first.non.null(obj[["id"]],random.string())) {
  args = list(...)
  if (is.null(obj)) obj = list()
  obj[names(args)] = args
  bb$objs[[id]] = obj
  bb
  
}

bb_data = function(bb, data) {
  bb$data = data
  bb
}

bb_var = function(bb, ..., id=paste0("var_", random.string())) {
  restore.point("bb_var")
  obj = list(id=id,type="var", var=list(...), no.draw=TRUE)
  bb$objs[[id]] = obj
  bb
}

cur.bb = function() {
  ..bb..env$bb
}

bb_svg_css = function() {
'
.axis-main {
  stroke: black;
  stroke-linecap: round;
  stroke-linejoin: round;
  stroke-width: 2;
  stroke-opacity: 0.8;
}

.point {
  fill: black;
  stroke: none;
  stroke-opacity: 0.8;
  fill-opacity: 0.8;
}


.polyline, .curve, .line, .arrow, .segment {
  fill: none;
  stroke: black;
  stroke-width: 2;
  stroke-opacity: 0.8;
}

.polyline:hover {
  stroke-width: 5;
}

.curve:hover {
  stroke-width: 5;
}

.axis {

}

.axis-main {
  stroke-width: 1.5;
}

.axis-tick {
  stroke-width: 0.5;
}

.axis-ticklabel {
  font-size: 10.00pt;
  font-family: Arial;
  font-weight: normal;
}


.boxed-label {
  font-size: 10.00pt;
  font-family: Arial;
  font-weight: normal;
  filter: url(#label_box);
}


.axis-label {
  font-size: 11.00pt;
  font-family: Arial;
  font-weight: normal
}

.marker_line {
  stroke: black;
  stroke-width: 1;
  stroke-opacity: 0.8;
}

.marker_line:hover {
  stroke-width: 3;
}


'
}
