..bb..env = new.env()

#' Create a blackboard-style graphic
#'
#' Creates a plotting pane or modifies an existing `bb` graphic object.
#'
#' @name bb_pane
#' @aliases bbsvg
#' @param bb An optional existing `bb` object to modify.
#' @param id An optional graphic identifier.
#' @param data Optional data frame used to evaluate graphic expressions.
#' @param xvar,yvar Names of the horizontal and vertical variables.
#' @param xy A two-element character vector used as the default variable names.
#' @param xrange,yrange Two-element coordinate ranges.
#' @param show.ticks Whether axes show ticks by default.
#' @param arrow.axis Whether axes use arrow heads by default.
#' @param xlen,ylen Numbers of grid points used for curve computation.
#' @param org.width,org.height Unscaled graphic dimensions in pixels.
#' @param margins Optional plot margins in pixels.
#' @param show,hide Object selectors controlling visibility.
#' @param init.data Whether to initialize data-dependent state.
#' @param dataenv Environment in which data expressions are evaluated.
#' @param css CSS included in the SVG.
#' @param values Named values used to evaluate expressions.
#' @param data.row Row of `data` used to initialize `values`.
#' @param enclos Enclosing environment for expression evaluation.
#' @param scale Scale factor applied to the original dimensions.
#' @param width,height Output dimensions in pixels.
#' @param ... Additional graphic defaults.
#' @return A `bb_pane` object.
#' @export
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 10), yrange = c(0, 5))
#' }
bbsvg = bb_pane = function(bb=NULL, id=NULL,  data=NULL, xvar=xy[1], yvar=xy[2], xy=c("x_","y_"), xrange=NULL, yrange=NULL, show.ticks=FALSE, arrow.axis=NULL, xlen=201,ylen=201, org.width = width, org.height=height, margins=NULL,  show=".all", hide=NULL, init.data=FALSE, dataenv=parent.frame(), css=bb_svg_css(), values = if (!is.null(data)) as.list(data[data.row,,drop=FALSE]) else list(), data.row = 1, enclos=parent.frame(), scale=1, width=420, height=300,... ) {
  restore.point("bb_pane")

  bb = first.non.null(bb, list())


  org.width = org.width * scale
  org.height = org.height * scale

  bb = copy.non.null.fields(bb,source=nlist(id,data,values, data.row,enclos, xvar,yvar,xrange,yrange,show, hide,xlen,ylen, org.width, org.height, margins, dataenv,css))

  bb$xrange = compute_bb_field(bb$xrange, bb=bb)
  bb$yrange = compute_bb_field(bb$yrange, bb=bb)
  if (!is.null(bb$xrange)) {
    bb$x.min = min(bb$xrange)
    bb$x.max = max(bb$xrange)
  } else {
    bb$x.min = quote(..x.min)
    bb$x.max = quote(..x.max)
  }
  if (!is.null(bb$yrange)) {
    bb$y.min = min(bb$yrange)
    bb$y.max = max(bb$yrange)
  } else {
    bb$y.min = quote(..y.min)
    bb$y.max = quote(..y.max)
  }

  bb$defaults = copy.non.null.fields(bb$defaults, nlist(show.ticks,arrow.axis,...))

  bb = copy.into.null.fields(bb, nlist(objs=list(),labels=list(), geoms=list()))

  class(bb) = c("bb_pane","list")

  restore.point("bb_pane_2")

  bb
}

#' Specify the x-axis
#'
#' @param bb A `bb` graphic object.
#' @param label Plain-text axis label.
#' @param latex Optional LaTeX axis label.
#' @param labelpos Label position: `"bottom"`, `"right"`, or `"center"`.
#' @param show.ticks Whether to draw ticks and tick labels.
#' @param arrow.axis Whether to draw an arrow-headed axis.
#' @param defaults Graphic defaults used to resolve axis settings.
#' @param y.offset,x.offset Label offsets in pixels.
#' @param y Axis position: `"bottom"`, `"top"`, or a numeric coordinate.
#' @param align Label alignment.
#' @param num.ticks Desired number of automatically generated ticks.
#' @param ticks Optional explicit tick coordinates.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_xaxis(label = "Quantity")
#' }
bb_xaxis = function(bb,
  label=latex,
  latex = NULL,
  labelpos = c("bottom","right","center")[1],
  show.ticks=first.non.null(defaults$show.ticks, TRUE),
  arrow.axis = first.non.null(defaults$arrow.axis, !isTRUE(show.ticks)),
  defaults=bb$defaults, y.offset = NULL, x.offset = NULL, y="bottom", align=NULL, num.ticks=5, ticks=NULL
) {
  restore.point("bb_xaxis")
  bb$xaxis = nlist(type="xaxis", show.ticks, arrow.axis, num.ticks,y=y)
  if (!is.null(ticks)) bb$xaxis$ticks = ticks

  if (!is.null(label)) {
    if (y=="bottom") {
      lab.y = bb$y.min
    } else if (y=="top") {
      lab.y = bb$y.max
    } else if (is.numeric(y)) {
      lab.y = y
    } else {
      lab.y = bb$y.min
    }

    if (labelpos == "bottom") {

      align = first.non.null(align, "center")
      lab.x = max(bb$xrange)
      y.offset = first.non.null(y.offset, if (!show.ticks) -20 else -50)
      x.offset = first.non.null(x.offset, 15)

    } else if (labelpos == "right") {
      align = first.non.null(align, "left")
      lab.x = max(bb$xrange)
      y.offset = first.non.null(y.offset, -3)
      x.offset = first.non.null(x.offset, 20)
    } else {
      align = first.non.null(align, "center")
      lab.x = mean(bb$xrange)
      y.offset = first.non.null(y.offset, -50)
      x.offset = first.non.null(x.offset,0)
    }
    bb = bb_text(bb,label=label, latex=latex, x=lab.x, y=lab.y, x.offset=x.offset, y.offset=y.offset, align=align)
  }

  bb
}

#' Specify the y-axis
#'
#' @param bb A `bb` graphic object.
#' @param label Plain-text axis label.
#' @param latex Optional LaTeX axis label.
#' @param labelpos Label position: `"left"`, `"top"`, or `"center"`.
#' @param show.ticks Whether to draw ticks and tick labels.
#' @param arrow.axis Whether to draw an arrow-headed axis.
#' @param defaults Graphic defaults used to resolve axis settings.
#' @param y.offset,x.offset Label offsets in pixels.
#' @param align Label alignment.
#' @param x Axis position: `"left"`, `"right"`, or a numeric coordinate.
#' @param ticks Optional explicit tick coordinates.
#' @param num.ticks Desired number of automatically generated ticks.
#' @param show.grid Whether to draw horizontal grid lines.
#' @param grid.ticks Coordinates of grid lines.
#' @param grid.color Grid-line color.
#' @param tick.labels Optional custom tick labels.
#' @param show.line Whether to draw the main axis line.
#' @param ... Additional axis settings.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_yaxis(label = "Price")
#' }
bb_yaxis = function(bb,
  label=latex,
  latex = NULL,
  labelpos = c("left","top","center")[1],
  show.ticks=first.non.null(defaults$show.ticks, TRUE),
  arrow.axis = first.non.null(defaults$arrow.axis, !isTRUE(show.ticks)),
  defaults=bb$defaults, y.offset = NULL, x.offset = NULL, align=NULL, x="left",ticks=NULL, num.ticks=5, show.grid=FALSE, grid.ticks=ticks, grid.color="#888888", tick.labels=NULL, show.line=TRUE, ...
) {
  restore.point("bb_yaxis")

  bb$yaxis = nlist(type="yaxis", show.ticks, arrow.axis,x=x,num.ticks,show.line, show.grid, grid.ticks, grid.color, labelpos)
  if (!is.null(ticks)) bb$yaxis$ticks = ticks
  if (!is.null(tick.labels)) bb$yaxis$tick.labels = tick.labels


  if (!is.null(label)) {
    if (x=="left") {
      lab.x = bb$x.min
    } else if (x=="right") {
      lab.x = bb$x.max
    } else if (is.numeric(x)) {
      lab.x = x
    } else {
      lab.x = bb$xmin
    }
    if (labelpos == "left") {
      lab.y = max(bb$yrange)
      align = first.non.null(align, "right")
      y.offset = first.non.null(y.offset, 5)
      x.offset = first.non.null(x.offset, -8)

    } else if (labelpos == "top") {
      lab.y = max(bb$yrange)
      align = first.non.null(align, "center")
      y.offset = first.non.null(y.offset, 20)
      x.offset = first.non.null(x.offset, 0)
    } else {
      lab.y = mean(bb$yrange)
      align = first.non.null(align, "right")
      y.offset = first.non.null(y.offset, 0)
      x.offset = first.non.null(x.offset,-5)
    }
    bb = bb_text(bb,label=label, latex=latex, x=lab.x, y=lab.y, x.offset=x.offset, y.offset=y.offset, align=align)
  }


  bb
}

#' Add a vertical marker
#'
#' @param bb A `bb` graphic object.
#' @param x Horizontal marker coordinate.
#' @param y2,y1 End coordinates of the marker line.
#' @param y Optional shorthand for `y2`.
#' @param ... Additional arguments passed to the tick and segment.
#' @param linetype Marker-line type.
#' @param label Plain-text marker label.
#' @param latex Optional LaTeX marker label.
#' @param align Label alignment.
#' @param y.offset,x.offset Label offsets in pixels.
#' @param id A base identifier for the marker objects.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |> bb_xmarker(x = 2)
#' }
bb_xmarker = function(bb,x=NULL,y2=y,y=NULL,y1=bb$y.min,...,linetype="dashed",label=x,latex=NULL, align="center", y.offset=-20, x.offset=0, id = random.string()) {
  restore.point("bb_xmarker")
  
  y2=first.non.null(y2,max(bb$yrange))

  bb=bb_xtick(bb,latex=latex,label=label,  align=align, y.offset=y.offset,x.offset=x.offset,x=x, ..., id=paste0(id,"_text"))
  bb = bb_segment(bb,class="marker_line", x1=x, y1=y1,y2=y2, linetype=linetype, ..., id=paste0(id,"_line"))
}


#' Add a horizontal marker
#'
#' @param bb A `bb` graphic object.
#' @param y Vertical marker coordinate.
#' @param x2 Ending horizontal coordinate.
#' @param x Optional shorthand for `x2`.
#' @param ... Additional arguments passed to the tick and segment.
#' @param linetype Marker-line type.
#' @param label Plain-text marker label.
#' @param latex Optional LaTeX marker label.
#' @param align Label alignment.
#' @param id A base identifier for the marker objects.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |> bb_ymarker(y = 2)
#' }
bb_ymarker = function(bb,y=NULL,x2=x,x=NULL,...,linetype="dashed",label=y,latex=NULL, align="right", id = random.string()) {
  restore.point("bb_ymarker")
  x1=bb$y.min
  x2=first.non.null(x2,max(bb$xrange))

  bb=bb_ytick(bb,latex=latex,label=label,  align=align, y=y, ..., id=paste0(id,"_text"))
  bb = bb_segment(bb,class="marker_line",linetype=linetype, x1=x1, x2=x2,y1=y,y2=y, ..., id=paste0(id,"_line"))
}



#' Specify graphic margins
#'
#' @param bb A `bb` graphic object.
#' @param bottom,left,top,right Margins in pixels. `NULL` preserves the current
#'   value.
#' @param ... Reserved for additional margin settings.
#' @return The modified `bb` object.
#' @export
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_margins(left = 60, bottom = 50)
#' }
bb_margins = function(bb, bottom=NULL,left=NULL, top=NULL, right=NULL,...) {
  margins = nlist(bottom, left, top, right)
  bb$margins = copy.non.null.fields(bb[["margins"]], margins)
  bb
}

#' Add a point
#'
#' @param bb A `bb` graphic object.
#' @param x,y Point coordinates.
#' @param r Point radius in pixels.
#' @param alpha Point opacity.
#' @param color Stroke color.
#' @param fill Fill color.
#' @param class SVG class name.
#' @param style A list of SVG style properties.
#' @param ... Additional SVG style properties.
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_point(x = 2, y = 3)
#' }
bb_point = function(bb, x,y,r=4, alpha=NULL,color=fill, fill=NULL, class="point", style=list(stroke=color, "fill-color"=fill, "stroke-opacity"=alpha, "fill-opacity"=alpha,...), ..., id=paste0("point_",random.string())) {
  restore.point("bb_point")
  obj = nlist(id, type="point", class, x,y,r, style, eval.fields=c("x","y","r"))
  bb_object(bb, obj)
}

#' Hide a graphic object
#'
#' @param bb A `bb` graphic object.
#' @param id Identifier of the object to hide.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_point(2, 3, id = "p")
#' bb_hide_object(bb, "p")
#' }
bb_hide_object = function(bb, id) {
  bb$objs[[id]]$no.draw=TRUE
  bb
}

#' Add an object specification
#'
#' @param bb A `bb` graphic object.
#' @param obj Optional list describing the object.
#' @param ... Named properties added to `obj`.
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 5), yrange = c(0, 5))
#' bb_object(bb, list(type = "point", x = 2, y = 3), id = "p")
#' }
bb_object = function(bb, obj=NULL,..., id = first.non.null(obj[["id"]],random.string())) {
  args = list(...)
  if (is.null(obj)) obj = list()
  obj[names(args)] = args
  bb$objs[[id]] = obj
  bb

}

#' Set the current data row and values
#'
#' This helper updates the `bb` object available in the calling evaluation
#' context.
#'
#' @param values Named values used to evaluate graphic expressions.
#' @param data Data frame associated with the graphic.
#' @param data.row Row of `data` used as the current observation.
#' @return The updated `bb` object.
#' @examples
#' \dontrun{
#' bb <- bb_pane(data = data.frame(x = 1:2, y = 3:4))
#' bb_set_data(data = bb$data, data.row = 2)
#' }
bb_set_data = function(values = if (!is.null(data)) as.list(data[data.row,,drop=FALSE]) else list(),data = bb$data,data.row = first.non.null(bb$data.row,1)) {
  bb$data = data
  bb$data.row = data.row
  bb$values = values
  bb
}

#' Attach data to a graphic
#'
#' @param bb A `bb` graphic object.
#' @param data A data frame or similar object.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane() |> bb_data(data.frame(x = 1:3, y = 3:1))
#' }
bb_data = function(bb, data) {
  bb$data = data
  bb
}

#' Define computed graphic variables
#'
#' @param bb A `bb` graphic object.
#' @param ... Named expressions defining variables.
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |> bb_var(a = 2)
#' }
bb_var = function(bb, ..., id=paste0("var_", random.string())) {
  restore.point("bb_var")
  obj = list(id=id,type="var", var=list(...), no.draw=TRUE)
  bb$objs[[id]] = obj
  bb
}

cur.bb = function() {
  ..bb..env$bb
}

#' Default CSS for blackboard-style SVG graphics
#'
#' @return A character string containing CSS rules.
#' @examples
#' \dontrun{
#' css <- bb_svg_css()
#' }
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

/*
.polyline:hover {
  stroke-width: 5;
}
*/

.curve:hover {
  stroke-width: 5;
}

.axis {

}

.axis-main {
  stroke-width: 1.5;
}

.axis-tick {
  stroke-width: 1;
  stroke: black;
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

@media print {
  .series_tooltip_bar {
    stroke-opacity: 0 !important;
    visibility: hidden;
  }

}
.series_tooltip_bar {
  stroke-opacity: 0 !important;
}

.series_tooltip_bar:hover {
  stroke-opacity: 0.3 !important;
}


'
}
