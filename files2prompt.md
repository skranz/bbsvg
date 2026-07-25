Below are relevant files from my R project.

################################################
# R project DESCRIPTION file:
################################################

```
Package: bbsvg
Type: Package
Title: Create svg graphs for typical blackboard figures that illustrate economic models
Version: 0.1
Date: 2017-05-27
Author: Sebastian Kranz
Maintainer: Sebastian Kranz <sebastian.kranz@uni-ulm.de>
Description: Create svg graphs for typical blackboard figures that illustrate economic models
License: GPL >= 2.0
Depends:
    dplyr, tidyr, restorepoint,codeUtils,
    stringtools, rmdtools,
    whisker,
    RColorBrewer,
    dplyrExtras,
    latexsvg,
    digest
Sugests:
  rsvg
RoxygenNote: 7.3.3
```


################################################
# R code files:
################################################


# FILE: bb.r
```
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
```
# END OF FILE: bb.r

-----------------------------------------------------------


# FILE: bb_animate.r
```
examples.bb.animate = function() {
  library(bbsvg)
  bb = bb_pane(id = "mysvg",xrange=c(0,10),yrange=c(0,10),org.width = 200,  org.height=200) %>%
  bb_point(id="mypoint", x=2,y=2)

  svg = bb_to_svg(bb)
  writeClipboard(svg)
  view.bb(bb)

  
}
```
# END OF FILE: bb_animate.r

-----------------------------------------------------------


# FILE: bb_area.r
```
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
```
# END OF FILE: bb_area.r

-----------------------------------------------------------


# FILE: bb_colors.r
```

colors_bb_series = function(n=11) {
  c(blue1="#0C5BB0",red="#EE0011",green2="#15983D", purple="#800080",
    orange="#FA6B09",brown= "#9A703E",blue2="#149BED",   turquoise ="#16A08C",
    pink="#EC579A",  yellow="#FEC10B" ,green="#A1C720")[1:n]    
}
```
# END OF FILE: bb_colors.r

-----------------------------------------------------------


# FILE: bb_comp.r
```

#' Compute all objects in a graphic
#'
#' @param bb A `bb` graphic object.
#' @return The `bb` object with computed geometry.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 1), yrange = c(0, 1)) |>
#'   bb_point(0.5, 0.5)
#' bb_compute_objs(bb)
#' }
bb_compute_objs = function(bb) {
  restore.point("bb_compute_objs")
  # later object may use computations from earlier ones
  for (i in seq_along(bb$objs)) {
    bb=bb_compute_obj(bb=bb,obj=bb$objs[[i]],i=i)
  }
  bb
}

#' Compute one object in a graphic
#'
#' @param bb A `bb` graphic object.
#' @param obj An object specification from `bb$objs`.
#' @param i The object's position in `bb$objs`.
#' @return The `bb` object with the selected geometry computed.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 1), yrange = c(0, 1)) |>
#'   bb_point(0.5, 0.5)
#' bb_compute_obj(bb, bb$objs[[1]], 1)
#' }
bb_compute_obj = function(bb,obj,i) {
  restore.point("bb_compute_obj")
  ..bb..env$bb=bb
  object.ind = i
  if (obj$type == "curve") {
    obj = bb_compute_curve(bb, obj)
  } else if (obj$type == "slopecurve") {
    obj = bb_compute_slopecurve(bb, obj)
  } else if (obj$type == "var") {
    #stop("compute.var")
    for (i in seq_along(obj$var)) {
      var = names(obj$var)[i]
      bb$values[[var]] = compute_bb_field(obj$var[[i]], values=bb$values, enclos=bb$enclos)
      ..bb..env$bb=bb
    }
    return(bb)
  } else {
    obj$geom = compute_bb_fields(obj=obj, fields=obj$eval.fields,bb=bb)
    if (obj$type == "segment") {
      obj = crop.bb.segment(obj,bb)
    }
  }
  obj$geom$tooltip = replace.latex.with.unicode(replace.whiskers(obj[["tooltip"]],obj.values(obj, bb)))
  if (!is.null(obj$dx)) {
    xfields = intersect(c("x","x1","x2"),names(obj$geom))
    for (field in xfields) 
      obj$geom[[field]] = obj$geom[[field]]+obj$dx
  }
  if (!is.null(obj$dy)) {
    yfields = intersect(c("y","y1","y2"),names(obj$geom))
    for (field in yfields) 
      obj$geom[[field]] = obj$geom[[field]]+obj$dy
  }
  bb$objs[[object.ind]]=obj
  bb
}


init.object.extras = function(obj) {
  restore.point("init.object.extras")
  
  if (isTRUE(obj$stop)) stop()
  lab = first.non.null(obj$latex, obj$label, "")
  obj$label.has.whiskers = grepl("{{",lab, fixed=TRUE)
  if (!is.null(obj$latex)) {
    obj$label.mode = "latex" 
  } else {
    obj$label.mode = "xlabel"
  }
  
  #obj$use.latex = !is.null(obj$latex) | is.null(obj$label)
  if (obj$label.mode == "latex" & !obj$label.has.whiskers) {
    obj$svg_label = svg.mathjax.label(lab)
  } else if (obj$label.mode == "xlabel" & !obj$label.has.whiskers) {
    obj$svg_label = latex.to.textspan(lab)
  } else {
    obj$svg_label = lab
  }  
  obj
}

obj.values = function(obj, bb) {
  if (!is.null(obj[["values"]])) return(obj$values)
  if (!is.null(obj[["data.row"]])) {
    if (!is.null(obj$data)) return(obj$data[obj$data.row,])
    if (!is.null(bb$data)) return(bb$data[obj$data.row,])
    
  }
  bb$values
}

compute_bb_fields = function(obj, fields, values=obj.values(obj,bb), enclos=bb$enclos, bb=NULL){
  restore.point("compute_bb_fields")
  li = lapply(obj[fields], function(field) {
    compute_bb_field(field, values=values, enclos=enclos)
  })
  li
}

compute_bb_field = function(field, values=obj.values(obj,bb), enclos=bb$enclos, bb=NULL, obj=NULL, character.field=FALSE) {
  restore.point("compute_bb_field")
  if(is.null(enclos)) enclos = parent.frame()
  if (is.null(field)) return(NULL)
  if (is.numeric(field)) return(field)
  
  if (is(field,"formula")) {
    if (length(field)==1) return(NULL)
    call = field[[2]]
    return(eval(call, values,enclos = enclos))
  }
  if (is(field,"call") | is(field,"name") | is(field,"expression")) {
    return(eval(field, values,enclos = enclos))
  }
  
  
  
  if (is.character(field) & !character.field) {
    if (length(field)>1) {
      res = sapply(field, function(f) {
        call = parse.as.call(f)
        return(eval(call, values,enclos = enclos))
      })
      return(res)
    }
    call = parse.as.call(field)
    return(eval(call, values,enclos = enclos))
  }
  return(field)
}
```
# END OF FILE: bb_comp.r

-----------------------------------------------------------


# FILE: bb_curve.r
```
#' Add an isoquant through a point
#'
#' @param bb A `bb` graphic object.
#' @param Q A production function or expression accepted by
#'   `isoquant.slope()`.
#' @param x,y Coordinates of a point on the isoquant.
#' @param ... Additional arguments passed to [bb_slopecurve()].
#' @param id A unique object identifier.
#' @param xvar,yvar Names of the horizontal and vertical variables.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_isoquant(Q = "x_ * y_", x = 2, y = 2)
#' }
bb_isoquant = function(bb, Q,x,y,...,id=paste0("isoquant_",random.string()),xvar=bb$xvar,yvar=bb$yvar) {
  restore.point("bb_isoquant")
  slope = isoquant.slope(Q,xvar,yvar)
  bb_slopecurve(bb,x=x,y=y,slope=slope,xvar=xvar,yvar=yvar,id=id,...)
}

#' Add a curve defined by its slope
#'
#' Numerically traces a curve through a supplied point using a slope
#' expression.
#'
#' @param bb A `bb` graphic object.
#' @param x,y Coordinates of a point on the curve.
#' @param slope A slope expression or character string.
#' @param color Stroke color.
#' @param lwd Stroke width.
#' @param alpha Stroke opacity.
#' @param style A list of SVG style properties.
#' @param x.move,y.move Offsets applied to the computed coordinates.
#' @param xrange,yrange Two-element coordinate ranges.
#' @param var.funs Optional variable functions used in computations.
#' @param tooltip Optional tooltip text.
#' @param ... Additional SVG style properties.
#' @param data Optional data used to evaluate expressions.
#' @param id A unique object identifier.
#' @param xvar,yvar Names of the horizontal and vertical variables.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_slopecurve(x = 2, y = 2, slope = "-y_ / x_")
#' }
bb_slopecurve = function(bb,x,y,slope,color=NULL, lwd=NULL, alpha=NULL, style=nlist(stroke=color, stroke_width=lwd,"stroke-opacity"=alpha,...), x.move=0, y.move=0, xrange=bb$xrange, yrange=bb$yrange, var.funs=NULL,tooltip=NULL,..., data=NULL, id=paste0("slopecurve_",random.string()),xvar=bb$xvar,yvar=bb$yvar) {
  restore.point("bb_slopecurve")
  
  if (is.character(slope)) {
    slope_ = parse.as.call(text=slope)
  } else {
    slope_ = slope
    slope = deparse1(slope_)
  }
  
  obj = nlist(type="slopecurve",id,x,y,slope,slope_,data,style, tooltip,xvar,yvar, eval.fields=c("x","y"), x.move, y.move,xrange,yrange)
  
  bb$objs[[obj$id]] = obj
  bb
}



#' Add a curve
#'
#' @param bb A `bb` graphic object.
#' @param id A unique object identifier.
#' @param eq An equation supplied as a character string.
#' @param latex Optional LaTeX curve label.
#' @param label Optional plain-text curve label.
#' @param data Optional data used to evaluate the equation.
#' @param color Stroke color.
#' @param lwd Stroke width.
#' @param style A list of SVG style properties.
#' @param var.funs Optional variable functions used to transform the equation.
#' @param labpos Optional two-element label position.
#' @param labx,laby Optional label coordinates.
#' @param tooltip Optional tooltip text.
#' @param dx,dy Coordinate offsets.
#' @param no.draw If `TRUE`, compute the curve without drawing it.
#' @param xrange,yrange Two-element coordinate ranges.
#' @param alpha Stroke opacity.
#' @param ... Additional SVG style properties.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_curve(eq = "y_ = 4 / x_", color = "blue")
#' }
bb_curve = function(bb,id=random.string(),eq,latex=NULL, label=NULL, data=NULL,color=NULL, lwd=NULL, style=nlist(stroke=color, stroke_width=lwd,"stroke-opacity"=alpha,...), var.funs=NULL,labpos=NULL,labx=NULL, laby=NULL,tooltip=NULL,dy=NULL,dx=NULL,no.draw=FALSE, xrange=bb$xrange, yrange=bb$yrange,alpha=NULL,...) {
  restore.point("bb_curve")
  
  curve = nlist(type="curve",id,eq,data,style,label,latex, tooltip,dx,dy, no.draw, xrange, yrange)
  
  curve$eq_ = parse.as.call(text=curve$eq)
  
  curve$xvar = bb$xvar
  curve$yvar = bb$yvar

  # Replace derivatives and variable functions
  if (!is.null(var.funs))
    curve$eq_ = compute.equation.funs(list(curve$eq_),var.funs)[[1]]
  
  res = specialize.curve.formula(curve$eq_, xvar=curve$xvar,yvar=curve$yvar)

  curve = c(curve, res)
  
  if (!is.null(curve$labpos)) {
    curve$labx = curve$labpos[[1]]
    curve$laby = curve$labpos[[2]]
  }
  
  if (!is.null(curve$labx)) {
    curve$labx_ = parse.as.call(paste0("(",curve$labx,")"))
  }
  if (!is.null(curve$laby)) {
    curve$laby_ = parse.as.call(paste0("(",curve$laby,")"))
  }
  if (!is.null(curve$labx_) & is.null(curve$laby_)) {
    restore.point("dfjdsfurgrbg")
    
    if (!is.null(curve$yformula_)) {
      li = list(curve$labx_)
      names(li) = xvar
      curve$laby_ = substitute.call(curve$yformula_,li)
    }
  }
  
  curve$type = "curve"
  curve = init.object.extras(curve)
  
  bb$objs[[curve$id]] = curve
  bb
}


# compute.curve.gcurve
#' Compute a curve's coordinates
#'
#' @param bb A `bb` graphic object.
#' @param curve A curve specification.
#' @param values Values used to evaluate the curve equation.
#' @param xlen,ylen Numbers of grid points in each direction.
#' @param xrange,yrange Two-element coordinate ranges.
#' @param ... Reserved for additional computation options.
#' @return The curve specification with computed geometry.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_curve(id = "diagonal", eq = "y_ = x_")
#' bb_compute_curve(bb, bb$objs$diagonal)
#' }
bb_compute_curve = function(bb,curve,values=bb$values, xlen=bb$xlen, ylen=bb$ylen,xrange=first.non.null(curve$xrange,bb$xrange),yrange=first.non.null(curve$yrange,bb$yrange), ...) {
  restore.point("bb_compute_curve")
  
  cu = curve
  xy = compute.curve.points(cu,xrange, yrange, values=values,xlen=xlen,ylen=ylen)
  
  if (!isTRUE((any(is.finite(xy$x+xy$y))))) {
    warning(paste0("No finite values for curve ", curve$id))
    return(NULL)
  }

  rows = xy$x >= min(xrange) & xy$x <= max(xrange) &
         xy$y >= min(yrange) & xy$y <= max(yrange) 

  x=xy$x[rows]
  y=xy$y[rows]
  
  curve$geom =list(type="curve",axis="",x=x,y=y,xrange=xrange,yrange=yrange, tooltip=curve$tooltip)
  curve
}

compute.curve.grid = function(cu=geom$obj, values=geom$values, xrange=geom$xrange,yrange=geom$yrange, xlen=geom$xlen,ylen=geom$ylen, dim="x",x=geom$x, y=geom$y, geom=NULL) {
  restore.point("compute.curve.grid")
  
  if (dim=="x") {
    xseq = seq(xrange[1], xrange[2], length=xlen)
    if (isTRUE(cu$is.vertical)) {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen)
      xy$x = round.to.grid(xy$x,length=xlen, range=xrange)
      return(xy)
    } else if (!is.null(cu$yformula_)) {
      values[[cu$xvar]] = xseq
      yseq = eval(cu$yformula_, values)
      if (length(yseq)==1) yseq <- rep(yseq,length(xseq))
      return(list(x=xseq,y=yseq))  

    } else if (!is.null(x) & !is.null(y)) {
      if (is.null(geom))
        geom = list(x=x,y=y, xrange=xrange, yrange=yrange,
                    xlen=xlen,ylen=ylen)
      return(compute.geom.grid(geom=geom,dim = dim,use.object = FALSE))
    } else {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen, use.xformula=FALSE)
      return(xy)
    }
  }

  if (dim=="y") {
    yseq = seq(yrange[1], yrange[2], length=xlen)
    if (isTRUE(cu$is.horizontal)) {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen)
      xy$y = round.to.grid(xy$y,length=ylen, range=yrange)
      return(xy)
    } else if (!is.null(cu$xformula_)) {
      values[[cu$yvar]] = yseq
      xseq = eval(cu$xformula_, values)
      if (length(xseq)==1) xseq <- rep(xseq,length(yseq))
      return(list(x=xseq,y=yseq))  

    } else if (!is.null(x) & !is.null(y)) {
      if (is.null(geom))
        geom = list(x=x,y=y, xrange=xrange, yrange=yrange,
                    xlen=xlen,ylen=ylen)
      return(compute.geom.grid(geom=geom,dim = dim,use.object = FALSE))
    } else {
      xy=compute.curve.points(cu, values=values, xrange=xrange,yrange=yrange, xlen=xlen,ylen=ylen, use.yformula=FALSE)
      return(xy)
    }
  }
   
  
}

compute.curve.points = function(cu, xrange, yrange, values, xlen=101,ylen=xlen, use.xformula=TRUE, use.yformula=TRUE, ...) {
  restore.point("compute.curve.points")

  #if (is.null(values)) values=list()
  values = as.list(values)
  
  if (isTRUE(cu$is.linear) & (!cu$is.vertical) & (!cu$is.horizontal)) {
    # need to add both x and y range to have at least 
    # 2 points inside the bb
    xseq = seq(xrange[1],xrange[2], length=2)
    values[[cu$xvar]] = xseq
    yval = eval(cu$yformula_, values)

    yseq = seq(yrange[1],yrange[2], length=2)
    values[[cu$yvar]] = yseq
    xval = eval(cu$xformula_, values)

    xy = adapt.linear.curve.points(x=c(xseq,xval),y=c(yval,yseq),xrange=xrange, yrange=yrange)

    return(xy)    
  }

  
  if (!is.null(cu$yformula_) & (!isTRUE(cu$is.vertical)) & use.yformula) {
    if (isTRUE(cu$is.horizontal) | isTRUE(cu$is.linear)) {
      xlen=2
    }
    xseq = seq(xrange[1],xrange[2], length=xlen)
    values[[cu$xvar]] = xseq
    yseq = eval(cu$yformula_, values)
    if (length(yseq)==1) yseq <- rep(yseq,length(xseq))
    return(list(x=xseq,y=yseq))    
  }
  if (!is.null(cu$xformula_) & use.xformula) {
    if (isTRUE(cu$is.vertical) | isTRUE(cu$is.linear)) {
      ylen=2
    }
    yseq = seq(yrange[1],yrange[2], length=ylen)
    values[[cu$yvar]] = yseq
    xseq = eval(cu$xformula_, values)
    if (length(xseq)==1) xseq <- rep(xseq,ylen)
    return(list(x=xseq,y=yseq))
  }
  
  li = compute.curve.implicit.z(cu, xrange, yrange, values, xlen=xlen,ylen=ylen, z.as.matrix=TRUE)
  options("max.contour.segments" =xlen) 
  res = contourLines(li$xseq,li$yseq,li$z, level = 0)
  if (length(res)==0) {
    res = NULL
  } else {
    res = res[[1]]
  }
  return(list(x = res$x, y=res$y))
}

adapt.linear.curve.points = function(x,y,xrange,yrange) {
  restore.point("adapt.linear.curve.points")
  
  rows = x >= min(xrange) & x <= max(xrange) &
         y >= min(yrange) & y <= max(yrange) 

  x=x[rows]
  y=y[rows]
  
  ord = order(x,y)
  x = x[ord]
  y = y[ord]
  ind = !duplicated(x)
  list(x=x[ind],y=y[ind])
}

compute.curve.implicit.z = function(cu, xrange, yrange,par,  xlen=101,ylen=xlen, z.as.matrix=FALSE) {
  restore.point("compute.implicit")
  
  # Compute a contour gcurve using the implicit function
  xseq = seq(xrange[1],xrange[2], length=xlen)
  yseq = seq(yrange[1],yrange[2], length=ylen)
  grid = expand.grid(list(x=xseq,y=yseq))

  par[[cu$xvar]] = grid$x
  par[[cu$yvar]] = grid$y
  grid$z = eval(cu$implicit_, par)
  
  if (z.as.matrix) {
    z = matrix(grid$z, nrow=length(xseq), ncol=length(yseq))
    return(list(xseq=xseq, yseq=yseq, z=z))
  }
  grid
}

draw.svg.slopecurve = function(...) {
  restore.point("draw.svg.slopecurve")
  draw.svg.curve(...)
}


draw.svg.curve = function(svg,obj,level=first.non.null(obj$level,0), display=NULL, bb=NULL) {
  restore.point("draw.svg.curve")
  display = init.geom.display(obj, display)
  geom=obj$geom
  
  svg_polyline(svg, id=obj$id, x=geom$x,y=geom$y, style=obj$style, level=level, tooltip=geom$tooltip,class = "curve", display=display)
}

# compute.curve.gcurve
#' Compute a slope-defined curve's coordinates
#'
#' @param bb A `bb` graphic object.
#' @param obj A slope-curve specification.
#' @param ... Reserved for additional computation options.
#' @return The curve specification with computed geometry.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 5), yrange = c(0, 5)) |>
#'   bb_slopecurve(x = 2, y = 2, slope = "-y_ / x_")
#' bb_compute_slopecurve(bb, bb$objs[[1]])
#' }
bb_compute_slopecurve = function(bb,obj, ...) {
  restore.point("bb_compute_curve")

  
  xy = compute_bb_fields(bb=bb, obj=obj,fields=c("x","y"))  #geom$x = round.to.grid(geom$x,)
  x = xy$x
  y = xy$y
  
  li = list(x,y)
  xvar = obj$xvar
  yvar = obj$yvar
  names(li) = c(xvar,yvar)
  values = c(li, obj.values(obj,bb))
  
  slope_ = obj$slope_
  
  dx = diff(obj$xrange) / bb$xlen
  nl = floor((x-obj$xrange[1]) / dx)
  nr = floor((obj$xrange[2]-x) / dx)
  xvec = unique(c(seq(x-nl*dx,x,by=dx),seq(x,x+nr*dx,by=dx)))
  
  #xvec = unique(sort(c(seq(obj$xrange[1],obj$xrange[2],length.out = bb$xlen),x)))

  
  ind = which(xvec==x)
  yvec = slope = rep(0, length(xvec))
  yvec[ind] = y
  slope[ind] = eval(slope_, values) 
  for (i in rev(seq_len(ind-1))) {
    yvec[i] = yvec[i+1]-slope[i+1]*dx
    values[[xvar]] = xvec[i]
    values[[yvar]] = yvec[i]
    slope[i] = eval(slope_, values) 
  }
  for (j in seq_len(length(xvec)-ind)) {
    i = j+ind
    yvec[i] = yvec[i-1]+slope[i-1]*dx
    values[[xvar]] = xvec[i]
    values[[yvar]] = yvec[i]
    slope[i] = eval(slope_, values) 
  }
  
  xvec = xvec+obj$x.move
  yvec = yvec+obj$y.move
  
  # crop curve to range
  ok = which(yvec >= obj$yrange[1] & yvec <= obj$yrange[2] & xvec >= obj$xrange[1] & xvec <= obj$xrange[2])
  start.ind = min(ok)
  end.ind = max(ok)
  
  xvec = xvec[start.ind:end.ind]
  yvec = yvec[start.ind:end.ind]
  slope = slope[start.ind:end.ind]
  
  
  obj$geom = list(x=xvec, y=yvec, slope=slope)
  obj
}


specialize.curve.formula = function(eq, xvar, yvar, level=NULL, solve.symbolic = require(symbeqs)) {
  restore.point("specizalize.curve.formula")
  formula_ = eq
  lhs_ = get.lhs(formula_)
  lhs = deparse1(lhs_)
  rhs_ = get.rhs(formula_)
  
  vl = find.variables(lhs_)
  vr = find.variables(rhs_)

  curve.funs = find.funs(rhs_)
  
  yformula_ = xformula_ = NULL

  curve.vars = c(vl, vr)
  is.vertical = !yvar  %in% curve.vars
  is.horizontal = ! xvar  %in% curve.vars

  # y variable is alone on lhs
  if (identical(lhs,yvar) & (! yvar %in% vr)) {
    yformula_ = substitute(rhs, list(rhs=rhs_))

  } else if (solve.symbolic) {
    res = sym.solve.eq(eq,yvar, simplify=TRUE)
    if (res$solved)
      yformula_ = res$eq[[3]]
    
  }

  # x variable is alone on lhs
  if (identical(lhs,xvar) & (! xvar %in% vr)) {
    xformula_ = substitute(rhs, list(rhs=rhs_))
  } else if (solve.symbolic) {
    res = sym.solve.eq(eq,xvar, simplify=TRUE)
    if (res$solved)
      xformula_ = res$eq[[3]]
  }
  
  # implicit formula
  implicit_ = substitute(lhs-(rhs), list(lhs=lhs_,rhs=rhs_))
  
  curve = nlist(eq_=eq,yformula_, xformula_,implicit_,is.horizontal, is.vertical,xvar,yvar)
  slope_ = compute.curve.slope(curve)
  slope.vars = find.variables(slope_)
  
  is.linear = (!xvar %in% slope.vars) & (! yvar %in% slope.vars) & length(curve.funs)==0
  
  ret = nlist(xformula_, yformula_, implicit_,slope_, is.vertical, is.horizontal, is.linear, curve.vars, slope.vars, parnames = setdiff(curve.vars,c(xvar,yvar)))
  ret
}

# compute symbolically a curve's slope
compute.curve.slope = function(curve) {
  restore.point("compute.curve.slope")
  slope = NULL
  try({
    if (isTRUE(curve$is.horizontal)) {
      slope = 0
    } else if (isTRUE(curve$is.vertical)) {
      slope = Inf
    } else if (!is.null(curve$yformula_)) {
      slope = Deriv::Deriv(curve$yformula_, curve$xvar)
    } else if (!is.null(curve$xformula_)) {
      slope = substitute(1 / (invslope))
    } else {
      dFdx =  Deriv::Deriv(curve$implicit_, curve$xvar)
      dFdy =  Deriv::Deriv(curve$implicit_, curve$yvar)
      slope = Deriv::Simplify(substitute(-dFdx/dFdy))
    }
  }, silent = TRUE)
  if (is(slope,"try-error"))
    slope = NULL
  slope  
}

```
# END OF FILE: bb_curve.r

-----------------------------------------------------------


# FILE: bb_export.r
```
# inline svg links to create a self-contained svg image

examples.export.svg = function() {
  library(bbsvg)
  setwd("D:/libraries/bbsvg/")
  bb = bb_pane(xrang=c(0,1),yrange=c(0,1)) %>%
    bb_xmarker(x=0.5,y=0.5, latex="\\hat{x}")
  bb_to_pdf(bb,"bb.pdf")
  
  svg = bb_to_svg(bb)
  #cat(svg)
  writeLines(svg, "test.svg")
  library(rsvg)
  rsvg_png("test.svg", "test.png")
  rsvg_pdf("test.svg","test.pdf")
  view.bb(bb)
}


svg_to_pdf = function(svg.file = NULL,out.file=paste0(tools::file_path_sans_ext(svg.file),".pdf"),svg=merge.lines(readLines(svg.file))) {
  library(rsvg)
  restore.point("save_to_pdf")
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_pdf(charToRaw(svg),out.file)
}


svg_to_png = function(svg.file = NULL,out.file=paste0(tools::file_path_sans_ext(svg.file),".png"),svg = merge.lines(readLines(svg.file)), ...) {
  library(rsvg)
  if (length(svg))
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_png(charToRaw(svg),out.file)
}

svg_to_ps = function(svg.file = NULL,out.file=paste0(tools::file_path_sans_ext(svg.file),".ps"),svg=merge.lines(readLines(svg.file)), ...) {
  library(rsvg)  
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_ps(charToRaw(svg),out.file)
}


#' Export a graphic to PDF
#'
#' @param bb A `bb` graphic object.
#' @param file Output file path.
#' @param ... Additional arguments passed through the export pipeline.
#' @return The result returned by the SVG conversion function, invisibly where
#'   applicable.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 1), yrange = c(0, 1))
#' bb_to_pdf(bb, "figure.pdf")
#' }
bb_to_pdf = function(bb, file,...) {
  library(rsvg)
  svg = bb_to_svg(bb)
  svg_to_pdf(svg=svg, out.file=file)
}

#' Export a graphic to PNG
#'
#' @inheritParams bb_to_pdf
#' @return The result returned by the SVG conversion function, invisibly where
#'   applicable.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 1), yrange = c(0, 1))
#' bb_to_png(bb, "figure.png")
#' }
bb_to_png = function(bb, file,...) {
  svg = bb_to_svg(bb)
  svg_to_png(svg=svg, out.file=file)
}

#' Export a graphic to PostScript
#'
#' @inheritParams bb_to_pdf
#' @return The result returned by the SVG conversion function, invisibly where
#'   applicable.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 1), yrange = c(0, 1))
#' bb_to_ps(bb, "figure.ps")
#' }
bb_to_ps = function(bb, file,...) {
  svg = bb_to_svg(bb)
  svg_to_ps(svg=svg, out.file=file)
}
```
# END OF FILE: bb_export.r

-----------------------------------------------------------


# FILE: bb_funs.r
```
var_at_posvar = function(id, posval,  var, posvar,bb=cur.bb()) {
  restore.point("var_at_pos")
  obj = bb$objs[[id]]; geom = obj$geom
  if (is.null(geom[[posvar]])) {
    stop(paste0("No ", posvar, " defined for object ", id))  
  }
  posval = compute_bb_field(posval, values=bb$values, bb=bb)
  ind = closest.index(geom[[posvar]],posval)
  if (length(var)>1)  {
    res = sapply(geom[var],function(g) g[ind])
    return(res)
  }
  return(geom[[var]][ind])
}

xy_at_slope = function(id, slope, bb=cur.bb()) {
  var_at_posvar(id,slope,c("x","y"),"slope", bb)
}

x_at_y = function(id, y, bb=cur.bb()) {
  var_at_posvar(id,y,"x","y", bb)
}

y_at_x = function(id, x, bb=cur.bb()) {
  var_at_posvar(id,x,"y","x", bb)
}


x_at_slope = function(id, slope, bb=cur.bb()) {
  var_at_posvar(id,slope,"x","slope", bb)
}

y_at_slope = function(id, slope, bb=cur.bb()) {
  var_at_posvar(id,slope,"y","slope", bb)
}


slope_at_x = function(id, slope, bb=cur.bb()) {
  var_at_posvar(id,slope,"slope","x", bb)
}
slope_at_y = function(id, slope, bb=cur.bb()) {
  var_at_posvar(id,slope,"slope","y", bb)
}



closest.index = function(vec, val) {
  e2 = abs(vec-val)
  which.min(e2)
}
```
# END OF FILE: bb_funs.r

-----------------------------------------------------------


# FILE: bb_label.r
```

#' Add text to a graphic
#'
#' @param bb A `bb` graphic object.
#' @param label Plain-text label.
#' @param latex LaTeX label. If supplied, it takes precedence over `label`.
#' @param x,y Absolute label coordinates.
#' @param xrel,yrel Relative label coordinates between zero and one.
#' @param align Horizontal alignment.
#' @param x.offset,y.offset Pixel offsets from the label coordinates.
#' @param boxed Whether to use boxed-label styling.
#' @param font_size Font size in pixels.
#' @param color Text color.
#' @param style A list of SVG style properties.
#' @param valign Vertical alignment.
#' @param vertical Whether to rotate the text vertically.
#' @param ... Additional label properties.
#' @param id A unique object identifier.
#' @param fill.background Whether to draw a background behind plain text.
#' @param background.alpha Background opacity.
#' @param background.color Background color.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_text("Equilibrium", x = 2, y = 2)
#' }
bb_text = function(bb, label=NULL,latex=NULL,x=NULL,y=NULL,xrel=NULL, yrel=NULL,align="center", x.offset=0, y.offset=NULL, boxed=FALSE, font_size=14, color=NULL, style=list("font-size"=font_size,"fill"=color),valign=c("center","bottom","top")[1], vertical=FALSE,  ..., id=random.string(), fill.background=FALSE, background.alpha=0.8, background.color="#ffffff") {
  restore.point("bb_text")
  use.latex = !is.null(latex)
  bb$use.latex = isTRUE(bb$use.latex) | use.latex
  
  if (use.latex) label = NULL
  
  if (is.null(y.offset)) {
    if (valign =="center") {
      y.offset = -ceiling(0.4*font_size)
    } else if (valign=="top") {
      y.offset = -ceiling(font_size)
    } else {
      y.offset = 0
    }

  }

  ma = bb.normalize.multi.arguments(nlist(x,y,label,latex))
  
  if (ma$len == 1) {
    obj = nlist(id, label, latex, x,y,xrel,yrel, use.latex,align,label.mode=ifelse(use.latex,"latex","text"), x.offset, y.offset, boxed,style, font_size,vertical=vertical, color, ...)
    if (fill.background) {
      bg = text.background.obj(obj, alpha=background.alpha, color=background.color)
      if (!is.null(bg)) bb$labels[[bg$id]] = bg
    }
    
    bb$labels[[id]] = obj
    return(bb)
  }
  restore.point("bb_text.multi")

  bid = id
  for (i in seq_len(ma$len)) {
    id = paste0(bid,"_",i)
    bb = bb_text(bb,id=id, label=ma$li$label[[i]], latex =ma$li$latex[[i]], x=ma$li$x[[i]],y=ma$li$y[[i]], use.latex=use.latex,align=align, x.offset=x.offset, y.offset=y.offset, boxed=boxed,style=style, font_size=font_size,vertical=vertical, color=color, fill.background=fill.background, ...)

  }
  bb
  
  
}



# create a new text object that functions as a 
# (white) background for the original text object
text.background.obj = function(obj,color="#ffffff", alpha=0.8, ...) {
  restore.point("text.background.obj")
  if (is.null(obj$label))
    return(NULL)
  label = sep.lines(obj$label)
  bg = sapply(label, function(el) paste0(rep("█", nchar(el)), collapse=""))
  obj$label = mark_utf8(bg)
  obj$style$fill = color
  obj$style[["fill-opacity"]] = alpha
  obj$id = paste0(obj$id,"-background")
  obj
}

#' Add a custom x-axis tick
#'
#' @param bb A `bb` graphic object.
#' @param x Horizontal tick coordinate.
#' @param ... Additional arguments passed to [bb_text()].
#' @param label Plain-text tick label.
#' @param latex LaTeX tick label.
#' @param align Horizontal label alignment.
#' @param y.offset Vertical pixel offset.
#' @param y Vertical coordinate of the label.
#' @param id A unique object identifier.
#' @param add.tick.line Whether to add a tick mark to the axis.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_xtick(x = 2, label = "two")
#' }
bb_xtick = function(bb,x=NULL,...,label=x,latex=NULL, align="center", y.offset=-20,y=NULL, id = random.string(),  add.tick.line=TRUE) {
  restore.point("bb_xtick")
  y=first.non.null(y,bb$y.min)
  bid = id
  
  bb = bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, y.offset=y.offset, ..., id=id)
  if (add.tick.line) {
    bb$custom.xticks = c(bb$custom.xticks,x)
  }

  bb
}

#' Add a custom y-axis tick
#'
#' @param bb A `bb` graphic object.
#' @param y Vertical tick coordinate.
#' @param ... Additional arguments passed to [bb_text()].
#' @param label Plain-text tick label.
#' @param latex LaTeX tick label.
#' @param align Horizontal label alignment.
#' @param x.offset Horizontal pixel offset.
#' @param x Horizontal coordinate of the label.
#' @param id A unique object identifier.
#' @param add.tick.line Whether to add a tick mark to the axis.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_ytick(y = 2, label = "two")
#' }
bb_ytick = function(bb,y=NULL,...,label=y,latex=NULL, align="right", x.offset=ifelse(add.tick.line,-15,-5), x=NULL, id = random.string(), add.tick.line=TRUE) {
  restore.point("bb.ytick")
  x=first.non.null(x,bb$x.min)
  #bb = bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, x.offset=x.offset,id=id)
  bb = bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, x.offset=x.offset, ..., id=id)

  if (add.tick.line) {
    bb$custom.yticks = c(bb$custom.yticks,y)
  }
  
  
  bb
}


compute_bb_label = function(bb, obj) {
  restore.point("compute_bb_label")
  geom = compute_bb_fields(obj,c("x","y","xrel","yrel","x.offset","y.offset","boxed"), bb$values)

  if (!is.null(geom$xrel))
    geom$x = geom$xrel*max(bb$xrange) + (1-geom$xrel)*min(bb$xrange)

  if (!is.null(geom$yrel))
    geom$y = geom$yrel*max(bb$yrange) + (1-geom$yrel)*min(bb$yrange)


  geom$label = first.non.null(obj$label, obj$latex)
  
  geom$label = compute_bb_field(geom$label,bb=bb, obj=obj, character.field = TRUE)

  geom$tooltip = obj[["tooltip"]]
  obj$geom = geom
  obj

}

draw.svg.label = function(svg,obj, display.whisker=FALSE,bb=NULL) {
  restore.point("draw.svg.label")
  geom = obj$geom
  display=""
  if (display.whisker)
    display = paste0("{{display_",obj$id,"}}")

  x = domain.to.range(x = geom$x,svg = svg)
  org.x = x
  if (!is.null(geom$x.offset))
    x = x + geom$x.offset
  y = domain.to.range(y = geom$y,svg = svg)
  org.y = y
  if (!is.null(geom$y.offset))
    y = y - geom$y.offset

  if (isTRUE(obj$label.mode=="latex")) {
    align = obj$align
    if (align=="right") align="R"
    if (align=="left") align="L"
    if (align=="center") align=""


    svg_mathjax_label(svg,x=x,y=y, text=geom$label,id=obj$id, level=first.non.null(obj$level,100), align=align, to.range = FALSE, tooltip = geom$tooltip, display=display, color=obj$color)

  } else {
    anchor = "middle"
    if (obj$align=="right") anchor = "end"
    if (obj$align=="left") anchor ="start"

    transform=NULL
    if (isTRUE(obj$vertical)) 
      transform=paste0("rotate(-90, ",org.x,", ",org.y,")")
    svg_text(svg,x=x,y=y, text=geom$label,id=obj$id, class="bb_text", level=first.non.null(obj$level,100), font_size=obj$font_size, style=obj$style, to.range = FALSE, "text-anchor"=anchor,transform=transform, tooltip = geom$tooltip, display=display)

  }
}


svg_text = function(svg, x,y, text,id=NULL, class="boxed-label",style=c(nlist("font-size"=font_size), extra.style), font_size=NULL, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE, math.label=TRUE,...) {
  restore.point("svg_text")

  text = sep.lines(text)
  if (length(text)>1) {
    text = multiline.tspans(text,x = x,y=y)
  } else if (math.label){
    text = latex.to.textspan(text)
  }

  rp = domain.to.range(x=x,y=y,svg=svg, to.range=to.range)
  el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,class,style,...), tooltip=tooltip, inner=text)
  #el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,style,...), tooltip=tooltip, inner=text)
  svg_add(svg,el,id,level=level)
}

multiline.tspans = function(txt,x,y, font_size=14, row_height = font_size+2) {
  if (length(txt)==1)
    txt = sep.lines(txt)
  y = y+ ((1:length(txt))-1)*row_height
  code = paste0('<tspan x="',x,'" y= "',y,'">',txt,'</tspan>',collapse="\n")
  code
}

bb.normalize.multi.arguments = function(li) {
  restore.point("bb.normalize.multi.arguments")
  len = sapply(li, function(el) {
    if (is(el,"formula") | is.expression(el) | is.call(el)) return(1)
    length(el)
  })
  if (all(len==1)) return(list(len=max(len),li=li))
  for (i in which(len==1)) {
    li[[i]] = replicate(n=max(len),li[[i]],simplify = FALSE)
  }
  return(list(len=max(len),li=li))
}
```
# END OF FILE: bb_label.r

-----------------------------------------------------------


# FILE: bb_line.r
```

#' Add a tangent line
#'
#' @param bb A `bb` graphic object.
#' @param x,y Coordinates of the tangency point.
#' @param slope The tangent slope.
#' @param width Total line width, or a two-element vector of widths on either
#'   side of the tangency point.
#' @param alpha Stroke opacity.
#' @param color Stroke color.
#' @param class SVG class name.
#' @param linetype Line type.
#' @param to Optional identifier of a curve from which missing tangent
#'   quantities are computed.
#' @param ... Additional arguments passed to [bb_segment()].
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_tangent(x = 2, y = 2, slope = 1)
#' }
bb_tangent = function(bb, x=NULL,y=NULL, slope=NULL, width=NULL, alpha=NULL,color=NULL, class="segment",linetype="solid", to=NULL, ..., id=paste0("tangent_",random.string())) {
  if (is.null(width)) {
    width = 2*(diff(bb$xrange)+diff(bb$yrange))
  }
  if (length(width)==1) {
    sw = ew = paste0("0.5 * ",width)
  } else {
    sw = width[1]
    ew = width[2]
  }
  if (!is.null(to)) {
    if (is.null(x)) x = paste0('x_at_slope("',to,'","',slope,'")')
    if (is.null(y)) y = paste0('y_at_slope("',to,'","',slope,'")')
    if (is.null(slope)) slope = paste0('slope_at_x("',to,'","',x,'")')
  }
  
  x1 = paste0(x, " - ", sw)
  x2 = paste0(x, " + ", ew)
  y1 = paste0(y, " - ", slope, " * ",sw)
  y2 = paste0(y, " + ", slope, " * ",ew)

  bb_segment(bb,x1=x1,x2=x2,y1=y1,y2=y2,alpha=alpha,color=color, class="segment",linetype=linetype,...)
}

#' Add a horizontal line
#'
#' @param bb A `bb` graphic object.
#' @param y Vertical coordinate.
#' @param alpha Stroke opacity.
#' @param color Stroke color.
#' @param class SVG class name.
#' @param linetype Line type.
#' @param lwd Stroke width.
#' @param style A list of SVG style properties.
#' @param ... Additional SVG style properties.
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |> bb_hline(y = 2)
#' }
bb_hline = function(bb,y,alpha=NULL,color=NULL, class="segment",linetype="solid", lwd=NULL,  style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), ..., id=paste0("hline_",random.string())) {
  bb_segment(bb,y=y, x1=bb$x.min, x2=bb$x.max, color=color, class=class, alpha=alpha, lwd=lwd, linetype=linetype, style=style, id=id)
}

#' Add a vertical line
#'
#' @inheritParams bb_hline
#' @param x Horizontal coordinate.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |> bb_vline(x = 2)
#' }
bb_vline = function(bb,x,alpha=NULL,color=NULL, class="segment",linetype="solid", lwd=NULL,  style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), ..., id=paste0("hline_",random.string())) {
  bb_segment(bb,x=x, y1=bb$y.min, y2=bb$y.max, color=color, class=class, alpha=alpha, lwd=lwd, linetype=linetype, style=style, id=id)
}

#' Add a line segment
#'
#' @param bb A `bb` graphic object.
#' @param x1,y1 Coordinates of the first endpoint.
#' @param x2,y2 Coordinates of the second endpoint.
#' @param x,y Convenience coordinates used by the endpoint defaults.
#' @param alpha Stroke opacity.
#' @param color Stroke color.
#' @param class SVG class name.
#' @param linetype Line type.
#' @param lwd Stroke width.
#' @param dasharray SVG stroke-dasharray value.
#' @param style A list of SVG style properties.
#' @param ... Additional SVG style properties.
#' @param tooltip Optional tooltip text.
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_segment(x1 = 1, y1 = 1, x2 = 3, y2 = 3)
#' }
bb_segment = function(bb, x1=x,x2=x1,y1=y,y2=y1,x,y, alpha=NULL,color=NULL, class="segment",linetype="solid", lwd=NULL, dasharray = linetype.to.dasharry(linetype),  style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), ..., tooltip=NULL, id=paste0("segment_",random.string())) {
  restore.point("bb_segment")

  ma = bb.normalize.multi.arguments(nlist(x1,x2,y1,y2))

  if (ma$len == 1) {
     obj = nlist(id, type="segment", class=class, x1=x1,y1=y1,x2=x2,y2=y2, style,"stroke-dasharray"=dasharray, eval.fields=c("x1","y1","x2", "y2"), tooltip=tooltip)
    return(bb_object(bb, obj))
  }
  restore.point("bb_segment.multi")

  bid = id
  for (i in seq_len(ma$len)) {
    id = paste0(bid,"_",i)
    bb = bb_segment(bb,id=id, x1=ma$li$x1[[i]],x2=ma$li$x2[[i]],y1=ma$li$y1[[i]],y2=ma$li$y2[[i]], style=style, font_size=font_size, alpha=alpha, color=color, dasharray=dasharray,linetype=linetype, lwd=lwd, ...)

  }
  bb
}



#' Add an arrow
#'
#' @param bb A `bb` graphic object.
#' @param x1,y1 Coordinates of the first endpoint.
#' @param x2,y2 Coordinates of the second endpoint.
#' @param x,y Convenience coordinates used by the endpoint defaults.
#' @param arrow.head Which endpoint receives an arrow head.
#' @param alpha Stroke opacity.
#' @param color Stroke color.
#' @param class SVG class name.
#' @param style A list of SVG style properties.
#' @param ... Additional SVG style properties.
#' @param id A unique object identifier.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(0, 4), yrange = c(0, 4)) |>
#'   bb_arrow(x1 = 1, y1 = 1, x2 = 3, y2 = 3)
#' }
bb_arrow = function(bb, x1=x,x2=x1,y1=y,y2=y1,x, y, arrow.head=c("end"), alpha=NULL,color=NULL, class="arrow", style=list(stroke=color, "stroke-opacity"=alpha,...), ..., id=random.string()) {
  obj = nlist(id, type="arrow", class, x1,y1,x2,y2, style, eval.fields=c("x1","y1","x2", "y2"))
  bb_object(bb, obj)
}

crop.bb.segment = function(obj,bb) {
  restore.point("crop.bb.segment")
  g = obj$geom
  obj$geom[c("x1","x2","y1","y2")] = crop.segment.to.range(x1=g$x1, x2=g$x2, y1=g$y1,y2=g$y2,xrange=bb$xrange, yrange=bb$yrange, return4list=TRUE)
  obj
}

crop.segment.to.range = function(x=c(x1,x2),y=c(y1,y2),xrange,yrange,x1,x2,y1,y2, return4list=FALSE) {
  restore.point("crop.segment.to.range")
  
  x.inv = x[2]<x[1]
  y.inv = y[2]<y[1]

  horizontal = y[1] == y[2]
  vertical = x[1] == x[2]
  ox = x; oy = y;
  
  if (horizontal) {
    x[1] = min(xrange[2],max(xrange[1],x[1]))
    x[2] = min(xrange[2],max(xrange[1],x[2]))
    
  } else if (vertical) {
    y[1] = min(yrange[2],max(yrange[1],y[1]))
    y[2] = min(yrange[2],max(yrange[1],y[2]))
    

  # downward sloping curve
  } else if (!x.inv & y.inv) {
    xr.alpha = (xrange-x[1]) / diff(x)
    yr.alpha = (rev(yrange)-y[1]) / diff(y)
    
    alpha.start = max(xr.alpha[1],yr.alpha[1], 0)
    alpha.end = min(xr.alpha[2],yr.alpha[2], 1)
    
    if (alpha.start > 0) {
      alpha = alpha.start
      x[1] = (1-alpha)*ox[1] + alpha*ox[2]
      y[1] = oy[1] - alpha*abs(diff(oy))
    }
    if (alpha.end < 1) {
      alpha = alpha.end
      x[2] = (1-alpha)*ox[1] + alpha*ox[2]
      y[2] = oy[1] - alpha*abs(diff(oy))
    }
    
  # upward sloping curve  
  } else if( !x.inv & !y.inv) {
    xr.alpha = (xrange-x[1]) / diff(x)
    yr.alpha = (yrange-y[1]) / diff(y)
    
    alpha.start = max(xr.alpha[1],yr.alpha[1], 0)
    alpha.end = min(xr.alpha[2],yr.alpha[2], 1)
    
    if (alpha.start > 0) {
      alpha = alpha.start
      x[1] = (1-alpha)*ox[1] + alpha*ox[2]
      y[1] = (1-alpha)*oy[1] + alpha*oy[2]
    }
    if (alpha.end < 1) {
      alpha = alpha.end
      x[2] = (1-alpha)*ox[1] + alpha*ox[2]
      y[2] = (1-alpha)*oy[1] + alpha*oy[2]
    }
  } else {
    stop("cropping of segments with inveres x order not yet implemented!")
  }

  if (return4list) {
    return(list(x1=x[1],x2=x[2],y1=y[1],y2=y[2]))
  }
  return(list(x=x,y=y))
}
```
# END OF FILE: bb_line.r

-----------------------------------------------------------


# FILE: bb_series.r
```
examples.bb_series = function() {
  setwd("D:/lehre/vwl_einf")
  d = readRDS("mh_de.rds")

  bb = bb_pane(show.ticks = TRUE, org.width=800) %>%
    bb_series(x=1:5,y=1:5) %>%
    bb_series_tooltip_bars(lwd=20)     
  view.bb(bb)
    
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
    bb_period(2007,2009+0.5,"Finanzkrise") %>%
    bb_series_tooltip_bars()  
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

#' Mark a period on a series plot
#'
#' @param bb A `bb` graphic object.
#' @param from Start coordinate.
#' @param to Optional end coordinate. If supplied, the period is shaded.
#' @param label Plain-text period label.
#' @param shade Shading color.
#' @param alpha Shading opacity.
#' @param lwd Boundary-line width.
#' @param linetype Boundary-line type.
#' @param tooltip Boundary tooltip text.
#' @param area.tooltip Shaded-area tooltip text.
#' @param latex Optional LaTeX label.
#' @param font_size Label font size.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane(xrange = c(2000, 2010), yrange = c(0, 5)) |>
#'   bb_period(2003, 2005, "Event")
#' }
bb_period = function(bb, from,to=NULL,label=NULL, shade="#555555", alpha=0.3, lwd=1, linetype="dashed", tooltip=label, area.tooltip = tooltip, latex=NULL, font_size=11) {
  restore.point("bb_period")
  if (!is.null(to))
    bb = bb_area(bb,x = c(from, from,to,to),y=c(bb$y.min,bb$y.max,bb$y.max,bb$y.min),fill = shade, alpha=alpha, tooltip=area.tooltip)
  bb = bb_segment(bb,x1=from,x2=from,y1=bb$y.min,y2=bb$y.max, linetype=linetype, lwd=lwd, tooltip=tooltip)
  bb = bb_text(bb, x= from, y=bb$y.min, label=label, latex=latex,font_size = font_size, vertical=TRUE, align="left", valign = "bottom",y.offset=2)
  bb
}

#' Add a data series
#'
#' @param bb A `bb` graphic object.
#' @param x,y Series coordinates.
#' @param data Data frame containing the series.
#' @param xvar,yvar Column names or positions used to obtain `x` and `y`.
#' @param name Series name used in tooltips.
#' @param alpha Overall opacity.
#' @param color Series color.
#' @param class SVG class name.
#' @param linetype Line type.
#' @param lwd Line width.
#' @param plot_type Plot type descriptor.
#' @param line.style,point.style Lists of SVG style properties.
#' @param dasharray SVG stroke-dasharray value.
#' @param ... Additional SVG style properties.
#' @param id A unique object identifier.
#' @param level Drawing level.
#' @param draw.line,draw.points Whether to draw lines and points.
#' @param r Point radius.
#' @param line.alpha,point.alpha Line and point opacity.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane() |>
#'   bb_series(x = 1:5, y = c(1, 3, 2, 4, 5), color = "blue")
#' }
bb_series = function(bb, x=data[[xvar]],y=data[[yvar]],data=bb$data, xvar=1,yvar=2,name= if (is.character(yvar)) yvar else id,alpha=NULL,color=NULL, class="series",linetype="solid", lwd=NULL, plot_type="l",line.style=list(stroke=color, "stroke-opacity"=line.alpha, "stroke-width"=lwd,...), point.style = list(fill=color, "fill-opacity"=point.alpha),
  dasharray = linetype.to.dasharry(linetype),...,id=paste0("series_",random.string()), level=10, draw.line=TRUE, draw.points=FALSE, r=3,line.alpha=alpha, point.alpha=alpha) {
  restore.point("bb_series")
  
  na.rows = is.na(x) | is.na(y)
  x = x[!na.rows]
  y = y[!na.rows]
  
  obj = nlist(id, type="series", class=class, x=x,y=y, line.style,point.style,"stroke-dasharray"=dasharray, eval.fields=c("x","y"), level=level, draw.line, draw.points,r, name)

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


draw.svg.series = function(svg,obj, level=0, display=NULL,bb=NULL) {
  restore.point("draw.svg.series")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  #r = domain.to.range(x=geom$x, y=geom$y, svg=svg)  
 
  svg = svg_polyline(svg=svg,x=geom$x,y=geom$y, style=obj$line.style,level = level, id=obj$id)
  

  if (!obj$draw.points) {
    svg = svg_tooltip_circles(svg, x=geom$x,y=geom$y, alpha=0)
  } else {
    svg = svg_tooltip_circles(svg, x=geom$x,y=geom$y, style=obj$point.style, r=obj$r)
  }
  svg
}

#' Add tooltip bars for data series
#'
#' @param bb A `bb` graphic object.
#' @param xname Label used for the horizontal coordinate in tooltips.
#' @param color Bar color.
#' @param lwd Bar width.
#' @param style A list of SVG style properties.
#' @param id A unique object identifier.
#' @param level Drawing level.
#' @param round.digits,signif.digits Numeric formatting controls.
#' @param tooltip.fun Optional function that creates tooltip text.
#' @param tooltip.data Optional data passed to `tooltip.fun`.
#' @param ... Additional object properties.
#' @return The modified `bb` object.
#' @examples
#' \dontrun{
#' bb_pane() |>
#'   bb_series(x = 1:5, y = 1:5) |>
#'   bb_series_tooltip_bars()
#' }
bb_series_tooltip_bars = function(bb, xname="t", color="yellow", lwd=11, style=list(stroke=color, "stroke-width"=lwd), id=paste0("series_tooltip_bars",random.string()), level=11, round.digits=2, signif.digits=5,tooltip.fun=NULL,tooltip.data=NULL,...) {
  obj = nlist(id, type="series_tooltip_bars",xname,color, style, level, round.digits, signif.digits, tooltip.fun=tooltip.fun,tooltip.data=tooltip.data)
  bb_object(bb,obj)
  
}

draw.svg.series_tooltip_bars = function(svg,obj, level=obj$level, display=NULL,bb=NULL) {
  restore.point("draw.svg.series_tooltip_bars")
  
  #tooltip.data = first.non.null(obj[["data"]],data)
  data = NULL
  if (is.null(data)) {
    is.ser = sapply(bb$objs, function(obj)obj$type=="series") 
    ser = bb$objs[is.ser]
    if (length(ser)==0) return()
    
    li = lapply(ser, function(obj) data_frame(name=obj$name,x=obj$geom$x,y=obj$geom$y))
    df = bind_rows(li)
    library(tidyr)
    data = spread(df, key = name, value=y)
    xcol="x"
    ycol = colnames(data)[-1]
  }
  
  cols = c(xcol,ycol)
  data[cols] = lapply(data[cols], function(val) {
    if (!is.null(obj$round.digits)) val = round(val, obj$round.digits)
    if (!is.null(obj$signif.digits)) val = signif(val, obj$signif.digits)
    val
  })
  
  rx= domain.to.range(x=data[[xcol]], svg=svg)
  ry = domain.to.range(y=bb$yrange, svg=svg)
  
  style = make_style_arg(obj$style)
  
  if (is.null(obj$tooltip.fun)) {
    tooltip = paste0(obj$xname,":", data[[xcol]])
    for (col in ycol) {
      tooltip = paste0(tooltip,"\n", col, ": ", data[[col]])
    }
  } else {
    tooltip.data = first.non.null(obj[["tooltip.data"]],data)
    tooltip = obj$tooltip.fun(data=tooltip.data, obj=obj)
  }
  
  txt = paste0('<line x1="',rx,'" x2="',rx,'" y1="',ry[1],'"  y2="',ry[2],'" style="',style,'" class="series_tooltip_bar"> <title>',tooltip,'</title></line>')
  txt = paste0('<g id="', obj$id,'">', paste0(txt, collapse="\n"),"</g>")
  svg_add(svg, txt, obj$id)
  
}

svg_tooltip_circles = function(svg, x, y, tooltip=paste0(round(x,2),",",round(y,2)), r=5, alpha=0.5, color="black", id=paste0("tooltips_", random.string()), style=list(fill=color, "fill-opacity"=alpha)) {
  restore.point("svg_tooltip_circles")
  
  ra = domain.to.range(x=x, y=y, svg=svg)
  
  style = make_style_arg(style)
  txt = paste0('<circle cx="',ra$x,'" cy="',ra$y,'" r="',r,'" style="',style,'"> <title>',tooltip,'</title></circle>')
  txt = paste0('<g id="', id,'">', paste0(txt, collapse="\n"),"</g>")
  svg_add(svg, txt, id)
}
```
# END OF FILE: bb_series.r

-----------------------------------------------------------


# FILE: bb_svg.r
```
#' Render a graphic as SVG
#'
#' @param bb A `bb` graphic object.
#' @param file Optional output file path.
#' @param id SVG element identifier.
#' @param css CSS included in the SVG.
#' @param width,height Output dimensions in pixels.
#' @param return.svg.object If `TRUE`, return the intermediate SVG object.
#' @param latexsvg Whether to convert LaTeX labels with `latexsvg`.
#' @param outfile Deprecated alternative output-file argument.
#' @param ... Reserved for additional rendering options.
#' @return An SVG character string, or the intermediate SVG object when
#'   `return.svg.object` is `TRUE`.
#' @examples
#' \dontrun{
#' bb <- bb_pane(xrange = c(0, 1), yrange = c(0, 1)) |>
#'   bb_point(0.5, 0.5)
#' bb_to_svg(bb)
#' }
bb_to_svg = function(bb, file=outfile, id = first.non.null(bb$id, random.string()), css=bb$css, width=first.non.null(bb$width,bb$org.width,480), height=first.non.null(bb$height,bb$org.height,320), return.svg.object = FALSE,latexsvg=isTRUE(bb$use.latex),outfile=NULL, ...) {
  restore.point("bb_to_svg")
  
  if (is.null(bb[["xaxis"]]))
    bb = bb %>% bb_xaxis()
  if (is.null(bb[["yaxis"]]))
    bb = bb %>% bb_yaxis()

  
  xrange = bb$xrange
  yrange = bb$yrange
  
  
  margins = compute_bb_margins(bb)
  
  
  svg = new_svg(id=id,width=width, height=height, xlim=bb$xrange, ylim=bb$yrange,css=css, margins=margins)


  bb$values$..x.min = min(xrange)
  bb$values$..x.max = max(xrange)
  bb$values$..y.min = min(yrange)
  bb$values$..y.max = max(yrange)
  
  
  dr = svg$dr
  

  if (bb$xaxis$show.ticks & is.null(bb$xaxis$ticks)) 
    bb$xaxis$ticks =pretty.ticks(dr$domain$x, n=bb$xaxis$num.ticks)
  if (bb$yaxis$show.ticks & is.null(bb$yaxis$ticks)) 
    bb$yaxis$ticks =pretty.ticks(dr$domain$y, n=bb$yaxis$num.ticks)

  bb$yaxis$custom.ticks = bb$custom.yticks
  bb$xaxis$custom.ticks = bb$custom.xticks

  
  bb = bb_compute_objs(bb)
  
  if (length(bb$objs)>0) {
    
    # draw objects in ascending level order
    levels = sapply(bb$objs, function(obj) first.non.null(obj$level,0))
    objs = bb$objs[rank(levels,ties.method = "first")]
    
    for (obj in objs) {
      draw.svg.obj(svg, obj,bb=bb)
    }  
    
  }

  
  
  for (obj in bb$labels) {
    obj = compute_bb_label(bb, obj)
    draw.svg.label(svg, obj, bb=bb)
  }

  if (isTRUE(bb$tooltip.bars))
    draw.series.tooltip.bars(bb=bb,svg=svg)
  
  do.call(svg_xaxis, c(list(svg=svg), bb$xaxis))
  do.call(svg_yaxis, c(list(svg=svg), bb$yaxis))
  
  
  if (return.svg.object) return(svg)
  
  ssvg=svg_string(svg)
  if (latexsvg)
    ssvg = latexsvg::latexsvg(ssvg)
  
  Encoding(ssvg) = "UTF-8"
  if (!is.null(file)) {
    writeLines(ssvg, file,useBytes = TRUE)
    return(invisible(ssvg))
  }
  ssvg
}

compute_bb_margins = function(bb) {
  restore.point("compute_bb_margins")
  if (!is.null(bb$margins)) (
    if (is.null(names(bb$margins))) {
      margins = rep(bb$margins, length.out=4)
      names(margins) = c("bottom","left","top","right")
      return(margins)
    }
  )
  
  margins = list(
    bottom=ifelse(isTRUE(bb$xaxis$show.ticks),60,50),
    left=ifelse(isTRUE(bb$yaxis$show.ticks),60,50),
    top=ifelse(isTRUE(bb$yaxis$labelpos=="top"),40,30),
    right=40
  )
  margins = copy.non.null.fields(dest=margins,source=bb$margins)
  unlist(margins)
}

draw.svg.obj = function(svg,obj,display=NULL,bb=NULL,...) {
  restore.point("draw.svg.obj")
  
  if (isTRUE(obj[["no.draw"]])) return(svg)
  
  if (obj$type=="curve") {
    draw.svg.curve(svg,obj, display=display, bb=bb)
  } else if (obj$type=="marker") {
    draw.svg.marker(svg,obj,  display=display, bb=bb)
  } else if (obj$type=="point") {
    draw.svg.point(svg,obj, display=display, bb=bb)
  } else {
    
    restore.point("draw.svg.type")
    fun = paste0("draw.svg.",obj$type)
    do.call(fun, list(svg=svg, obj, display=display,bb=bb))
  }
  svg
}

draw.svg.point = function(svg,obj, level=0, display=NULL,bb=NULL) {
  restore.point("draw.svg.point")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  range = domain.to.range(x=geom$x, y=geom$y, svg=svg)
  
  el = svg_tag("circle", c(nlist(cx=range$x,cy=range$y,r = geom$r, style=obj$style, class=obj$class, id=obj$id)))

  svg_add(svg, el, id=obj$id)
}


draw.svg.segment = function(svg,obj, level=0, display=NULL,bb=NULL) {
  restore.point("draw.svg.segment")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  r1 = domain.to.range(x=geom$x1, y=geom$y1, svg=svg)  
  r2 = domain.to.range(x=geom$x2, y=geom$y2, svg=svg)  
  
  el = svg_tag("line", c(nlist(x1=r1$x,x2=r2$x,y1=r1$y,y2=r2$y, style=obj$style, class=obj$class, "stroke-dasharray"=obj[["stroke-dasharray"]])),tooltip = geom$tooltip)

  svg_add(svg, el, id=obj$id)
}

draw.svg.arrow = function(svg,obj, level=-1, display=NULL,bb=NULL, arrow.id = paste0(svg$id,"_small_arrow_head")) {
  restore.point("draw.svg.arrow")
  #display = init.geom.display(geom, display)
  geom = obj$geom
  
  r1 = domain.to.range(x=geom$x1, y=geom$y1, svg=svg)  
  r2 = domain.to.range(x=geom$x2, y=geom$y2, svg=svg)  
  
  svg_def_small_arrow_head(svg)
  arrow.li = list("marker-end"=paste0("url(#",arrow.id,")"))  
  
  el = svg_tag("line", c(nlist(x1=r1$x,x2=r2$x,y1=r1$y,y2=r2$y, style=obj$style, class=obj$class), arrow.li),tooltip = geom$tooltip)

  svg_add(svg, el, id=obj$id)
}

svg_def_small_arrow_head =  function(svg,id=paste0(svg$id,"_small_arrow_head"), class="arrow_head") {
  svg_add_def(svg=svg,id=id,
    paste0('
  <marker id="',id,'" class="',class,'" markerWidth="10" markerHeight="10" refX="0" refY="3" orient="auto" markerUnits="userSpaceOnUse">
    <path d="M0,0 L0,6 L9,3 z" style ="fill: black;"/>
  </marker>
'
    )
  )
}


svg_def_arrow_head =  function(svg,id=paste0(svg$id,"_arrow_head"), class="arrow_head") {
  svg_add_def(svg=svg,id=id,
    paste0('
  <marker id="',id,'" class="',class,'" markerWidth="10" markerHeight="10" refX="0" refY="3" orient="auto" markerUnits="strokeWidth">
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
```
# END OF FILE: bb_svg.r

-----------------------------------------------------------


# FILE: bb_tools.r
```
isoquant.slope = function(U,x,y=NULL, as.character=is.character(U)) {
  restore.point("isoquant.slope")
  if (is.character(U)) {
    U = parse(text=U)
  }
  if (is.null(y)) {
    x = x[1]
    y = x[2]
  }
  dx = D(U,x)
  dy = D(U,y)
  
  dydx = substitute(- (dx) / (dy), list(dx=dx,dy=dy)) 
  
  if (as.character) (
    return(deparse1(dydx))
  )
  dydx
}

linetype.to.dasharry = function(linetype) {
  if (linetype=="dashed") return("4,4")
  if (linetype=="dotted") return("2,4")
  if (linetype=="dotdash") return("2,4,4,4")
  if (linetype=="longdash") return("8,4")
  if (linetype=="twodash") return("4,1,4,4")
  return(NULL)
}

round.to.grid = function(val, step=(end-start)/(length-1), start=range[1], end=range[2], length=101, range=c(0,NA)) {
  round( (val-start) / step)*step + start 
}


first.non.null = function(...) {
  args = list(...)
  for (arg in args) {
    if (!is.null(arg)) return(arg)
  }
  return(NULL)
}

copy.non.null.fields = function(dest=NULL, source, fields=names(source)) {
  restore.point("copy.non.null.fields")
  
  use.fields = intersect(names(source), fields)
  copy.fields = use.fields[!sapply(source[use.fields], is.null)]
  if (is.null(dest))
    return(source[copy.fields])
  
  if (is.environment(dest)) {
    for (field in copy.fields) dest[[field]] = source[[field]]
  } else {
    dest[copy.fields] = source[copy.fields]
  }

  invisible(dest)
}


is.false = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = TRUE  
  return(!val)
}

random.string = function(n=1,nchar=14, set=c(letters,LETTERS)) {
  chars = sample(set,nchar*n, replace = TRUE)
  if (n == 1) return(paste0(chars, collapse=""))
  mat = as.data.frame(matrix(chars, n, nchar))
  do.call(paste0,mat)
}

copy.into.null.fields = function(dest, source) {
  restore.point("copy.into.fields")
  
  snames = names(source)
  dest.val = dest[snames]
  dest.null = sapply(dest.val, is.null)
  
  dest[snames[dest.null]] = source[dest.null]
  dest
}



deparse1 = function (call, collapse = "") 
{
    paste0(deparse(call, width = 500), collapse = collapse)
}

```
# END OF FILE: bb_tools.r

-----------------------------------------------------------


# FILE: clicks.r
```

example.draw.clicks = function() {

yaml = '
pane:
  curves:
    demand:
      label: D
      eq: y == A - b *p
      color: red
    supply:
      label: S
      eq: p == mc
      color: blue
  xy: [y,p]
  xrange: [0,100]
  yrange: [0,100]
  xmarkers: [y_eq]
  ymarkers: [p_eq]
'
  pane = init.yaml.pane(yaml=yaml)
  values = list(A=100, b=1, mc=20,y_eq=30, p_eq=40)
  pane$geoms = compute.pane.geoms(pane, values=values)

  plot.pane(pane)
  
  clicks = list()
  
  while(length(clicks)<5) {
    click = locator(1)
    clicks[[length(clicks)+1]] = unlist(click)
    draw.clicks(clicks)
  }
}


draw.click = function(click,x=click[["x"]],y=click[["y"]],pch="+", color=grey(0.2), cex=1.2) {
  points(x,y,pch=pch,col=color,cex=cex)
}

draw.clicks = function(clicks,pch="+", color=grey(0.2), cex=1.2, add.line=TRUE) {
  restore.point("draw.clicks")
  
  if ((!is.data.frame(clicks)) & is.list(clicks)) {
    clicks = do.call("rbind",clicks)   
  }
  points(clicks[,1],clicks[,2],pch=pch,col=color,cex=cex)
  lines(clicks[,1],clicks[,2], col=color)
}

click.finds.geom.to.geom.pos = function(click, new, old, check=c("above","below","left","right"), need.all.dir = TRUE,...) {
  gg = geom.to.geom.pos(new, old, check=check)
  cg = point.to.geom.pos(click, old, check=check)
  
  cg = setdiff(cg,"on")
  
  if (need.all.dir) {
    ok = setequal(gg,cg)
  } else {
    ok = length(intersect(gg,cg))>0
  }
  ok
  
}

#' 
click.selects.single.geom = function(click, geoms, on.tol=0.05, single.tol=0.05) {
  restore.point("click.selects.single.geom")
  
  dists = click.dist.to.geoms(click, geoms)
  
  on = which(dists<=on.tol)
  if (length(on)==1) {
    return(list(ok=TRUE, selected=on))
  }
  if (length(on)==0) {
    return(list(ok=FALSE, selected=NULL))
  }
  
  close = which(dists<=single.tol)
  if (length(close)==1) {
    return(list(ok=TRUE, selected=close))
  }
  if (length(close)==0) {
    return(list(ok=FALSE, selected=on))
  }
  return(list(ok=FALSE, selected=close))
  
}

click.dist.to.geoms = function(click, geoms, ...) {
  sapply(geoms, function(geom) {
    point.to.geom.dist(click, geom,...)
  })
}

click.dist.to.geom = function(click, geom,...) {
  point.to.geom.dist(click, geom,...)
} 

is.click.on.geoms = function(click, geoms, on.tol=0.05) {
  sapply(geoms, function(geom) {
    is.point.on.geom(click, geom, on.tol=on.tol)
  })
}

is.click.on.geom = function(click, geom, on.tol=0.05) {
  is.point.on.geom(click, geom, on.tol=on.tol)
}

is.click.on.point = function(click, ref, axis="xy", on.tol=0.05,xrange=pane$xrange, yrange=pane$yrange, pane=NULL) {
  restore.point("has.click.found.point")
  dist = point.to.point.dist(click, ref, axis=axis, xrange=xrange, yrange=yrange, normalize=TRUE)
  
  return(dist<=tol)
}

```
# END OF FILE: clicks.r

-----------------------------------------------------------


# FILE: colors.r
```
hue.palette = function(n, h = 0, s.start = 1, s.end=0.35, v.start = 0.4, v.end=1, alpha=1) {
  if (n<=2) {
    s = c(1,  0.5)[1:n]
    v = c(0.7,1)[1:n]  
  } else if (n<=3) {
    s = c(1,0.5,0.9)[1:n]
    v = c(0.9,1,0.6)[1:n]
  } else {
    s = seq(s.start,s.end, length=n)
    v = seq(v.start,v.end, length=n)
  }
  hsv(h=rep(h,n),alpha=alpha,
      s=s,
      v=v)
}

reds = function(n, alpha=1, h=0) {
  if (n<=2) {
    s = c(1,  0.6)[1:n]
    v = c(0.9,1)[1:n]  
  } else if (n<=3) {
    s = c(1,0.5,0.9)[1:n]
    v = c(0.9,1,0.6)[1:n]
  } else {
    return(hue.palette(n, h=0, alpha=alpha))
  }
  hsv(h=rep(0,n),alpha=alpha,s=s, v=v)
}


blues = function(n, alpha=1, h=2/3) {
  if (n<=2) {
    s = c(1,  0.3)[1:n]
    v = c(0.7,1)[1:n]  
  } else if (n<=3) {
    s = c(1,0.5,0.9)[1:n]
    v = c(0.9,1,0.6)[1:n]
  } else {
    return(hue.palette(n, h=h, alpha=alpha))
  }
  hsv(h=rep(h,n),alpha=alpha,s=s, v=v)
}


examples.hue.palette = function() {
  n = 2
  reds = reds(n)
  oranges = hue.palette(n, h=1/16)
  yellows = hue.palette(n, h=1/8)
  greens = hue.palette(n, h=0.25)
  blues = blues(n)
  lilas = hue.palette(n, h=9/12)
  cyans = hue.palette(n, h=1/2)
  purples = hue.palette(n, h=11/12)
  
  show.colors(c(greens,cyans, blues, lilas, purples, reds, oranges, yellows),n)
}

color.pals = function() {
  list(
    black = c("#111111","#999999"),
    grey = c("#111111","#999999"),
    #blue = c("blue","#1F78B4", "#A6CEE3"),
    blue = c("blue","#6688ff", "#A6CEE3"),
    #green = c("green","#33A02C", "#B2DF8A"),
    green = c("green", "#00dd00","#77ff77", "#B2DF8A"),
    #red = c ("red","#E31A1C", "#FF9A99"),
    red = c("#ff0000","#ff7777", "#FF9A99"),
    orange = c("#FF7F00","#FDBF6F"),
    lila = c("#6A3D9A","#AA92B6"),
    yellow = c("yellow","#FFDF80"),
    brown = c("brown","#B3430F"),
    purple = c("#B30059","#FF80BF"),
    cyan = c("#00A3A3","#8FCCCC")
  )
}

curve.color = function(base="blue",level=0,color=NULL) {
  restore.point("curve.color")
  
  if (!is.null(color))
    return(color)
  
  if (is.null(base))
    return("black")
  library(RColorBrewer)
  
  pal = color.pals()  
  if (!base %in% names(pal)) base = "grey"
  
  li = pal[[base]]
  if (level > length(li)) return(grey(0.8))
  li[level]
} 

show.colors = function(colors, colCount=ceiling(sqrt(length(colors)))) {
  n = length(colors)
  rowCount = ceiling(n / colCount)

  plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
    axes=FALSE, ylim=c(rowCount,0))
  title("Colors")

  for (j in 0:(rowCount-1))
  {
    base <- j*colCount
    remaining <- length(colors) - base
    RowSize <- ifelse(remaining < colCount, remaining, colCount)
    rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
      border="black",
      col=colors[base + (1:RowSize)])
    text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
      col="black")
  }
}
```
# END OF FILE: colors.r

-----------------------------------------------------------


# FILE: ddbb.r
```

ddbb.examples = function() {
library(ddsim)
dd = ddsim() %>%
  dd_param(I=10,c0=0,c1=0.9) %>%
  dd_init_steady_state(Y) %>%
  dd_explicit(
    EY = lag_Y,
    C = c0 + c1*EY,
    Y = C + I
   ) %>%
  dd_expost(S = Y-C, S_PLAN=EY-C, "Geplante Sparquote" = (1-c1)*100, "Reale Sparquote" = 100*S / Y) %>%
  dd_shock(c1=0.8, start=3, length=Inf, name="Sparschock") %>%
  dd_run(T=20)
  sim = dd_data(dd)
select(sim[1:5,],"t","Y","C","I")
show = c("Y","C","I")

bb = dd_bbplot(dd,sim,show, rows=1:20,ylim=c(0,110),lwd=3, margins=c("right"=60)) %>% bb_xaxis(label="Periode",labelpos = "center") %>% bb_yaxis(ticks=c(0,25,50,75,100)) %>% bb_ymarker(y=50, label="") %>% bb_series_tooltip_bars(xname="Periode")
  

view.bb(bb)
}


dd_bbplot = function(dd, dat=dd_data(dd), cols=dd$var.names, main="",xlab=dd$time.var,ylab="", shocks=dd$shocks, show.shocks = TRUE, rows=1:NROW(dat),xlim=range(rows),ylim=NULL,colors=colors_bb_series(),show.ticks=TRUE,lwd=2,labels=cols,draw.points=TRUE, draw.line=TRUE, r=3, auto.labels = TRUE,  ...) {
  restore.point("dd_bbplot")
  #dat$t = t.to.date(dat$t)
  library(bbsvg)
  
  if (is.null(ylim)) {
    ylim=range(dat[,cols])
  }
  dat = dat[rows,,drop=FALSE]
  bb = bb_pane(data=dat,xrange=xlim,yrange=ylim,show.ticks = show.ticks, tooltip.bar = TRUE, ...)

  for (i in seq_along(cols)) {
    col = cols[i]
    bb = bb_series(bb,xvar = "t",yvar=col, name=col, color=colors[i],lwd=lwd, draw.points=draw.points, draw.line=draw.line, r=r)
    if (!is.null(labels) & auto.labels)
      bb = bb %>% bb_text(label=labels[i], x=max(xlim),y=dat[[col]][max(rows)], x.offset=10, color=colors[i])
  }
  if (show.shocks) {
    bb = dd_bb_annotate_shocks(bb, shocks, rows=rows, dd=dd, T=max(xlim))
  }
  bb
}
dd_bb_annotate_shocks = function(bb, shocks=dd[["shocks"]],dd=NULL, T = first.non.null(dd$T,NROW(bb$data[[1]])), rows = 1:T) {
  restore.point("dd_bb_annotate_shocks")
  #shock = shocks[[1]]
  for (shock in shocks) {
    if (shock$start > max(rows)) next
    #start = t.to.date(shock$start)
    name = shock$name
    
    bb = bb %>%
      bb_period(from=shock$start,to=min(shock$end,T),label = name,alpha = 0.15, font_size=12)
  }
  bb
} 
```
# END OF FILE: ddbb.r

-----------------------------------------------------------


# FILE: display.r
```

init.geom.display = function(geom, display) {
  if (identical(display,"whisker")) {
    return(paste0("{{display_",geom$id,"}}"))
  }
  display
}
```
# END OF FILE: display.r

-----------------------------------------------------------


# FILE: htmlplot.r
```

examples.plot.to.html = function() {
  setwd("D:/libraries/EconCurves/")
  addResourcePath("image",getwd())

  filename="test.png"
  res = plot.to.html({
    plot(1:10,(1:10)^2)
    abline(h=5)
    abline(v=5)
    }, format="png", src.path="image", filename = filename, embed=FALSE, compute.coordmap = TRUE, img.id="myimg", img.style="cursor: pointer;")
  html = res$html
  coordmap = res$coordmap
  
  app = eventsApp()
  addResourcePath("fig", getwd())
  app$ui = fluidPage(
    p("Image"),
    HTML(html)
  )
  imageClickHandler(id="myimg", function(...,app=getApp()) {
    args = list(...)
    pixelratio = get.pixelratio()
    restore.point("my.image.handler")
    cat("\nclicked on image pixelratio = ", pixelratio,"\n")
    x = args$x
    y = args$y
    
    cat(paste0(c(x,y)," -> ",scaleInvCoords(x,y,coordmap), collapse="\n"))
  })
  viewApp()

}  
  
plot.to.html = function(expr,envir=parent.frame(), quoted=NULL, width.px=width.in*res, height.px=height.in*res, res=144, width.in=5, height.in=4, pointsize=10, bg="white", format=c("png","svg")[1],out.dir=getwd(),src.path=".", filename=NULL, embed=FALSE, compute.coordmap=FALSE, img.id=NULL, img.style=NULL, img.class=NULL) {
  restore.point("plot.to.html")
  
  library(rmdtools)
 
  if (is.null(quoted)) {
    quoted = substitute(expr)
  }
  
  if (!embed & is.null(filename)) {
    filename = paste0("zzz",random.string(n = 1,nchar=10,set=letters),".",format)
  }
  if (!is.null(img.id)) img.id = paste0(' id = "',img.id,'"')
  if (!is.null(img.class)) img.class = paste0(' class = "',img.class,'"')
  #if (!is.null(img.style)) img.style = paste0(' style = "',img.style,'"')
  
  if (format == "svg") {
    library(svglite)

    if (embed) {
      s <- svgstring(bg=bg, pointsize=pointsize, width=width.in, height=height.in)
      html = s()
      dev.off()
    } else {
      svglite(file = paste0(out.dir,"/",filename),bg=bg, pointsize=pointsize, width=width.in, height=height.in)
    }
    eval(quoted, envir)
    if (compute.coordmap) {
      coordmap = shiny:::getPrevPlotCoordmap(width=width.px, height=height.px)[[1]]
    }
    if (embed) {
      html = s()
      html = paste0('<div style="width: ',width.in,'in; height: ',height.in,'in;">\n',html,'\n</div>')

    } else {
      html = paste0('<img src="',src.path,'/',filename,'" style="width: ',width.in,'in; height: ',height.in,'in;', img.style,';"',img.id,img.class,'>')
    }
    dev.off()
 
  } else if (format == "png") {
    if (embed) {
      restore.point("html.embed.png")
      out.dir = tempdir()
      filename= basename(tempfile(fileext = ".png",tmpdir = out.dir))
      ret = plot.png.with.coordmap(quoted=quoted,width.px = width.px,height.px = height.px, res=res,envir = envir,dir = out.dir,filename = filename)
      library(base64enc)
      enc = base64encode(paste0(out.dir,"/",filename))
      html = paste0('<img src="data:image/png;base64,',enc,'" style="width: ',width.in,'in; height: ',height.in,'in;', img.style,'"', img.class, img.id,'>')
      #html = paste0('<img src="',src.path,'/',filename,'"',img.id,img.class, img.style,'>')
      #html = paste0('<div style="width: ',width.in,'in; height: ',height.in,'in;">\n',html,'\n</div>')
    } else {
      restore.point("html.external.png")
      ret = plot.png.with.coordmap(quoted=quoted,width.px = width.px,height.px = height.px, res=res,envir = envir,dir = out.dir,filename = filename)
      html = paste0('<img src="',src.path,'/',filename,'" style="width: ',width.in,'in; height: ',height.in,'in;', img.style,'"', img.class, img.id,'>')
      #html = paste0('<img src="',src.path,'/',filename,'"',img.id,img.class, img.style,'>')
      #html = paste0('<div style="width: ',width.in,'in; height: ',height.in,'in;">\n',html,'\n</div>')
    }
    coordmap = ret$coordmap
    
  }
  
  ret = list(html=html, filename=filename, out.dir=out.dir)
  if (compute.coordmap) {
    ret$coordmap = coordmap
  }
  return(ret)
}


get.pixelratio = function(session = app$session, app=getApp()) {
  if (is.null(session)) return(1)
  session$clientData$pixelratio %OR% 1  
}

examples.plot.png.with.coordmap = function() {
  setwd("D:/libraries/EconCurves/")
  filename="test.png"
  res = plot.png.with.coordmap(plot(1:10), width.px = 400, height.px=300, dir=getwd(), filename=filename)
  coordmap=res$coordmap[[1]]
  library(shinyEvents)
  app = eventsApp()
  addResourcePath("fig", getwd())
  app$ui = fluidPage(
    p("Image"),
    tags$img(src = paste0("fig/",filename),id="myimg", style="cursor: crosshair;")
  )
  imageClickHandler(id="myimg", function(...,app=getApp()) {
    args = list(...)
    pixelratio = get.pixelratio()
    restore.point("my.image.handler")
    cat("\nclicked on image pxielratio = ", pixelratio,"\n")
    x = args$x
    y = args$y
    
    cat(paste0(c(x,y)," -> ",scaleInvCoords(x,y,coordmap), collapse="\n"))
  })
  viewApp()
}

plot.png.with.coordmap = function(expr,width.px=width.in*res, height.px=height.in*res, res=144, width.in=4, height.in=3, envir=parent.frame(), quoted=NULL, filename = tempfile(tmpdir = dir,fileext = ".png"), dir=tempdir(), pixelratio=1,...) {
  
  restore.point("plot.png.with.coordmap")
  
  if (is.null(quoted)) quoted = substitute(expr)  

  if (capabilities("aqua")) {
      pngfun <- grDevices::png
  }
  else if ((getOption("shiny.usecairo") %OR% TRUE) && nchar(system.file(package = "Cairo"))) {
      pngfun <- Cairo::CairoPNG
  }
  else {
      pngfun <- grDevices::png
  }
  filename = paste0(dir,"/",basename(filename))
  pngfun(filename = filename, width = width.px, height = height.px, 
      res = res, ...)
  #op <- graphics::par(mar = rep(0, 4))
  #tryCatch(graphics::plot.new(), finally = graphics::par(op))
  #op <- graphics::par(mar = rep(0, 4))
  tryCatch(graphics::plot.new())
  dv <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dv), add = TRUE)
  
  eval(quoted, envir)
  coordmap = shiny:::getPrevPlotCoordmap(width=width.px, height=height.px)[[1]]
 
  list(filename=filename, dir=dir, coordmap=coordmap)

}

`%OR%` = function (x, y) 
{
    if (is.null(x) || isTRUE(is.na(x))) 
        y
    else x
}

# Copied from shiny source code
# Scale x and y coordinates from domain to range, using information in
# scaleinfo. scaleinfo must contain items $domain, $range, and $log. The
# scaleinfo object corresponds to one element from the coordmap object generated
# by getPrevPlotCoordmap or getGgplotCoordmap; it is the scaling information for
# one panel in a plot.
scaleCoords <- function(x, y, scaleinfo) {
  if (is.null(scaleinfo))
    return(NULL)

  domain <- scaleinfo$domain
  range <- scaleinfo$range
  log <- scaleinfo$log

  list(
    x = shiny:::scale1D(x, domain$left, domain$right, range$left, range$right, log$x),
    y = shiny:::scale1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
  )
}

# Copied from shiny source code
# Inverse scale x and y coordinates from range to domain, using information in
# scaleinfo.
scaleInvCoords <- function(x, y, scaleinfo) {
  if (is.null(scaleinfo))
    return(NULL)

  domain <- scaleinfo$domain
  range <- scaleinfo$range
  log <- scaleinfo$log

  list(
    x = shiny:::scaleInv1D(x, domain$left, domain$right, range$left, range$right, log$x),
    y = shiny:::scaleInv1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
  )
}
```
# END OF FILE: htmlplot.r

-----------------------------------------------------------


# FILE: label_pos.r
```

get.endpoints = function(geoms) {
  restore.point("get.endpoints")
  #geom = geoms[[2]]
  li = lapply(seq_along(geoms), function(ind) {
    restore.point("uhsfanjadnfjn")
    geom = geoms[[ind]]
    n = min(length(geom$x), length(geom$y))
    if (n==0) return(NULL)
    if (n==1) {
      res = quick.df(x=geom$x[1],y=geom$y[1],ind=ind)
    } else {
      res = quick.df(x=geom$x[c(1,n)],y=geom$y[c(1,n)],ind=rep(ind,2))
    }
    res
  })   
  as_data_frame(bind_rows(li))
}

find.label.pos = function(geoms,xrange, yrange, yshift=diff(yrange)*0.05, do.shuffle=FALSE) {
  restore.point("find.label.pos")  

  inds = seq_along(geoms)


  
  ep.df = get.endpoints(geoms)

  labx = sapply(geoms, function(geom) geom$labx)
  laby = sapply(geoms, function(geom) geom$laby)
  
  
  ind = which(!is.na(labx) & !is.na(laby))
  lab.mat = data_frame(x=labx[ind],y=laby[ind],ind=ind)
  
  ep.df = rbind(ep.df[!(ep.df$ind %in% ind),],lab.mat) %>% arrange(ind)
  
  
  
  ep.df$remain = TRUE
  ep.df = mutate(ep.df,
    right = x == xrange[2],
    left = x == xrange[1],
    top = y == yrange[1],
    bottom = y == yrange[2],
    outer = right | left | top | bottom
  )
  ep.df = mutate(group_by(ep.df, ind),
    del = any(outer) & !outer
  )
  ep.df = ep.df[!ep.df$del,]

  # For a single geom pick the last point
  if (length(inds)==1) {
    ep.df$remain = FALSE
    ep.df$remain[NROW(ep.df)] = TRUE
  
  # For multiple geoms try to find endpoint that is farthest away
  # from other endpoints
  } else {
    if (do.shuffle) {
      shuffle = sample.int(length(inds))
    } else {
      shuffle = inds
    }
    i = 1

    # greedy search: find end points that are closest
    for (i in shuffle) {
      ind =inds[i]
      rows = which(ep.df$ind == ind)
      ep.df$remain[rows] = FALSE
      dist = sapply(rows,ep.df=ep.df, function(row, ep.df) {
        x = ep.df$x[row]; y = ep.df$y[row]
        dist = min( (ep.df$x[ep.df$remain]-x)^2 + (ep.df$y[ep.df$remain]-y)^2)
        dist
      })
      sel.row = rows[which.max(dist)]
      ep.df$remain[sel.row] = TRUE
    }
  }  
  label.pos = ep.df[ep.df$remain, 1:4]
  dupl = which(duplicated(label.pos[,c("x","y")]))
  sign = -((-1)^(seq_along(dupl)))
  label.pos$y[dupl] = label.pos$y[dupl]+sign*yshift 
  label.pos
}
```
# END OF FILE: label_pos.r

-----------------------------------------------------------


# FILE: latex.r
```
svg.mathjax.label = function(str, align=c("","L","R","C")[1]) {
  return(paste0(align,"\\(",str,"\\)"))
}

latex.to.textspan = function(str) {
  #str = "x_{5ab} y_{1} z_3"
  restore.point("latex.to.textspan")

  #str = "x_{5ab}\\alpha * \\beta"

  #str = "E^{*}"
  str = replace.latex.with.unicode(str)  

  txt = str
  txt = svg.change.subscripts(txt,super = FALSE)
  txt = svg.change.subscripts(txt,super = TRUE)
  
  if (!identical(txt,str)) {
    txt = paste0("<tspan>",txt,"</tspan>")
  }
  
  # remove curley braces
  txt = gsub("{{","jJj",txt, fixed=TRUE)
  txt = gsub("}}","hHh",txt, fixed=TRUE)
  txt = gsub("{","",txt, fixed=TRUE)
  # Without the zero space &#8203; sub- and superscripts wont work
  txt = gsub("}","&#8203;",txt, fixed=TRUE)
  txt = gsub("  "," ",txt, fixed=TRUE)
  txt = gsub("jJj","{{",txt, fixed=TRUE)
  txt = gsub("hHh","}}",txt, fixed=TRUE)

  txt
}

svg.change.subscripts = function(str, add.tspan = FALSE, super=FALSE) {
  restore.point("svg.change.subscripts")
  
  if (!super) {
    char = "_"
    class = "label_subscript"
    sign = 1
  } else {
    char = "\\^"
    class = "label_superscript"
    sign = -0.75
  }
  
  li = find.subscripts(str,char=char)$s
  if (length(li)==1) {
    txt = li
  } else {
    #if (length(li) %% 2 ==1) li = c(li,"")
    sub = seq(2, length(li),by=2)
    dy = sapply(seq_along(sub),function(i) {
      restore.point("hdfhkjdh")
      nc = nchar(li[sub[i]])
      if (substring(li[sub[i]],1,1)=="{") nc = nc-2
      paste0(c(5,rep(0,nc-1),-5)*sign , collapse=",")
    })
    li[sub] = paste0('<tspan dy="',dy,'" class="', class,'">', li[sub],'</tspan>')
    
    txt = paste0(li,collapse="")
    if (add.tspan) {
      txt = paste0("<tspan>",txt,"</tspan>")
    }
  }
  txt
} 

find.subscripts = function(str, char = "_") {
  restore.point("find.subscripts")
  

  # find subscripts
  pos1 = str.find(str,paste0(char,'[0-9a-zA-Z|.=]+'),fixed=FALSE)
  pos2 = str.find(str,paste0(char,'\\{[0-9a-zA-Z_|.=,*+-°]+\\}'),fixed=FALSE)
  pos = rbind(pos1,pos2)
  if (NROW(pos)==0) {
    return(list(s=str,is.sub=FALSE))
  }
  
  spl = str.split.at.pos(str,pos,keep.pos = TRUE)  
  first = pos[1,1]==1
  if (first) {
    is.sub = rep(c(TRUE,FALSE),length.out=length(spl))
  } else {
    is.sub = rep(c(FALSE,TRUE),length.out=length(spl))
  }
  spl[is.sub] = substring(spl[is.sub],2)

  
    
  list(s=spl, is.sub=is.sub)

}

replace.latex.with.unicode = function(str) {

  latex = c( "\\alpha","\\beta","\\gamma","\\delta","\\epsilon","\\zeta","\\eta","\\theta","\\iota","\\kappa","\\lambda","\\mu","\\nu","\\xi","\\pi","\\rho","\\varsigma","\\sigma","\\tau","\\upsilon","\\phi","\\chi","\\psi","\\omega","\\Gamma","\\Delta","\\Theta","\\Lambda","\\Xi","\\Pi","\\Sigma","\\Upsilon","\\Phi","\\Psi","\\Omega","\\neg","\\pm","\\cdot","\\to","\\Rightarrow","\\Leftrightarrow","\\forall","\\partial","\\exists","\\emptyset","\\nabla","\\in","\\notin","\\prod","\\sum","\\surd","\\infty","\\wedge","\\vee","\\cap","\\cup","\\int","\\approx","\\neq","\\equiv","\\leq","\\geq","\\subset","\\supset","\\^circ","\\times","\\lfloor","\\rfloor","\\lceil","\\rceil" ) 
  
uc = c( "\U3B1","\U3B2","\U3B3","\U3B4","\U3B5","\U3B6","\U3B7","\U3B8","\U3B9","\U3BA","\U3BB","\U3BC","\U3BD","\U3BE","\U3C0","\U3C1","\U3C2","\U3C3","\U3C4","\U3C5","\U3C6","\U3C7","\U3C8","\U3C9","\U393","\U394","\U398","\U39B","\U39E","\U3A0","\U3A3","\U3A5","\U3A6","\U3A8","\U3A9","\U00AC","\U00B1","\U00B7","\U2192","\U21D2","\U21D4","\U2200","\U2202","\U2203","\U2205","\U2207","\U2208","\U2209","\U220F","\U2211","\U221A","\U221E","\U2227","\U2228","\U2229","\U222A","\U222B","\U2248","\U2260","\U2261","\U2264","\U2265","\U2282","\U2283","\U00B0","\U00D7","\U230A","\U230B","\U2308","\U2309" )
  
  pos = str.find(str,'\\\\[0-9a-zA-Z]+',fixed=FALSE)
  spl = str.split.at.pos(str,pos,keep.pos = TRUE)  
  ind = match(spl, latex)
  rows = !is.na(ind)
  spl[rows] = uc[ind[rows]]
  
  res = paste0(spl,collapse="")
  Encoding(res) = "UTF-8"
  res
}

make.greece.code = function() {  
  str='
  α,alpha,&alpha;,x3B1
  β,beta,&beta;,x3B2
  γ,gamma,&gamma;,x3B3
  δ,delta,&delta;,x3B4
  ε,epsilon,&epsilon;,x3B5
  ζ,zeta,&zeta;,x3B6
  η,eta,&eta;,x3B7
  θ,theta,&theta;,x3B8
  ι,iota,&iota;,x3B9
  κ,kappa,&kappa;,x3BA
  λ,lambda,&lambda;,x3BB
  μ,mu,&mu;,x3BC
  ν,nu,&nu;,x3BD
  ξ,xi,&xi;,x3BE
  π,pi,&pi;,x3C0
  ρ,rho,&rho;,x3C1
  ς,varsigma,&sigmaf;,x3C2
  σ,sigma,&sigma;,x3C3
  τ,tau,&tau;,x3C4
  υ,upsilon,&upsilon;,x3C5
  φ,phi,&phi;,x3C6
  χ,chi,&chi;,x3C7
  ψ,psi,&psi;,x3C8
  ω,omega,&omega;,x3C9
  Γ,Gamma,&Gamma;,x393
  Δ,Delta,&Delta;,x394
  Θ,Theta,&Theta;,x398
  Λ,Lambda,&Lambda;,x39B
  Ξ,Xi,&Xi;,x39E
  Π,Pi,&Pi;,x3A0
  Σ,Sigma,&Sigma;,x3A3
  Υ,Upsilon,&Upsilon;,x3A5
  Φ,Phi,&Phi;,x3A6
  Ψ,Psi,&Psi;,x3A8
  Ω,Omega,&Omega;,x3A9
¬,neg,&not;,x00AC
±,pm,&plusmn;,x00B1
·,cdot,&middot;,x00B7
→,to,&rarr;,x2192
⇒,Rightarrow,&rArr;,x21D2
⇔,Leftrightarrow,&hArr;,x21D4
∀,forall,&forall;,x2200
∂,partial,&part;,x2202
∃,exists,&exist;,x2203
∅,emptyset,&empty;,x2205
∇,nabla,&nabla;,x2207
∈,in,&isin;,x2208
∉,notin,&notin;,x2209
∏,prod,&prod;,x220F
∑,sum,&sum;,x2211
∩,cap,&cap;,x2229
∪,cup,&cup;,x222A
∫,int,&int;,x222B
≈,approx,&asymp;,x2248
≠,neq,&ne;,x2260
≡,equiv,&equiv;,x2261
≤,leq,&le;,x2264
≥,geq,&ge;,x2265
⊂,subset,&sub;,x2282
⊃,supset,&sup;,x2283
°,^circ,&deg;,x00B0
×,times,&times;,x00D7
⌊,lfloor,&lfloor;,x230A
⌋,rfloor,&rfloor;,x230B
⌈,lceil,&lceil;,x2308
⌉,rceil,&rceil;,x2309'
  d = read.csv(textConnection(str),header = FALSE,stringsAsFactors = FALSE)
  
  cat("latex = c(",paste0('"\\\\',d[,2],'"',collapse=","),")")
  uc = d[,4]
  cat("uc = c(",paste0('"\\U',substring(uc,2),'"',collapse=","),")")
}
```
# END OF FILE: latex.r

-----------------------------------------------------------


# FILE: lines_utils.r
```
#' intersection of two geoms that are characterized by two points each
#' 
#' Formula based on Wikipedia entry
#' http://en.wikipedia.org/wiki/Line%E2%80%93geom_intersection
two.point.lines.intersections = function(x1,x2,x3,x4,y1,y2,y3,y4) {
  xi.num = (x1*y2-y1*x2)*(x3-x4) - (x1-x2)*(x3*y4-y3*x4)
  yi.num  = (x1*y2-y1*x2)*(y3-y4) -(y1-y2)*(x3*y4-y3*x4)
  den = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  
  list(x=xi.num / den, y=yi.num / den)
}

#' Find the intersections of two curves, which are characterized by their xy values
geom.curves.intersections = function(geom1, geom2, grid.length=201) {

  xmin = max(min(geom1$x),min(geom2$x))
  xmax = min(max(geom1$x),max(geom2$x))

  xout = seq(xmin,xmax, length=grid.length)
  ay1 = approx(geom1$x,geom1$y, xout, method="geomar")$y
  ay2 = approx(geom2$x,geom2$y, xout, method="geomar")$y
  
  dy = ay1 - ay2
  sign.change = which(diff(sign(dy))!=0)
  
  
  #xi = (xout[sign.change]+xout[sign.change+1]) / 2
  #yi = (ay1[sign.change]+ay2[sign.change])/2
  
  if (length(sign.change)==0)
    return(list(x=numeric(0),y=numeric(0)))
  
  int = two.point.lines.intersections(
    x1 = xout[sign.change],
    x2 = xout[sign.change+1],
    y1 = ay1[sign.change],
    y2 = ay1[sign.change+1],
    x3 = xout[sign.change],
    x4 = xout[sign.change+1],
    y3 = ay2[sign.change],
    y4 = ay2[sign.change+1]
  )
  
  return(int)
}
```
# END OF FILE: lines_utils.r

-----------------------------------------------------------


# FILE: positions.r
```
# when is a geom new below a geom old
# i) no common x range
#    max(new$y) < max(old$y) & min(new$y) < min(old$y)
# ii) a common x range
#    for all x in the common x range new$y(x) < old$y(x)
#    and the max, min conditions hold weakly 
# 

examples.geom.relations = function() {
  yaml = '
  pane:
    curves:
      demand:
        label: D{{idD}}
        eq: y == A - b *p
        color: red
      supply:
        label: S{{idS}}
        eq: p == mc
        color: blue
    xy: [y,p]
    xrange: [0,100]
    yrange: [0,150]
    xmarkers: [y_eq]
    ymarkers: [p_eq]
  '
  pane = init.yaml.pane(yaml=yaml)
  values1 = list(A=100, b=1, mc=20,y_eq=30, p_eq=40, idD=1,idS="")
  geoms1 = compute.pane.geoms(pane, values=values1, name.postfix="1")

  values2 = list(A=130, b=1, mc=20,y_eq=30, p_eq=40, idD=2,idS="")
  geoms2 = compute.pane.geoms(pane, values=values2, name.postfix="2", color.level = 2)

  pane$geoms = c(geoms1, geoms2["demand2"])
  #  pane$geoms = compute.pane.geoms(pane, values=values, name.postfix="2")

  plot.pane(pane)

  
    
  click = locator(n=1,type="p")
  click.selects.single.geom(click, pane$geoms)
  
  geom1 = geoms1[[1]]
  geom2 = geoms2[[1]]

  
  pane$geoms = geoms1
  plot.pane(pane)
  click = locator(n=1,type="p")  
  click.finds.geom.to.geom.pos(click, geom2, geom1)
  
  
  is.geom.right(geom2, geom1)
  is.geom.below(geom2, geom1)
  is.geom.above(geom2, geom1)

  
  geom.to.geom.pos(geom1, geom2)

  geom.to.geom.pos(geom2, geom1)

  point.to.geom.pos(c(90,30), geom2)
  point.to.geom.pos(c(90,30), geom1)
  
  point.to.geom.pos(c(90,30), geoms1[[3]])
  point.to.geom.pos(c(30,30), geoms1[[3]])

  geom.to.geom.pos(geom1, geom2)
  geom.to.geom.pos(geom1, geoms1[[3]])

}

#' Find relative position (above, below, left, right) of a point to a geom
point.to.geom.pos = function(xy, geom,check=c("above","below","left","right","on"), tol.on=0.01) {
  restore.point("point.to.geom.pos")
  
  found = NULL
  
  for (pos in check) {
    fun.name = paste0("is.point.",pos,".geom")
    call = substitute(fun(xy=xy,geom=geom, tol.on=tol.on), list(fun=as.name(fun.name))) 
    if (eval(call)) found = c(found, pos) 
  }
  found
  
  
}

#' Find relative position (above, below, left, right) of a geom to a geom
#' 
#' If the geoms intersect non of the attributes holds true, i.e.
#' the geom new must be stricly above old
geom.to.geom.pos = function(new, old, check=c("above","below","left","right")) {
  found = NULL
  for (pos in check) {
    fun.name = paste0("is.geom.",pos)
    call = substitute(fun(new,old), list(fun=as.name(fun.name))) 
    if (eval(call)) found = c(found, pos) 
  }
  found
}

is.point.below.geom = function(xy, geom,...) {
  restore.point("is.point.below.geom")
  x = xy[[1]]; y = xy[[2]]
  
  x = round.to.grid(x,range=geom$xrange, length = geom$xlen)
  geom = add.geom.grids(geom,dim="x")   

  ind = which(geom$xgr.min$x==x)
  
  isTRUE(y < geom$xgr.min$y[ind])
}

is.point.above.geom = function(xy, geom,...) {
  restore.point("is.point.above.geom")
  x = xy[[1]]; y = xy[[2]]

  x = round.to.grid(x,range=geom$xrange, length = geom$xlen)
  geom = add.geom.grids(geom,dim="x")   

  ind = which(geom$xgr.max$x==x)
  
  isTRUE(y > geom$xgr.max$y[ind])
}

is.point.left.geom = function(xy, geom,...) {
  restore.point("is.point.above.geom")
  x = xy[[1]]; y = xy[[2]]

  y = round.to.grid(y,range=geom$yrange, length = geom$ylen)
  geom = add.geom.grids(geom,dim="y")   

  ind = which(geom$ygr.min$y==y)
  
  isTRUE(x < geom$ygr.min$x[ind])
}


is.point.right.geom = function(xy, geom,...) {
  restore.point("is.point.above.geom")
  x = xy[[1]]; y = xy[[2]]

  y = round.to.grid(y,range=geom$yrange, length = geom$ylen)
  geom = add.geom.grids(geom,dim="y")   

  ind = which(geom$ygr.max$y==y)
  
  isTRUE(x > geom$ygr.max$x[ind])
}

point.to.point.dist = function(xy,ref, axis="xy",normalize=TRUE, xrange=pane$xrange, yrange=pane$yrange,pane=NULL, ...) {
  restore.point("point.dist.to.geom")
  x = xy[[1]]; y = xy[[2]]
  rx= ref[[1]]; ry = ref[[2]]
  
  if (normalize) {
    xs = diff(pane$xrange)
    ys = diff(pane$yrange)
  } else {
    xs = ys = 1
  }

  if (axis=="x") {
    dist = abs(x-rx) / xs
  } else if (axis=="y") {
    dist = abs(y-ry) / ys
  } else if (axis=="xy") {
    dist = sqrt(((x-rx)/xs)^2+ ((y-ry)/ys)^2)
  }
  dist
}


point.to.geom.dist = function(xy, geom, axis="xy",normalize=TRUE, xrange=geom$xrange, yrange=geom$yrange, ...) {
  restore.point("point.dist.to.geom")
  x = xy[[1]]; y = xy[[2]]

  if (normalize) {
    xs = diff(xrange)
    ys = diff(yrange)
  } else {
    xs = ys = 1
  }

  geom = add.geom.grids(geom,dim=c("x", "y"))   
  gx=c(geom$x, geom$xgr$x, geom$ygr$x)
  gy=c(geom$y, geom$xgr$y, geom$ygr$y)
      
  if (axis=="x") {
    dist = min(abs(x-gx) / xs, na.rm = TRUE)
  } else if (axis=="y") {
    dist = min(abs(y-gy) / ys, na.rm = TRUE)
  } else if (axis=="xy") {
    dist = min(sqrt(((x-gx)/xs)^2+ ((y-gy)/ys)^2), na.rm=TRUE)
  }
  dist
}


is.point.on.geom = function(xy, geom, on.tol=0.03,...) {
  restore.point("is.point.on.geom")
  dist = point.to.geom.dist(xy,geom, normalize=TRUE)
  if (length(dist)==0) return(FALSE)
  dist <= on.tol
}


is.geom.below = function(new, old) {
  restore.point("is.geom.below")

  nyr = range(new$y)
  oyr = range(old$y)

  # if min or max is above, so new is not below old  
  if (any(nyr>oyr)) return(FALSE)

  new = add.geom.grids(new,dim="x")
  old = add.geom.grids(old,dim="x")

  below = new$xgr.max$y < old$xgr.min$y
  any(is.true(below)) & !any(is.false(below))
}

is.geom.above = function(new, old) {
  restore.point("is.geom.above")

  nyr = range(new$y)
  oyr = range(old$y)

  if (any(nyr<oyr)) return(FALSE)

  new = add.geom.grids(new,dim="x")
  old = add.geom.grids(old,dim="x")

  above = new$xgr.min$y > old$xgr.max$y
  any(is.true(above)) & !any(is.false(above))
}

is.geom.left = function(new, old) {
  restore.point("is.geom.left")

  nxr = range(new$x)
  oxr = range(old$x)

  if (any(nxr>oxr)) return(FALSE)

  new = add.geom.grids(new,dim="y")
  old = add.geom.grids(old,dim="y")

  left = new$ygr.max$x < old$ygr.min$x
  any(is.true(left)) & !any(is.false(left))
}

is.geom.right = function(new, old) {
  restore.point("is.geom.right")

  nxr = range(new$x)
  oxr = range(old$x)

  if (any(nxr>oxr)) return(FALSE)

  new = add.geom.grids(new,dim="y")
  old = add.geom.grids(old,dim="y")

  right = new$ygr.min$x > old$ygr.max$x
  any(is.true(right)) & !any(is.false(right))
}


round.to.grid = function(val, step=(end-start)/(length-1), start=range[1], end=range[2], length=101, range=c(0,NA)) {
  round( (val-start) / step)*step + start 
}


get.geom.segments = function(geom, dim="x") {
  restore.point("get.geom.segments")
  
  cdim = if (dim=="x") "y" else "x"
  sig = sign(diff(geom[[dim]]))
  
  swing = which(diff(sig)!=0)
  
  # All x bewegen sich in gleiche Richtung
  if (length(swing)==0) {
    return(list(list(x=geom$x,y=geom$y)))
  }
  inds = c(1,swing+1, length(geom[[dim]]))
  res = lapply(1:(length(inds)-1), function(i) {
    rows = inds[i]:inds[i+1]
    list(x=geom$x[rows],y=geom$y[rows])
  })
  res 
}

geom.max.grid = function(geom, grid=NULL, dim="x", dir="max") {
  geom.min.grid(geom,grid,dim,dir)
}

geom.min.grid = function(geom, grid=NULL, dim="x", dir="min") {
  restore.point("geom.min.grid")
  
  if (dim=="x") {
    if (is.null(grid))
      grid = geom$xgr
    odim = "y"
    sign = if (dir=="min") 1 else -1
    
    xseq = seq(geom$xrange[1], geom$xrange[2], length=geom$xlen)

    ord = order(grid$x, sign*grid$y)
    x = c(grid$x[ord],xseq)
    y = c(grid$y[ord],rep(NA,length(xseq)))
    
    dupl = duplicated(x)
    nx = x[!dupl]
    ny = y[!dupl]
    nord = order(nx)
    return(list(x=nx[nord],y=ny[nord]))
  }
  
  if (dim=="y") {
    if (is.null(grid))
      grid = geom$ygr
    odim = "x"
    sign = if (dir=="min") 1 else -1
    
    yseq = seq(geom$yrange[1], geom$yrange[2], length=geom$ylen)

    ord = order(grid$y, sign*grid$x)
    y = c(grid$y[ord],yseq)
    x = c(grid$x[ord],rep(NA,length(yseq)))
    
    dupl = duplicated(y)
    nx = x[!dupl]
    ny = y[!dupl]
    nord = order(ny)
    return(list(x=nx[nord],y=ny[nord]))
    
  }
}

compute.geom.grid = function(geom, dim="x", use.object=TRUE) {
  restore.point("compute.geom.grid")
  
  if (!is.null(geom[["obj"]]) & use.object) {
    if (geom$obj$type=="curve") {
      return(compute.curve.grid(cu=geom$obj,geom=geom,dim=dim))
    }
  }
  if (dim=="x") {
    if (length(unique(geom$x))==1) {
      restore.point("nfbdhfbhrbdufbur")
      
      return(list(
        x=round.to.grid(geom$x,length=geom$xlen, range=geom$xrange),
        y=geom$y
      ))
    }
    
    segs = get.geom.segments(geom=geom, dim=dim)
    xseq = seq(geom$xrange[1], geom$xrange[2], length=geom$xlen)
    
    if (length(segs)==1) {
      # nice one-to-one function
      yseq = approx(x = geom$x,y=geom$y,xout=xseq)$y
      return(list(x=xseq,y=yseq))
    } else {
      
      # deal with backward bending curve
      yseqs = unlist(lapply(segs, function(seg) {
        approx(x=seg$x,y=seg$y, xout=xseq)$y
      }))
      keep = !is.na(yseqs) 
      xseqs = rep(xseq,times=NROW(segs))[keep]
      yseqs = yseqs[keep]
      ord = order(xseqs,yseqs)
      
      return(list(x=xseqs[ord],y=yseqs[ord]))
        
    }
  }
  if (dim=="y") {
    restore.point("compute.grid.y")
    if (length(unique(geom$y))==1) {
     restore.point("nfbdhfbhrbdufefef3bur")
      
      return(list(
        x=geom$x,
        y=round.to.grid(geom$y,length=geom$ylen, range=geom$yrange)
      ))
    }
    
    segs = get.geom.segments(geom=geom, dim=dim)
    yseq = seq(geom$yrange[1], geom$yrange[2], length=geom$ylen)
    if (length(segs)==1) {
      xseq = approx(x = geom$y,y=geom$x,xout=yseq)$y
      # nice one-to-one function
      return(list(x=xseq,y=yseq))
    } else {
      # deal with backward bending curve
      xseqs = unlist(lapply(segs, function(seg) {
        approx(x=seg$y,y=seg$x, xout=yseq)$y
      }))
      keep = !is.na(xseqs) 
      yseqs = rep(yseq,times=NROW(segs))[keep]
      xseqs = xseqs[keep]
      ord = order(yseqs,xseqs)
      
      return(list(x=xseqs[ord],y=yseqs[ord]))
    }
  }
  
}

add.geom.grids = function(geom, dim=c("x","y"), add.min.max=TRUE, overwrite=FALSE) {
  restore.point("add.geom.grids")
  
  if ("x" %in% dim) {
    if (is.null(geom[["xgr"]]) |  overwrite) {
      geom$xgr = compute.geom.grid(geom, dim="x")
    }
    if (add.min.max) {
      if (is.null(geom[["xgr.max"]])  | overwrite) {
        geom$xgr.max = geom.max.grid(geom,geom$xgr,dim = "x")
      }
      if (is.null(geom[["xgr.min"]])  | overwrite) {
        geom$xgr.min = geom.min.grid(geom,geom$xgr,dim = "x")
      }
    }  
  }
  if ("y" %in% dim) {
    if (is.null(geom[["ygr"]]) |  overwrite) {
      geom$ygr = compute.geom.grid(geom, dim="y")
    }
    if (add.min.max) {
      if (is.null(geom[["ygr.max"]])  | overwrite) {
        geom$ygr.max = geom.max.grid(geom,geom$ygr,dim = "y")
      }
      if (is.null(geom[["ygr.min"]])  | overwrite) {
        geom$ygr.min = geom.min.grid(geom,geom$ygr,dim = "y")
      }
    }  
  }
  geom  
}

```
# END OF FILE: positions.r

-----------------------------------------------------------


# FILE: svg.r
```
export.svg = function(html, dest.file,format=tools::file_ext(dest.file), width=NULL, height=NULL) {
  restore.point("export.svg")
  
  library(rsvg)
  library(convertGraph)
  dest.file = tools::file_path_sans_ext(dest.file)
  
  svg.file = paste0(dest.file,".svg")
  writeLines(html, svg.file)
  
  Encoding(html) <- "UTF-8"
  writeUtf8(html, svg.file)
  raw = charToRaw(paste0(html,collapse="\n"))
  for (form in format) {
    fun = paste0("rsvg_",form)
    to.file = paste0(dest.file,".",form)
    #do.call(fun,nlist(svg=svg.file,file=to.file,width, height))
  }
}

examples.svg = function() {
  library(dplyr)
  library(rmdtools)
  library(svglite)
  library(EconCurves)

  code = svg_from_plot(plot(1:10))$code
  cat(code)

  xrange = c(-10,10)
  yrange = c(0,100)

  svg = new_svg(xlim=xrange,ylim=yrange) %>%
    svg_xaxis(label="The x-axis") %>%
    svg_yaxis(label="The y-axis") %>%
    svg_boxed_label(x=-8,y=50,text="A label") %>%
    svg_polyline(x=(-10):10,y=((-10):10)^2,stroke = "blue", tooltip="I am a <bold>parabel</bold>.")
    

  html = svg_string(svg)
  cat(html)
  #html = paste0(html, collapse="\n")
  view.html(text=sep.lines(html))
}



svg_from_plot = function(call, width=500, height=400, envir=parent.frame(), bg="white",pointsize=1, pixel.per.inch=72, level=0, id=NULL) {
  quoted = substitute(call)
  restore.point("svg_from_plot")


  s <- svgstring(bg=bg, pointsize=pointsize, width=width / pixel.per.inch, height=height / pixel.per.inch, standalone = TRUE)

  eval(quoted, envir)
  code = sep.lines(s())
  head = code[1:2]
  code = code[-c(1:2,length(code))]
  dev.off()

  svg.code = svglite:::inlineSVG(eval(quoted,envir))

  el =id_char(id=id,code, collapse="\n")
  el.level = level
  names(el.level) = id

  coordmap = shiny:::getPrevPlotCoordmap(width=width, height=height)[[1]]

  svg = new.env()
  svg$head = head
  svg$width = width
  svg$height = height
  svg$coordmap = coordmap
  svg$dr =list(
    range=list(x=unlist(coordmap$range[1:2]), y=unlist(coordmap$range[3:4])),
    domain = list(x=unlist(coordmap$domain[1:2]), y=unlist(coordmap$domain[3:4]))
  )
  #svg$dr = make.domain.range(xlim=xlim,ylim=ylim,width=width, height=height)
  svg$el = el
  svg$el.level = el.level
  svg$defs
  svg$code = svg.code
  #svg = svg_def_label_box(svg)
  svg
}


new_svg = function(width=500, height=400, vb_w=width, vb_h=height, xlim=c(0,1),ylim=xlim,id=NULL, css=default_svg_css(), margins=c(bottom=80,left=100, top=40, right=50), class="clickable_svg", viewBox = paste0("0,0,",vb_w,",", vb_h)
) {
  restore.point("svg")

  if (is.null(id))
    id = paste0("svg_",random.string(1))
  svg = new.env()
  
  svg$id = id
  
  #svg$head = paste0("<svg xmlns='http://www.w3.org/2000/svg' version='1.1' width='",width,"' height='",height,"' id = '",id,"' class='",class,"'>")
  svg$head = paste0("<svg xmlns='http://www.w3.org/2000/svg' version='1.1' width='",width,"' height='",height,"' viewBox='",viewBox,"' id = '",id,"' class='",class,"'>")
  
  svg$width = width
  svg$height = height
  svg$dr = make.domain.range(xlim=xlim,ylim=ylim,width=width, height=height, margins=margins)
  svg$el = NULL
  svg$el.level = NULL
  svg$defs = NULL
  svg_add_def(svg, id="default_css", defs = paste0('
  <style type="text/css"><![CDATA[',
    css,'
  ]]>
  </style>
  '))
  #svg_def_label_box(svg)
  
  svg
}

default_svg_css = function() {
'
.axis-main {
  stroke: black;
  stroke-linecap: round;
  stroke-linejoin: round;
  stroke-width: 1.5;
  stroke-opacity: 0.8;
}


.polyline {
  stroke: black;
  stroke-linecap: round;
  stroke-linejoin: round;
  stroke-width: 2;
  stroke-opacity: 0.8;
}

.polyline:hover {
  stroke-width: 5;
}

.axis-tick {
  stroke-width: 0.5;
}

.axis-ticklabel {
  font-size: 10.00pt;
  font-family: Arial;
  font-weight: normal;
}

.axis-label {
  font-size: 11.00pt;
  font-family: Arial;
  font-weight: normal
}
'
}

svg_add = function(svg, el, id = names(el), level=0) {
  if (is.null(id)) {
    svg$el = c(svg$el,el)
    svg$el.level = c(svg$el.level, rep(level, length.out=length(el)))
  } else {
    svg$el[id] = el
    svg$el.level[id] = level
  }
  svg
}


svg_add_def = function(svg, defs, id = names(defs)) {
  if (is.null(id)) {
    svg$defs = c(svg$defs,defs)
  } else {
    svg$defs[id] = defs
  }
  svg
}


svg_string = function(svg) {
  defs = if (length(svg$defs)>0) c('<defs>', svg$defs,'</defs>') else NULL
  paste0(
    c(
      svg$head,
      defs,
      svg$el[order(svg$el.level)],
      "</svg>"
    ),
    collapse = "\n"
  )
}

make.domain.range = function(xlim,ylim,width=500, height=400, margins=c(bottom, left, top, right), bottom=height / 10, top = height/10, left = width/8, right=width/20) {
  restore.point("make.domain.range")
  
  list(
    domain=list(x=xlim,y=ylim),
    range = list(
      x=c(margins[2],width-margins[4]),
      y=rev(c(margins[3],height-margins[1]))
    )
  )
}

domain.to.range = function(x=NULL,y=NULL,domain=dr$domain, range=dr$range,dr=svg$dr, svg=NULL, to.range=TRUE) {
  restore.point("domain.to.range")
  if (!to.range) {
    if (is.null(x)) return(y)
    if (is.null(y)) return(x)
    return(list(x=x,y=y))
  }
  
  if (!is.null(x))
    x = ((x - domain$x[1]) /(domain$x[2]-domain$x[1])) * (range$x[2]-range$x[1]) + range$x[1]
  if (!is.null(y))
    y = ((y - domain$y[1]) /(domain$y[2]-domain$y[1])) * (range$y[2]-range$y[1]) + range$y[1]


  if (is.null(y)) return(x)
  if (is.null(x)) return(y)
  nlist(x,y)
}


range.to.domain = function(x=NULL,y=NULL,domain=dr$domain, range=dr$range,dr=svg$dr, svg=NULL) {
  restore.point("range.to.domain")

  if (!is.null(x))
    x = ((x - range$x[1]) /(range$x[2]-range$x[1])) * (domain$x[2]-domain$x[1]) + domain$x[1]
  if (!is.null(y))
    y = ((y - range$y[1]) /(range$y[2]-range$y[1])) * (domain$y[2]-domain$y[1]) + domain$y[1]


  if (is.null(y)) return(x)
  if (is.null(x)) return(y)
  nlist(x,y)
}


make_style_arg = function(style) {
  if (is.list(style)) {
    style = style[!sapply(style,is.null)]
    if (length(style)==0) return(NULL)
    na = names(style)
    na = gsub("_","-",na, fixed=TRUE)
    style = paste0(na,": ",style, collapse="; ")
  }
  style
}

svg_tag = function(name, args, inner=NULL, .quote='"',tooltip=NULL) {
  restore.point("svg_tag")
  args = args[!sapply(args, is.null)]
  args$style = make_style_arg(args$style)
  arg.str = paste0(names(args),"=",.quote,unlist(args),.quote, collapse=" ")

  if (!is.null(tooltip)) {
    inner =c(paste0("<title>",tooltip,"</title>"),inner)
  }
  if (is.null(inner)) {
    str = paste0("<",name," ", arg.str,"/>")
  } else {
    str = paste0("<",name," ", arg.str,">\n",paste0(unlist(inner),collapse="\n"),"</",name,">")
  }
  str
}

html_arg_str = function(..., .quote='"') {
  args = list(...)
  restore.point("html_arg_str")

  args$style = make_style_arg(args$style)
  args = args[!sapply(args, is.null)]
  arg.str = paste0(names(args),"=",.quote,args,.quote, collapse=" ")
  arg.str
}


svg_point = function(svg, x,y,id=NULL, class="point",level=110,fill=NULL, tooltip=NULL,label = NULL,r=5,...) {
  restore.point("svg_point")
  rp = domain.to.range(x=x,y=y,svg=svg)
  ci = svg_tag("circle",nlist(cx=rp$x,cy=rp$y,r=r,class,id=id,fill=fill,...), tooltip=tooltip) 
  el = ci
  svg_add(svg,el,id,level=level)
  if (!is.null(label)) {
    svg_boxed_label(svg,rp$x+r+1,rp$y,text=label, to.range = FALSE,id=paste0("label__",id), tooltip=tooltip,level=level, class="point-label")
  }
}


svg_polyline = function(svg, x,y,id=NULL, class="polyline",style=c(nlist(fill, stroke,stroke_width), extra.style), fill="none", stroke="black",stroke_width=NULL, extra.style=list(), level=0, tooltip=NULL, extra.args = list(...),...) {
  restore.point("svg_polyline")
  rp = domain.to.range(x=x,y=y,svg=svg)
  points = paste0(rp$x,",",rp$y, collapse=" ")
  el = svg_tag("polyline", c(nlist(points,id,class,style), extra.args), tooltip=tooltip)
  svg_add(svg,el,id,level=level)
}

color.inner.latex = function(inner, color=NULL) {
  if (is.null(color)) return(inner)
  paste0("\\color{",color,"}{",inner,"}")  
}

svg_mathjax_label = function(svg, x,y, text, latex = paste0("\\(",color.inner.latex(text,color),"\\)"),id=NULL, class=NULL,style=c(nlist("font-size"=font_size), extra.style), font_size=12, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE,align="",color=NULL,...) {
  restore.point("svg_mathjax_label")
  text = paste0(align,latex)
  rp = domain.to.range(x=x,y=y,svg=svg, to.range=to.range)
  el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,style,...), tooltip=tooltip, inner=text)
  svg_add(svg,el,id,level=level)
}



svg_boxed_label = function(svg, x,y, text,id=NULL, class="boxed-label",style=c(nlist("font-size"=font_size), extra.style), font_size=NULL, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE,...) {
  restore.point("svg_boxed_label")
  rp = domain.to.range(x=x,y=y,svg=svg, to.range=to.range)
  el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,class,style,...), tooltip=tooltip, inner=text)
  #el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,style,...), tooltip=tooltip, inner=text)
  svg_add(svg,el,id,level=level)
}



svg_xaxis = function(svg, id="xaxis", label=NULL, latex = NULL,  y="default", dr=svg$dr, return.string=FALSE, level=100, num.ticks=5, ticks =pretty.ticks(dr$domain$x, n=num.ticks), tick.size = 10, arrow=!show.ticks, show.ticks = TRUE, show.tick.labels=show.ticks, class.group= "axis x-axis",  class.line="axis-main", class.tick="axis-tick",class.tick.label="axis-ticklabel", class.label="axis-label", style.line=NULL, style.tick=NULL,style.tick.label=NULL, style.label=NULL, axis.offset=if (show.ticks) 10 else 0, axis.label.offset=if (show.ticks) 30 else 20, custom.ticks=NULL,...) {
  restore.point("svg_xaxis")
  x.ax = dr$range$x
  if (y=="default" || y == "bottom") {
    y = dr$range$y[1] + axis.offset
  } else if (y=="top") {
    y = dr$range$y[2] - axis.offset
  } else if (y=="zero") {
    y = max(dr$range$y[1],domain.to.range(y=y,svg = svg))
  } else if (is.numeric(y)) {
    y = domain.to.range(y=y,svg = svg)
  }
  y.ax = rep(y,2)

  if (arrow) {
    arrow.id = paste0(svg$id,"_arrow_head")
    svg_def_arrow_head(svg)
    arrow.li = list("marker-end"=paste0("url(#",arrow.id,")"))
  } else {
    arrow.li = NULL
  }
   line = svg_tag("line", c(nlist(x1=x.ax[1],x2=x.ax[2]+10,y1=y.ax[1],y2=y.ax[2], style=style.line, class=class.line), arrow.li))

  y1.tick = y.ax[1]
  y2.tick = y1.tick + tick.size * show.ticks
  x.ticks = domain.to.range(x=ticks,svg = svg)

  if (show.ticks) {
    ti.str = paste0('<line x1="',x.ticks,'" x2="',x.ticks,'" y1="',y1.tick,'" y2="',y2.tick,'" ', html_arg_str(style=style.tick, class=class.tick),'/>')
  } else {
    ti.str = ""
  }
  
  if (show.tick.labels) {
    ti.lab = paste0('<text x="',x.ticks,'" y="',y2.tick+15,'" ', html_arg_str(style=style.tick.label, class=class.tick.label),' text-anchor="middle">',ticks,"</text>")
  } else {
    ti.lab = ""
  }
  
  if (!is.null(custom.ticks)) {
    y1.tick = y.ax[1]
    y2.tick = y1.tick + tick.size
    cx.ticks = domain.to.range(x=custom.ticks,svg = svg)
    cti.str = paste0('<line x1="',cx.ticks,'" x2="',cx.ticks,'" y1="',y1.tick,'" y2="',y2.tick,'" ', html_arg_str(style=style.tick, class=class.tick),'/>')
  } else {
    cti.str = ""
  }

  if (!is.null(latex)) {
    label = latex.to.textspan(latex)
  }
  if (!is.null(label)) {
    y.lab =  y2.tick+axis.label.offset
    x.lab = x.ax[2]+arrow*10
    label = svg_tag(name = "text",args=list(x=x.lab,y=y.lab,style=style.label,class=class.label, "text-anchor"="right"),inner=label)
  }
  
  inner = c(line,cti.str, ti.str, ti.lab,label)

  g = svg_tag("g", nlist(id,class=class.group),inner=inner)
  svg_add(svg,g,id=id, level=level)
}


svg_yaxis = function(svg, id="yaxis", label=NULL,latex = NULL,x="left", dr=svg$dr, return.string=FALSE, level=100, num.ticks=5, ticks =pretty.ticks(dr$domain$y, n=num.ticks), tick.labels=ticks, tick.size = 10, arrow=!show.ticks, show.ticks = TRUE, show.tick.labels=show.ticks,
  axis.offset = if (show.ticks) 10 else 0, axis.label.offset=20,
  class.group= "axis y-axis",  class.line="axis-main", class.tick="axis-tick",class.tick.label="axis-ticklabel", class.label="axis-label",
  style.line=NULL, style.tick=NULL,style.tick.label=NULL, style.label=NULL, show.line=TRUE, custom.ticks=NULL,...  ) {
  restore.point("svg_yaxis")

  y.ax = dr$range$y
  if (is.null(x)) x= dr$domain$x[1]
  min.xr = dr$range$x[1]
  
  if (x == "default" || x=="left") {
    x = min.xr - axis.offset
  } else if (x=="zero") {
    x = max(min.xr,domain.to.range(x=x,svg = svg))
  } else if (x=="right") {
    x = dr$range$y[2] + axis.offset
  } else if (is.numeric(x)) {
    x = domain.to.range(x=x,svg = svg)
  }
  x.ax = rep(x,2)

  if (show.line) {
    if (arrow) {
      arrow.id = paste0(svg$id,"_arrow_head")
      svg_def_arrow_head(svg)
      arrow.li = list("marker-end"=paste0("url(#",arrow.id,")"))
    } else {
      arrow.li = NULL
    }
  
    svg_def_arrow_head(svg)
    line = svg_tag("line", c(nlist(x1=x.ax[1],x2=x.ax[2],y1=y.ax[1],y2=y.ax[2], style=style.line, class=class.line), arrow.li))
  } else {
    line = NULL
  }
  
  x1.tick = x.ax[1] - tick.size * show.ticks
  x2.tick = x1.tick + tick.size * show.ticks
  y.ticks = domain.to.range(y=ticks,svg = svg)
  if (show.ticks) {
    ti.str = paste0('<line x1="',x1.tick,'" x2="',x2.tick,'" y1="',y.ticks,'" y2="',y.ticks,'" ', html_arg_str(style=style.tick, class=class.tick),'/>')
  } else {
    ti.str = ""
  }


  
  if (show.tick.labels) {
    
    ti.lab = paste0('<text x="',x1.tick-3,'" y="',y.ticks,'" ', html_arg_str(style=style.tick.label, class=class.tick.label),' text-anchor="end" alignment-baseline="middle">',tick.labels,"</text>")
  } else {
    ti.lab = ""
  }
  
  if (!is.null(custom.ticks)) {
    x1.tick = x.ax[1] - tick.size 
    x2.tick = x1.tick + tick.size 
    cy.ticks = domain.to.range(y=custom.ticks,svg = svg)
    cti.str = paste0('<line x1="',x1.tick,'" x2="',x2.tick,'" y1="',cy.ticks,'" y2="',cy.ticks,'" ', html_arg_str(style=style.tick, class=class.tick),'/>')
  } else {
    cti.str = ""
  }

  if (!is.null(latex)) {
    label = latex.to.textspan(latex)
  }
  if (!is.null(label)) {
    x.lab =  x1.tick
    y.lab = y.ax[2]-10-arrow*10
    label = svg_tag(name = "text",args=list(x=x.lab,y=y.lab,style=style.label,class=class.label,"text-anchor"="middle"),inner=label)
  }
  
  inner = c(line,cti.str, ti.str, ti.lab,label)

  g = svg_tag("g", nlist(id,class=class.group),inner=inner)
  svg_add(svg,g,id=id, level=level)
}



pretty.ticks = function(x,n=5,max.rel.out = 0.01,...) {
  ticks = pretty(x,n,...)
  # remove ticks that are too far outside the range
  tol = (max(x)-min(x))*max.rel.out
  ticks = ticks[ticks >= min(x)-tol & ticks<=max(x)+tol]
  ticks
}
      
        
id_char = function(id,...) {
  res = paste0(...)
  names(res) = id
  res
}
```
# END OF FILE: svg.r

-----------------------------------------------------------


# FILE: view_bb.r
```

disable.view.bb = function(disable=TRUE) {
  options(disable_view_bb = disable)
}

view.bb = function(bb, latexsvg=isTRUE(bb$use.latex), launch.browser = rstudioapi::viewer, skip = isTRUE(getOption("disable_view_bb")),...) {
  restore.point("view.bb")
  if (skip) return()
  library(shinyEvents)
  svg = bb_to_svg(bb,id = "mysvg", return.svg.object = TRUE)

  www = system.file("www", package="bbsvg")  
  addResourcePath(prefix = "bbsvg",directoryPath = www)
  
  hsvg = svg_string(svg)
  if (latexsvg) {
    hsvg = latexsvg::latexsvg(hsvg)
  }

  app=eventsApp()
  
  app$prev.xy = NULL
  
  app$ui = fluidPage(
    tags$head(tags$script(src="bbsvg/velocity.min.js")),
    div(style="cursor: crosshair;",HTML(hsvg))
  )
  svgClickHandler("mysvg", function(x,y,app=getApp(),...) {
    args = list(...)
    restore.point("svg_click")
    dom = range.to.domain(x=x,y=y,svg=svg)
    cat(paste0("\n range  x=",x,",y=",y))
    
    # compute number of rounding digits
    dw = diff(svg$dr$domain$x)
    x.round = pmax(round(-log(dw / 1000,base = 10)),1)
    dh = diff(svg$dr$domain$y)
    y.round = pmax(round(-log(dh / 1000,base = 10)),1)
    
    cat(paste0("\n domain x=",round(dom$x,x.round),",y=",round(dom$y,y.round)))
    msg = paste0('\tbb_text(x=',round(dom$x,x.round),',y=',round(dom$y,y.round),',label="", align="left", color=NULL)  %>%')
    writeClipboard(msg)
    cat(paste0('\n',msg))
    
    msg = paste0('\tbb_point(x=',round(dom$x,x.round),',y=',round(dom$y,y.round),', r=3)  %>%')
    cat(paste0('\n',msg))

    prev.xy = app$prev.xy
    if (!is.null(prev.xy)) {
      msg = paste0('\tbb_arrow(x1=',round(prev.xy[1],x.round),',y1=',round(prev.xy[2],y.round),',x2=',round(dom$x,x.round),',y2=',round(dom$y,y.round),', linetype="solid",color="black")  %>%')
      cat(paste0('\n',msg))
    }
    app$prev.xy = c(x=dom$x,y=dom$y)
  })
  
  viewApp(app,launch.browser = launch.browser)
}

```
# END OF FILE: view_bb.r

-----------------------------------------------------------


```md
Author: Sebastian Kranz (Ulm University)

`bbsvg` is an R package that shall help to make svg graphics similar to graphs drawn on blackboards in my economics classes.

The functions are not yet documented, but this README illustrates the package with examples.

```r
bb = bb_pane(xrange=c(0,110),yrange=c(0,600), show.ticks=FALSE, org.width = 480,  org.height=320) %>%
  bb_xaxis(label = "GW") %>%
  bb_yaxis(label = "Euro / MWh", labelpos="top") %>%
  
  bb_area_rect(x1=0,x2=80,y1=0,y2=200, fill="#aa6666") %>%
  bb_text(x=40, y=100, label="Coal", align="center") %>%  
  
  bb_area_rect(x1=80,x2=100,y1=0,y2=500, fill="#8888aa") %>%
  bb_text(x=90, y=100, label="Gas", align="center")  %>%
  
  bb_vline(x=100) %>%
  bb_text(x=100,align = "center",y=640,label="Demand") %>%
  
  bb_segment(y=500,x1=0,x2=100,linetype = "dashed") %>%
  bb_ytick(y=500, label="p")

```



## Format of proposed changes to code or text files

When you suggest changes to code or text files or completely new files, you MUST use the following format for each modification. Each change must be enclosed in a `!MODIFICATION` block.

### Overall Structure

Each modification block has three parts:
1.  Start and end markers: `!MODIFICATION {{what}}` and `!END_MODIFICATION {{what}}`, where {{what}} is just a short reference to what is modified, file name or function name with with file. It will not be parsed but makes it easier for a human to understand blocks.
2.  A metadata block in **TOML format**. This block ends with a `---` separator line.
3.  A code payload block, which is a standard markdown code fence.

```
!MODIFICATION {{what}}
# TOML metadata goes here
# ...
---
```language
# New code payload goes here
```
!END_MODIFICATION {{what}}
```

### Modification Scope

Each modification is of one of the following two scopes:

* `file` (re-)writes a complete file

* `function` (re-)writes a complete function (including comments above).



If more than two functions or more than two line edits will be performed in the same file, better rewrite the whole file using a `file` scope. For extremely long files also more smaller edits are ok.

Function scope only works for R code files, but in R files it is preferred if one or two functions in a larger file are changed.

The metadata block **MUST** contain a `scope` field, which can be `"file"`, `"function"`, or `"lines"`.

---

### **Scope 1: `file`**

Use this to create a new file or to completely rewrite an existing one.

**Required Fields:**
*   `scope = "file"`
*   `file` (string): The path to the file. Very important: state the path of the file as shown in the files header above. If there is a complete absolute path, also show the complete absolute path here.
*   `description` (string): A brief explanation of the change in quotes ""

#### **Example 1.1: Creating or modifying a file**
!MODIFICATION new_helpers.R
scope = "file"
file = "/home/rstudio/myproject/R/new_helpers.R"
description = "Create a new file for helper functions."
---
```r
# A new helper function
say_hello = function(name) {
  paste0("Hello, ", name)
}
```
!END_MODIFICATION new_helpers.R


### **Scope 2: `function`**

Use this to replace an existing function or to insert a new function. The new code payload should contain the complete function, including any preceding comments.

#### **Fields for replacing an existing function:**
*   `scope = "function"`
*   `file` (string): The path to the file. Very important: state the path of the file as shown in the files header above. If there is a complete absolute path, also show the complete absolute path here.
*   `function_name` (string): The name of the function to be replaced.
*   `description` (string): A brief explanation of the change in quotes.

#### **Fields for inserting a new function:**
*   `scope = "function"`
*   `file` (string): The path to the file. Very important: state the path of the file as shown in the files header above. If there is a complete absolute path, also show the complete absolute path here.
*   `description` (string): A brief explanation of the change, enclose in quotes.
*   **One of** the following fields to specify the insertion point. They are mutually exclusive.
    *   `insert_top = true`: Insert at the top of the file.
    *   `insert_bottom = true`: Insert at the bottom of the file.
    *   `insert_before_fun = "function_name"`: Insert before the specified function.
    *   `insert_after_fun = "function_name"`: Insert after the specified function.


#### **Example 2.1: Replacing an existing function**
!MODIFICATION calculate_sum utils.R
scope = "function"
file = "R/utils.R"
function_name = "calculate_sum"
description = "Update `calculate_sum` to handle NA values correctly.""
---
```r
#' Calculate the sum of a vector, ignoring NAs
calculate_sum = function(vec) {
  sum(vec, na.rm = TRUE)
}
```
!END_MODIFICATION calculate_sum utils.R

#### **Example 2.2: Inserting a new function at the bottom of a file (here was an absolute file path given)**
!MODIFICATION is_positive in R/utils.R
scope = "function"
file = "/home/myuser/mypkg/R/utils.R"
insert_bottom = true
description = "Add a new helper function to check for positivity."
---
```r

#' Check if a number is positive
is_positive = function(n) {
  n > 0
}
```
!END_MODIFICATION is_positive in R/utils.R

#### **Example 2.3: Inserting a new function after a specific function**
!MODIFICATION is_negative in R/utils.R
scope = "function"
file = "R/utils.R"
insert_after_fun = "is_positive"
description = "Add a new helper function `is_negative` after `is_positive`."
---
```r

#' Check if a number is negative
is_negative = function(n) {
  n < 0
}
```
!END_MODIFICATION is_negative in R/utils.R


# General coding instructions

- For string operations try to use `stringi` functions.

- Try to avoid loops, vectorize if possible.

- Use `=` instead of `<-` as assignment operator.

- Very Important: Do not warp code into `try` statements to generally catch errors.
  I want that the code fails if there are errors we have not yet understood dealt with.
  General try catch wrappers will dillude this goal. Existing wrappers are probably
  ok, but don't add new ones, unless there is really a strong reason to believe that
  this cleanly solves the underlying problem and does not hide other possible problems.



#######################################################
# YOUR TASK
#######################################################

Somehow if I have a label

"Initial\nSupply\n S_{0}"

The S_{0} is not transformed to math (no latex mode) but it is transformed in the label

"Initial Supply S_{0}"

Any idea why the newline \n may disable transformation? Can you fix?



