
bb_tangent = function(bb, x,y, slope, width=NULL, alpha=NULL,color=NULL, class="segment",linetype="solid", ..., id=paste0("tangent_",random.string())) {
  if (is.null(width)) {
    width = 2*(diff(bb$xrange)+diff(bb$yrange))
  }
  if (length(width)==1) {
    sw = ew = paste0("0.5 * ",width)
  } else {
    sw = width[1]
    ew = width[2]
  }
  x1 = paste0(x, " - ", sw)
  x2 = paste0(x, " + ", ew)
  y1 = paste0(y, " - ", slope, " * ",sw)
  y2 = paste0(y, " + ", slope, " * ",ew)

  bb_segment(bb,x1=x1,x2=x2,y1=y1,y2=y2,alpha=alpha,color=color, class="segment",linetype=linetype,...)
}

bb_segment = function(bb, x1,x2=x1,y1,y2=y1, alpha=NULL,color=NULL, class="segment",linetype="solid", lwd=NULL, dasharray = linetype.to.dasharry(linetype),  style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), ..., id=paste0("segment_",random.string())) {
  restore.point("bb_segment")
  obj = nlist(id, type="segment", class, x1,y1,x2,y2, style,"stroke-dasharray"=dasharray, eval.fields=c("x1","y1","x2", "y2"))
  bb_object(bb, obj)
}



bb_arrow = function(bb, x1,x2=x1,y1,y2=y1, arrow.head=c("end"), alpha=NULL,color=NULL, class="arrow", style=list(stroke=color, "stroke-opacity"=alpha,...), ..., id=random.string()) {
  obj = nlist(id, type="arrow", class, x1,y1,x2,y2, style, eval.fields=c("x1","y1","x2", "y2"))
  bb_object(bb, obj)
}

