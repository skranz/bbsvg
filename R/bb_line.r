
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

bb_hline = function(bb,y,alpha=NULL,color=NULL, class="segment",linetype="solid", lwd=NULL,  style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), ..., id=paste0("hline_",random.string())) {
  bb_segment(bb,y=y, x1=bb$x.min, x2=bb$x.max, color=color, class=class, alpha=alpha, lwd=lwd, linetype=linetype, style=style, id=id)
}

bb_vline = function(bb,x,alpha=NULL,color=NULL, class="segment",linetype="solid", lwd=NULL,  style=list(stroke=color, "stroke-opacity"=alpha, "stroke-width"=lwd,...), ..., id=paste0("hline_",random.string())) {
  bb_segment(bb,x=x, y1=bb$y.min, y2=bb$y.max, color=color, class=class, alpha=alpha, lwd=lwd, linetype=linetype, style=style, id=id)
}

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
