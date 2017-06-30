bb_isoquant = function(bb, Q,x,y,...,id=paste0("isoquant_",random.string()),xvar=bb$xvar,yvar=bb$yvar) {
  restore.point("bb_isoquant")
  slope = isoquant.slope(Q,xvar,yvar)
  bb_slopecurve(bb,x=x,y=y,slope=slope,xvar=xvar,yvar=yvar,id=id,...)
}

bb_slopecurve = function(bb,x,y,slope,color=NULL, lwd=NULL, style=nlist(stroke=color, stroke_width=lwd,...), x.move=0, y.move=0, xrange=bb$xrange, yrange=bb$yrange, var.funs=NULL,tooltip=NULL,..., data=NULL, id=paste0("slopecurve_",random.string()),xvar=bb$xvar,yvar=bb$yvar) {
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



bb_curve = function(bb,id=random.string(),eq,latex=NULL, label=NULL, data=NULL,color=NULL, lwd=NULL, style=nlist(stroke=color, stroke_width=lwd,...), var.funs=NULL,labpos=NULL,labx=NULL, laby=NULL,tooltip=NULL,dy=NULL,dx=NULL,no.draw=FALSE, xrange=bb$xrange, yrange=bb$yrange,...) {
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

  yformula_ = xformula_ = NULL

  curve.vars = c(vl, vr)
  is.vertical = ! yvar  %in% curve.vars
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
  is.linear = (!xvar %in% slope.vars) & (! yvar %in% slope.vars) 
  
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

