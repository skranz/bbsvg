examples.init.curve = function() {
  yaml = '
IS:
  eq: r == A-a*y
  color: red
  xy: [y,r]
'
  
  curve = init.yaml.curve(yaml=yaml)
  geom = curve.to.geom(curve,xrange=c(0,1),yrange=c(0,200),values=list(A=100,a=1))
}

init.curve = function(name=NULL, eq=NULL, xvar=NULL,yvar=NULL, color=NULL,label=NULL, curve=list(), var.funs=NULL,...) {
  restore.point("init.curve")
  
  curve = copy.into.null.fields(dest=curve, source=nlist(name,eq,xvar, yvar,color, label))

  if (is.character(curve$eq)) {
    curve$eq_ = parse.as.call(text=curve$eq)
  } else {
    curve$eq_ = curve$eq = strip.parentheses(curve$eq)
  }

  
  if (is.null(curve$name)) {
    curve$name = attr(curve,"name")
  }
  
  if (!is.null(curve["xy"])) {
    if (is.null(curve$xvar))
      curve$xvar = curve$xy[1]
    if (is.null(curve$xvar))
      curve$yvar = curve$xy[2]
  }

  check.curve(curve)
  
  
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
  curve
}

init.curves = function(curves,...) {
  curve.names = names(curves)
  curves = lapply(seq_along(curves), function(i) {
    init.curve(name=curve.names[i],..., curve=curves[[i]])
  })
  names(curves) = curve.names
  curves
}

init.yaml.curve = function(yaml=NULL, curve=NULL, var.funs=NULL) {
  restore.point("init.yaml.curve")
  
  if (is.null(curve)) {
    li = read.yaml(text=yaml)
    curve = li[[1]]
    if (is.null(curve$name))
      curve$name = names(li)[1]
  }
  
  init.curve(curve=curve, var.funs=var.funs)
}

specialize.curve.formula = function(eq, xvar, yvar, level=NULL, solve.symbolic = TRUE) {
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

