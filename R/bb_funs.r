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