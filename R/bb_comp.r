
bb_compute_objs = function(bb) {
  restore.point("bb_compute_objs")
  bb$values = bb$data
  # later object may use computations from earlier ones
  for (i in seq_along(bb$objs)) {
    bb=bb_compute_obj(bb=bb,obj=bb$objs[[i]],i=i)
  }
  bb
}

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
  obj$geom$tooltip = replace.latex.with.unicode(replace.whiskers(obj$tooltip,obj.values(obj, bb)))
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
  bb$values
}

compute_bb_fields = function(obj, fields, values=obj.values(obj,bb), enclos=bb$enclos, bb=NULL){
  restore.point("compute_bb_fields")
  li = lapply(obj[fields], function(field) {
    compute_bb_field(field, values=values, enclos=enclos)
  })
  li
}

compute_bb_field = function(field, values=obj.values(obj,bb), enclos=bb$enclos, bb=NULL, obj=NULL) {
  restore.point("compute_bb_field")
  if(is.null(enclos)) enclos = parent.frame()
  if (is.null(field)) return(NULL)
  if (is.numeric(field)) return(field)
  
  if (is(field,"formula")) {
    if (length(field)==1) return(NULL)
    call = field[[2]]
    return(eval(call, values,enclos = enclos))
  }
  
  if (is.character(field)) {
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
  
}
