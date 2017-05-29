
init.pane.objects = function( pane) {
  objects = pane[["objects"]]
  if (is.null(objects)) {
    pane$objects = list()
    return()
  }

  names = names(objects)
  objects = lapply(seq_along(objects), function(i) {
    obj = objects[[i]]
    obj$name = names[[i]]
    init.object(obj,name=names[[i]],pane=pane)
  })
  names(objects) = names
  objects
}


init.object = function(obj, name=obj$name,pane) {
  restore.point("init.object")
  
  type = obj$type
  fun = paste0("init.",type)
  do.call(fun,list(name=name,obj=obj,pane=pane))
  
}