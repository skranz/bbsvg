linetype.to.dasharry = function(linetype) {
  if (linetype=="dashed") return("4,4")
  if (linetype=="dotted") return("2,4")
  if (linetype=="dotdash") return("2,4,4,4")
  if (linetype=="longdash") return("8,4")
  if (linetype=="twodash") return("4,1,4,4")
  return(NULL)
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
