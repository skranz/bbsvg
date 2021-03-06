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

