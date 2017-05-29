
bb_text = function(bb, label=NULL,latex=NULL,x=NULL,y=NULL,xrel=NULL, yrel=NULL,align="center", x.offset=0, y.offset=0, boxed=FALSE, font_size=14, color=NULL, style=list("font-size"=font_size,"fill"=color,"stroke"=color),  ..., id=random.string()) {
  restore.point("bb_text")
  use.latex = !is.null(latex)
  bb$use.latex = isTRUE(bb$use.latex) | use.latex
  
  obj = nlist(id, label, latex, x,y,xrel,yrel, use.latex,align,label.mode=ifelse(use.latex,"latex","text"), x.offset, y.offset, boxed,style, font_size, ...)
  bb$labels[[id]] = obj
  bb
}

bb_xtick = function(bb,x=NULL,...,label=NULL,latex=NULL, align="center", y.offset=-20,y=NULL, id = random.string()) {
  y=first.non.null(y,bb$y0)
  bb_text(bb,latex=latex,label=label,  align=align, y.offset=y.offset,y=y,x=x, ..., id=id)
}

bb_ytick = function(bb,y=NULL,...,label=NULL,latex=NULL, align="right", x.offset=-5, x=NULL, id = random.string()) {
  x=first.non.null(x,bb$x0)
  bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, x.offset=x.offset, ..., id=id)
}


compute_bb_label = function(bb, obj) {
  restore.point("compute_bb_label")
  geom = compute_bb_fields(obj,c("x","y","xrel","yrel","x.offset","y.offset","boxed"), bb$values)
  
  if (!is.null(geom$xrel)) 
    geom$x = geom$xrel*max(bb$xrange) + (1-geom$xrel)*min(bb$xrange)
  
  if (!is.null(geom$yrel)) 
    geom$y = geom$yrel*max(bb$yrange) + (1-geom$yrel)*min(bb$yrange)
  
  
  geom$label = first.non.null(obj$label, obj$latex)
  geom$tooltip = obj$tooltip
  obj$geom = geom
  obj
  
}

draw.svg.label = function(svg,obj, display.whisker=FALSE) {
  restore.point("draw.svg.label")
  geom = obj$geom
  display=""
  if (display.whisker)
    display = paste0("{{display_",obj$id,"}}")
    
  x = domain.to.range(x = geom$x,svg = svg)
  if (!is.null(geom$x.offset))
    x = x + geom$x.offset
  y = domain.to.range(y = geom$y,svg = svg)
  if (!is.null(geom$y.offset))
    y = y - geom$y.offset
  
  if (isTRUE(obj$label.mode=="latex")) {
    align = obj$align
    if (align=="right") align="R"
    if (align=="left") align="L"
    if (align=="center") align=""
    
    
    svg_mathjax_label(svg,x=x,y=y, text=geom$label,id=obj$id, level=first.non.null(obj$level,100), align=align, to.range = FALSE, tooltip = geom$tooltip, display=display)
    
  } else {
    anchor = "middle"
    if (obj$align=="right") anchor = "end"
    if (obj$align=="left") anchor ="start"
    if (isTRUE(geom$boxed)) {
      svg_boxed_label(svg,x=x,y=y, text=geom$label,id=obj$id, class=class, level=first.non.null(obj$level,100), style=obj$style, to.range = FALSE, "text-anchor"=anchor, tooltip = geom$tooltip, display=display)
    } else {
       svg_text(svg,x=x,y=y, text=geom$label,id=obj$id, class=class, level=first.non.null(obj$level,100), font_size=obj$font_size, style=obj$style, to.range = FALSE, "text-anchor"=anchor, tooltip = geom$tooltip, display=display)
    }
    
    class = ifelse(isTRUE(geom$boxed),"boxed_label","bb_text")
  }
}


svg_text = function(svg, x,y, text,id=NULL, class="boxed-label",style=c(nlist("font-size"=font_size), extra.style), font_size=NULL, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE,...) {
  restore.point("svg_boxed_label")
  
  text = sep.lines(text)
  if (length(text)>1) {
    text = multiline.tspans(text,x = x,y=y)
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