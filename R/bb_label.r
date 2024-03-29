
bb_text = function(bb, label=NULL,latex=NULL,x=NULL,y=NULL,xrel=NULL, yrel=NULL,align="center", x.offset=0, y.offset=NULL, boxed=FALSE, font_size=14, color=NULL, style=list("font-size"=font_size,"fill"=color),valign=c("center","bottom","top")[1], vertical=FALSE,  ..., id=random.string(), fill.background=FALSE, background.alpha=0.8, background.color="#ffffff") {
  restore.point("bb_text")
  use.latex = !is.null(latex)
  bb$use.latex = isTRUE(bb$use.latex) | use.latex
  
  if (use.latex) label = NULL
  
  if (is.null(y.offset)) {
    if (valign =="center") {
      y.offset = -ceiling(0.4*font_size)
    } else if (valign=="top") {
      y.offset = -ceiling(font_size)
    } else {
      y.offset = 0
    }

  }

  ma = bb.normalize.multi.arguments(nlist(x,y,label,latex))
  
  if (ma$len == 1) {
    obj = nlist(id, label, latex, x,y,xrel,yrel, use.latex,align,label.mode=ifelse(use.latex,"latex","text"), x.offset, y.offset, boxed,style, font_size,vertical=vertical, color, ...)
    if (fill.background) {
      bg = text.background.obj(obj, alpha=background.alpha, color=background.color)
      if (!is.null(bg)) bb$labels[[bg$id]] = bg
    }
    
    bb$labels[[id]] = obj
    return(bb)
  }
  restore.point("bb_text.multi")

  bid = id
  for (i in seq_len(ma$len)) {
    id = paste0(bid,"_",i)
    bb = bb_text(bb,id=id, label=ma$li$label[[i]], latex =ma$li$latex[[i]], x=ma$li$x[[i]],y=ma$li$y[[i]], use.latex=use.latex,align=align, x.offset=x.offset, y.offset=y.offset, boxed=boxed,style=style, font_size=font_size,vertical=vertical, color=color, fill.background=fill.background, ...)

  }
  bb
  
  
}



# create a new text object that functions as a 
# (white) background for the original text object
text.background.obj = function(obj,color="#ffffff", alpha=0.8, ...) {
  restore.point("text.background.obj")
  if (is.null(obj$label))
    return(NULL)
  label = sep.lines(obj$label)
  bg = sapply(label, function(el) paste0(rep("█", nchar(el)), collapse=""))
  obj$label = mark_utf8(bg)
  obj$style$fill = color
  obj$style[["fill-opacity"]] = alpha
  obj$id = paste0(obj$id,"-background")
  obj
}

bb_xtick = function(bb,x=NULL,...,label=x,latex=NULL, align="center", y.offset=-20,y=NULL, id = random.string(),  add.tick.line=TRUE) {
  restore.point("bb_xtick")
  y=first.non.null(y,bb$y.min)
  bid = id
  
  bb = bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, y.offset=y.offset, ..., id=id)
  if (add.tick.line) {
    bb$custom.xticks = c(bb$custom.xticks,x)
  }

  bb
}

bb_ytick = function(bb,y=NULL,...,label=y,latex=NULL, align="right", x.offset=ifelse(add.tick.line,-15,-5), x=NULL, id = random.string(), add.tick.line=TRUE) {
  restore.point("bb.ytick")
  x=first.non.null(x,bb$x.min)
  #bb = bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, x.offset=x.offset,id=id)
  bb = bb_text(bb,x=x,y=y,latex=latex,label=label, align=align, x.offset=x.offset, ..., id=id)

  if (add.tick.line) {
    bb$custom.yticks = c(bb$custom.yticks,y)
  }
  
  
  bb
}


compute_bb_label = function(bb, obj) {
  restore.point("compute_bb_label")
  geom = compute_bb_fields(obj,c("x","y","xrel","yrel","x.offset","y.offset","boxed"), bb$values)

  if (!is.null(geom$xrel))
    geom$x = geom$xrel*max(bb$xrange) + (1-geom$xrel)*min(bb$xrange)

  if (!is.null(geom$yrel))
    geom$y = geom$yrel*max(bb$yrange) + (1-geom$yrel)*min(bb$yrange)


  geom$label = first.non.null(obj$label, obj$latex)
  
  geom$label = compute_bb_field(geom$label,bb=bb, obj=obj, character.field = TRUE)

  geom$tooltip = obj[["tooltip"]]
  obj$geom = geom
  obj

}

draw.svg.label = function(svg,obj, display.whisker=FALSE,bb=NULL) {
  restore.point("draw.svg.label")
  geom = obj$geom
  display=""
  if (display.whisker)
    display = paste0("{{display_",obj$id,"}}")

  x = domain.to.range(x = geom$x,svg = svg)
  org.x = x
  if (!is.null(geom$x.offset))
    x = x + geom$x.offset
  y = domain.to.range(y = geom$y,svg = svg)
  org.y = y
  if (!is.null(geom$y.offset))
    y = y - geom$y.offset

  if (isTRUE(obj$label.mode=="latex")) {
    align = obj$align
    if (align=="right") align="R"
    if (align=="left") align="L"
    if (align=="center") align=""


    svg_mathjax_label(svg,x=x,y=y, text=geom$label,id=obj$id, level=first.non.null(obj$level,100), align=align, to.range = FALSE, tooltip = geom$tooltip, display=display, color=obj$color)

  } else {
    anchor = "middle"
    if (obj$align=="right") anchor = "end"
    if (obj$align=="left") anchor ="start"

    transform=NULL
    if (isTRUE(obj$vertical)) 
      transform=paste0("rotate(-90, ",org.x,", ",org.y,")")
    svg_text(svg,x=x,y=y, text=geom$label,id=obj$id, class="bb_text", level=first.non.null(obj$level,100), font_size=obj$font_size, style=obj$style, to.range = FALSE, "text-anchor"=anchor,transform=transform, tooltip = geom$tooltip, display=display)

  }
}


svg_text = function(svg, x,y, text,id=NULL, class="boxed-label",style=c(nlist("font-size"=font_size), extra.style), font_size=NULL, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE, math.label=TRUE,...) {
  restore.point("svg_text")

  text = sep.lines(text)
  if (length(text)>1) {
    text = multiline.tspans(text,x = x,y=y)
  } else if (math.label){
    text = latex.to.textspan(text)
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

bb.normalize.multi.arguments = function(li) {
  restore.point("bb.normalize.multi.arguments")
  len = sapply(li, function(el) {
    if (is(el,"formula") | is.expression(el) | is.call(el)) return(1)
    length(el)
  })
  if (all(len==1)) return(list(len=max(len),li=li))
  for (i in which(len==1)) {
    li[[i]] = replicate(n=max(len),li[[i]],simplify = FALSE)
  }
  return(list(len=max(len),li=li))
}
