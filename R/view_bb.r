
disable.view.bb = function(disable=TRUE) {
  options(disable_view_bb = disable)
}

view.bb = function(bb, latexsvg=isTRUE(bb$use.latex), launch.browser = rstudioapi::viewer, skip = isTRUE(getOption("disable_view_bb")),...) {
  restore.point("view.bb")
  if (skip) return()
  library(shinyEvents)
  svg = bb_to_svg(bb,id = "mysvg", return.svg.object = TRUE)

  www = system.file("www", package="bbsvg")  
  addResourcePath(prefix = "bbsvg",directoryPath = www)
  
  hsvg = svg_string(svg)
  if (latexsvg) {
    hsvg = latexsvg::latexsvg(hsvg)
  }

  app=eventsApp()
  
  app$prev.xy = NULL
  
  app$ui = fluidPage(
    tags$head(tags$script(src="bbsvg/velocity.min.js")),
    div(style="cursor: crosshair;",HTML(hsvg))
  )
  svgClickHandler("mysvg", function(x,y,app=getApp(),...) {
    args = list(...)
    restore.point("svg_click")
    dom = range.to.domain(x=x,y=y,svg=svg)
    cat(paste0("\n range  x=",x,",y=",y))
    
    # compute number of rounding digits
    dw = diff(svg$dr$domain$x)
    x.round = pmax(round(-log(dw / 1000,base = 10)),1)
    dh = diff(svg$dr$domain$y)
    y.round = pmax(round(-log(dh / 1000,base = 10)),1)
    
    cat(paste0("\n domain x=",round(dom$x,x.round),",y=",round(dom$y,y.round)))
    msg = paste0('\tbb_text(x=',round(dom$x,x.round),',y=',round(dom$y,y.round),',label="", align="left", color=NULL)  %>%')
    writeClipboard(msg)
    cat(paste0('\n',msg))
    
    msg = paste0('\tbb_point(x=',round(dom$x,x.round),',y=',round(dom$y,y.round),', r=3)  %>%')
    cat(paste0('\n',msg))

    prev.xy = app$prev.xy
    if (!is.null(prev.xy)) {
      msg = paste0('\tbb_arrow(x1=',round(prev.xy[1],x.round),',y1=',round(prev.xy[2],y.round),',x2=',round(dom$x,x.round),',y2=',round(dom$y,y.round),', linetype="solid",color="black")  %>%')
      cat(paste0('\n',msg))
    }
    app$prev.xy = c(x=dom$x,y=dom$y)
  })
  
  viewApp(app,launch.browser = launch.browser)
}

