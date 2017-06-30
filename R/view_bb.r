
view.bb = function(bb, latexsvg=isTRUE(bb$use.latex), launch.browser = rstudio::viewer,...) {
  restore.point("view.bb")
  library(shinyEvents)
  svg = bb_to_svg(bb,id = "mysvg", return.svg.object = TRUE)
  
  hsvg = svg_string(svg)
  if (latexsvg) {
    hsvg = latexsvg::latexsvg(hsvg)
  }

  app=eventsApp()
  
  app$prev.xy = NULL
  
  app$ui = fluidPage(
    div(style="cursor: crosshair;",HTML(hsvg))
  )
  svgClickHandler("mysvg", function(x,y,app=getApp(),...) {
    args = list(...)
    restore.point("svg_click")
    dom = range.to.domain(x=x,y=y,svg=svg)
    cat(paste0("\n range  x=",x,",y=",y))
    cat(paste0("\n domain x=",round(dom$x,2),",y=",round(dom$y,2)))
    msg = paste0('\tbb_text(x=',round(dom$x,2),',y=',round(dom$y,2),',label="", align="left")  %>%')
    writeClipboard(msg)
    cat(paste0('\n',msg))
    
    msg = paste0('\tbb_point(x=',round(dom$x,2),',y=',round(dom$y,2),', r=3)  %>%')
    cat(paste0('\n',msg))

    prev.xy = app$prev.xy
    if (!is.null(prev.xy)) {
      msg = paste0('\tbb_segment(x1=',round(prev.xy[1],2),',y1=',round(prev.xy[2],2),',x2=',round(dom$x,2),',y2=',round(dom$y,2),', linetype="solid",color="black")  %>%')
      cat(paste0('\n',msg))
    }
    app$prev.xy = c(x=dom$x,y=dom$y)
  })
  
  viewApp(app,launch.browser = launch.browser)
}

