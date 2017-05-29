
view.bb = function(bb, latexsvg=isTRUE(bb$use.latex), launch.browser = rstudio::viewer,...) {
  restore.point("view.bb")
  library(shinyEvents)
  svg = bb_to_svg(bb,id = "mysvg", return.svg.object = TRUE)
  
  hsvg = svg_string(svg)
  if (latexsvg) {
    hsvg = latexsvg::latexsvg(hsvg)
  }

  app=eventsApp()
  
  app$ui = fluidPage(
    div(style="cursor: crosshair;",HTML(hsvg))
  )
  svgClickHandler("mysvg", function(x,y,...) {
    args = list(...)
    restore.point("svg_click")
    dom = range.to.domain(x=x,y=y,svg=svg)
    cat(paste0("\n range  x=",x,",y=",y))
    cat(paste0("\n domain x=",round(dom$x,2),",y=",round(dom$y,2)))
    msg = paste0('\tbb_text(x=',round(dom$x,2),',y=',round(dom$y,2),',label="")  %>%')
    writeClipboard(msg)
    cat(paste0('\n',msg))
      
  })
  
  viewApp(app,launch.browser = launch.browser)
}

