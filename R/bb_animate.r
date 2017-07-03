examples.bb.animate = function() {
  library(bbsvg)
  bb = bb_pane(id = "mysvg",xrange=c(0,10),yrange=c(0,10),org.width = 200,  org.height=200) %>%
  bb_point(id="mypoint", x=2,y=2)

  svg = bb_to_svg(bb)
  writeClipboard(svg)
  view.bb(bb)

  
}