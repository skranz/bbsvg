# inline svg links to create a self-contained svg image

examples.export.svg = function() {
  library(bbsvg)
  setwd("D:/libraries/bbsvg/")
  bb = bb_pane(xrang=c(0,1),yrange=c(0,1)) %>%
    bb_xmarker(x=0.5,y=0.5, latex="\\hat{x}")
  bb_to_pdf(bb,"bb.pdf")
  
  svg = bb_to_svg(bb)
  #cat(svg)
  writeLines(svg, "test.svg")
  library(rsvg)
  rsvg_png("test.svg", "test.png")
  rsvg_pdf("test.svg","test.pdf")
  view.bb(bb)
}


svg_to_pdf = function(svg.file = NULL,out.file=paste0(tools::file_path_sans_ext(svg.file),".pdf"),svg=merge.lines(readLines(svg.file))) {
  library(rsvg)
  restore.point("save_to_pdf")
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_pdf(charToRaw(svg),out.file)
}


svg_to_png = function(svg.file = NULL,out.file=paste0(tools::file_path_sans_ext(svg.file),".png"),svg = merge.lines(readLines(svg.file)), ...) {
  library(rsvg)
  if (length(svg))
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_png(charToRaw(svg),out.file)
}

svg_to_ps = function(svg.file = NULL,out.file=paste0(tools::file_path_sans_ext(svg.file),".ps"),svg=merge.lines(readLines(svg.file)), ...) {
  library(rsvg)  
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_ps(charToRaw(svg),out.file)
}


bb_to_pdf = function(bb, file,...) {
  library(rsvg)
  svg = bb_to_svg(bb)
  svg_to_pdf(svg=svg, out.file=file)
}

bb_to_png = function(bb, file,...) {
  svg = bb_to_svg(bb)
  svg_to_png(svg=svg, out.file=file)
}

bb_to_ps = function(bb, file,...) {
  svg = bb_to_svg(bb)
  svg_to_ps(svg=svg, out.file=file)
}
