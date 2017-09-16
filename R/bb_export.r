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


svg_to_pdf = function(svg=readLines(svg.file),svg.file = NULL,pdf.file=paste0(tools::file_path_sans_ext(svg.file),".pdf"), ...) {
  library(rsvg)
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_pdf(charToRaw(svg),pdf.file)
}


svg_to_png = function(svg=readLines(svg.file),svg.file = NULL,png.file=paste0(tools::file_path_sans_ext(svg.file),".png"), ...) {
  library(rsvg)
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_png(charToRaw(svg),png.file)
}

svg_to_ps = function(svg=readLines(svg.file),svg.file = NULL,ps.file=paste0(tools::file_path_sans_ext(svg.file),".ps"), ...) {
  library(rsvg)  
  svg = gsub('href="','xlink:href="',svg, fixed=TRUE)
  svg = gsub('xlink:xlink:href="','xlink:href="',svg, fixed=TRUE)
  rsvg_ps(charToRaw(svg),ps.file)
}


bb_to_pdf = function(bb, file,...) {
  library(rsvg)
  svg = bb_to_svg(bb)
  svg_to_pdf(svg, pdf.file=file)
}

bb_to_png = function(bb, file,...) {
  svg = bb_to_svg(bb)
  svg_to_png(svg, png.file=file)
}

bb_to_ps = function(bb, file,...) {
  svg = bb_to_svg(bb)
  svg_to_ps(svg, ps.file=file)
}
