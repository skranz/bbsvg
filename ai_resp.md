An excellent observation! The issue is caused by how `svg_text` handles multi-line labels.

In the original code for `svg_text`, there is an `if ... else if` block that only applied the `latex.to.textspan` conversion (which translates `S_{0}` to proper `<tspan>` subscripts) if the text did **not** have multiple lines (`length(text) == 1`). If the label had line breaks (`\n`), it fell into the first `if` branch which split the lines and bypassed the math label conversion completely. 

Additionally, the original code had a bug where the `x` and `y` coordinates sent to `multiline.tspans` were unscaled domain coordinates instead of the scaled pixel range coordinates when `to.range=TRUE` was requested.

Here is the modification to fix both issues. We now apply the latex math transformation independently to each line using `sapply` before creating the multi-line `tspan` elements, and we ensure `multiline.tspans` always receives the scaled `rp` coordinates.

!MODIFICATION svg_text bb_label.r
scope = "function"
file = "bb_label.r"
function_name = "svg_text"
description = "Apply `latex.to.textspan` to multiline labels and use properly scaled range coordinates for `multiline.tspans`."
---
```r
svg_text = function(svg, x,y, text,id=NULL, class="boxed-label",style=c(nlist("font-size"=font_size), extra.style), font_size=NULL, extra.style=list(), level=1, tooltip=NULL, to.range=TRUE, math.label=TRUE,...) {
  restore.point("svg_text")

  rp = domain.to.range(x=x,y=y,svg=svg, to.range=to.range)

  text = sep.lines(text)
  
  if (math.label) {
    text = sapply(text, latex.to.textspan, USE.NAMES = FALSE)
  }

  if (length(text)>1) {
    if (is.null(font_size)) {
      text = multiline.tspans(text, x = rp$x, y = rp$y)
    } else {
      text = multiline.tspans(text, x = rp$x, y = rp$y, font_size = font_size)
    }
  } else {
    text = text[1]
  }

  el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,class,style,...), tooltip=tooltip, inner=text)
  #el = svg_tag("text", nlist(x=rp$x,y=rp$y,id,style,...), tooltip=tooltip, inner=text)
  svg_add(svg,el,id,level=level)
}
```
!END_MODIFICATION svg_text bb_label.r