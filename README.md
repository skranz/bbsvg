Author: Sebastian Kranz (Ulm University)

`bbsvg` is an R package that shall help to make svg graphics similar to graphs drawn on blackboards in my economics classes.

The functions are not yet documented, but this README illustrates the package with examples.

```r
bb = bb_pane(xrange=c(0,110),yrange=c(0,600), show.ticks=FALSE, org.width = 480,  org.height=320) %>%
  bb_xaxis(label = "GW") %>%
  bb_yaxis(label = "Euro / MWh", labelpos="top") %>%
  
  bb_area_rect(x1=0,x2=80,y1=0,y2=200, fill="#aa6666") %>%
  bb_text(x=40, y=100, label="Coal", align="center") %>%  
  
  bb_area_rect(x1=80,x2=100,y1=0,y2=500, fill="#8888aa") %>%
  bb_text(x=90, y=100, label="Gas", align="center")  %>%
  
  bb_vline(x=100) %>%
  bb_text(x=100,align = "center",y=640,label="Demand") %>%
  
  bb_segment(y=500,x1=0,x2=100,linetype = "dashed") %>%
  bb_ytick(y=500, label="p")

