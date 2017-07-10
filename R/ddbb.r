
ddbb.examples = function() {
library(ddsim)
dd = ddsim() %>%
  dd_param(I=10,c0=0,c1=0.9) %>%
  dd_init_steady_state(Y) %>%
  dd_explicit(
    EY = lag_Y,
    C = c0 + c1*EY,
    Y = C + I
   ) %>%
  dd_expost(S = Y-C, S_PLAN=EY-C, "Geplante Sparquote" = (1-c1)*100, "Reale Sparquote" = 100*S / Y) %>%
  dd_shock(c1=0.8, start=3, length=Inf, name="Sparschock") %>%
  dd_run(T=20)
  sim = dd_data(dd)
select(sim[1:5,],"t","Y","C","I")
show = c("Y","C","I")

bb = dd_bbplot(dd,sim,show, rows=1:20,ylim=c(0,110),lwd=3, margins=c("right"=60)) %>% bb_xaxis(label="Periode",labelpos = "center") %>% bb_yaxis(ticks=c(0,25,50,75,100)) %>% bb_ymarker(y=50, label="") %>% bb_series_tooltip_bars(xname="Periode")
  

view.bb(bb)
}


dd_bbplot = function(dd, dat=dd_data(dd), cols=dd$var.names, main="",xlab=dd$time.var,ylab="", shocks=dd$shocks, show.shocks = TRUE, rows=1:NROW(dat),xlim=range(rows),ylim=NULL,colors=colors_bb_series(),show.ticks=TRUE,lwd=2,labels=cols,draw.points=TRUE, draw.line=TRUE, r=3, auto.labels = TRUE,  ...) {
  restore.point("dd_bbplot")
  #dat$t = t.to.date(dat$t)
  library(bbsvg)
  
  if (is.null(ylim)) {
    ylim=range(dat[,cols])
  }
  dat = dat[rows,,drop=FALSE]
  bb = bb_pane(data=dat,xrange=xlim,yrange=ylim,show.ticks = show.ticks, tooltip.bar = TRUE, ...)

  for (i in seq_along(cols)) {
    col = cols[i]
    bb = bb_series(bb,xvar = "t",yvar=col, name=col, color=colors[i],lwd=lwd, draw.points=draw.points, draw.line=draw.line, r=r)
    if (!is.null(labels) & auto.labels)
      bb = bb %>% bb_text(label=labels[i], x=max(xlim),y=dat[[col]][max(rows)], x.offset=10, color=colors[i])
  }
  if (show.shocks) {
    bb = dd_bb_annotate_shocks(bb, shocks, rows=rows, dd=dd, T=max(xlim))
  }
  bb
}
dd_bb_annotate_shocks = function(bb, shocks=dd[["shocks"]],dd=NULL, T = first.non.null(dd$T,NROW(bb$data[[1]])), rows = 1:T) {
  restore.point("dd_bb_annotate_shocks")
  #shock = shocks[[1]]
  for (shock in shocks) {
    if (shock$start > max(rows)) next
    #start = t.to.date(shock$start)
    name = shock$name
    
    bb = bb %>%
      bb_period(from=shock$start,to=min(shock$end,T),label = name,alpha = 0.15, font_size=12)
  }
  bb
} 
