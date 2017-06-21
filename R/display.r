
init.geom.display = function(geom, display) {
  if (identical(display,"whisker")) {
    return(paste0("{{display_",geom$id,"}}"))
  }
  display
}
