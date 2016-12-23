#' Perform the required operations for displaying interactive plot generator.
#' 
#' Time-stamp: <2016-09-19 17:12:21 Graham Williams>
#' 
executeExploreGGRaptR <- function(dataset.name)
{
  # Check prerequisite packages.

  if (!packageIsAvailable("ggraptR", 
                          Rtxt("interactively generate ggplot2 graphics"))) return(FALSE)
  
  # 160630 Currently there is no way to pass a default dataset.
  
  startLog(Rtxt("Display interactive plot builder."))

  cmd <- sprintf('ggraptR::ggraptR(%s, port=5002)', dataset.name)
  appendLog("Initiate the ggraptR application in a browser", cmd)
  system(sprintf('R -q -e "%s"', cmd), wait=F, intern=F)  # debug: wait=intern=T + print
  
  return()
}
