#' Perform the required operations for displaying interactive plot generator.
#' 
#' Time-stamp: <2016-07-24 16:38:40 Graham Williams>
#' 
executeExploreGGRaptR <- function(dataset.name)
{
  # 160630 Currently there is no way to pass a default dataset.
  
  startLog(Rtxt("Display interactive plot builder."))

  cmd <- 'ggraptR::ggraptR(rattle.dataset.for.ggraptR)'
  appendLog("Initiate the ggraptR applicaiton in a browser", cmd)
  eval(parse(text=cmd))  # NEED TO DETATCH?
  return()
  
  varsi <- getVariableIndicies(vars)
  
 # v1 <- theWidget("pairs_color_combobox")$getActiveText()
  v1 <- target
  if (is.null(v1) || v1 == " ")
  {
    colorStr<-'' # No color selected.
  }
  else
  {
    colorStr<-sprintf('mapping=ggplot2::aes(colour=%s, alpha=0.5),',v1)
  }

  plot.cmd <- paste0('GGally::ggpairs(', dataset, ',\n',
                     '        columns=c(',
                     paste(varsi, collapse=','), '),\n', 
                     if (colorStr!="") paste0('        ', colorStr, "\n"),
                     '        diag=list(continuous="density",\n',
                     '                  discrete="bar"),\n',
                     '        upper=list(continuous="cor",\n',
                     '                   combo="box",\n',
                     '                   discrete="ratio"),\n',
                     '        lower=list(continuous="points",\n',
                     '                   combo="denstrip",\n',
                     '                   discrete="facetbar"))',
                     ' +\n  ggplot2::theme(panel.grid.major=ggplot2::element_blank())')
  # When this next blank theme is included we get bad plots???? Some
  # problem with colour.
  #
  #                         '         panel.grid.minor=ggplot2::element_blank())')
      
  appendLibLog(Rtxt("Use GGally's ggpairs() to do the hard work."), plot.cmd)
  newPlot()
  eval(parse(text=sprintf("suppressMessages(print(%s))", plot.cmd)))
}
