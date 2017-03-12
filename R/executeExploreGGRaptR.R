#' Perform the required operations for displaying interactive plot generator.
#' 
#' Time-stamp: <2016-09-19 17:12:21 Graham Williams>
#' 
executeExploreGGRaptR <- function(df_name, df)
{
  # Check prerequisite packages.

  if (!packageIsAvailable("ggraptR", 
                          Rtxt("interactively generate ggplot2 graphics"))) return(FALSE)
  
  # 160630 Currently there is no way to pass a default dataset.
  
  startLog(Rtxt("Display interactive plot builder."))

  df_file <- 'ggraptr_df.rds'
  saveRDS(df, file=df_file)
  r_expr <- sprintf(
    '%s <- readRDS(\'%s\');file.remove(\'%s\');ggraptR::ggraptR(%s, port=5002)', 
    df_name, df_file, df_file, df_name)
  
  appendLog("Initiate the ggraptR application in a browser", r_expr)
  
  system(sprintf('R -q --vanilla -e "%s"', r_expr), wait=F, intern=F)
  return()
}
