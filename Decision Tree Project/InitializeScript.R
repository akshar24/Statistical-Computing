library(rstudioapi)
#Code to get the default working directory where all data files are
getScriptLoc <- function(){
  context <- getSourceEditorContext()
  wd <- dirname(context$path)
  return(wd)
}
getWorkingDirectory <- function(){
  wd <- getScriptLoc()
  wd <- paste(wd, "DataFiles",sep ="/")
  return(wd)
}