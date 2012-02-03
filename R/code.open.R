code.open <-
function() {
  sink(file=file.path(Env$l.code$folder,paste(paste("GrapheR",paste(strsplit(as.character(Sys.Date()),split="-")[[1]],collapse="."),
    sep="-"),".R",sep=""),fsep=.Platform$file.sep),append=TRUE)
  if (Env$l.code$graphsnb==0) {
    cat("#----------------------------------------\n")
    cat(paste("# GrapheR - session of ",paste(strsplit(as.character(Sys.Date()),split="-")[[1]],collapse="."),"\n",sep=""))
    cat("#----------------------------------------")
  }
  code.graphtype()
  code.data()
  code.graph()
  sink(NULL)
}
