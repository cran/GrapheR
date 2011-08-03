language <-
function(lang=NULL) {
  if (!is.null(lang)) {
    Env$lang<-lang
  } else {
    Env$lang<-read.table(file.path(.path.package("GrapheR"),"lang","Language.txt",fsep=.Platform$file.sep))[1,1]
  }
}

