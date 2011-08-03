fermer.GrapheR <-
function() {
  tkdestroy(Env$Fen)
  rm(Env,pos="package:GrapheR")
}

