code.graph <-
function() {
  cat("# Graph\n\n")
  if (Env$l.var$ecran=="H") {
    code.graph.hist()
  } else if (Env$l.var$ecran=="M") {
    code.graph.moust()
  } else if (Env$l.var$ecran=="B") {
    code.graph.barres()
  } else if (Env$l.var$ecran=="Ca") {
    code.graph.cam()
  } else if (Env$l.var$ecran=="Co") {
    code.graph.courbe()
  } else if (Env$l.var$ecran=="N") {
    code.graph.nuage()
  }
}
