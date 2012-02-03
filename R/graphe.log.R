graphe.log <-
function() {
  result<-if (tclvalue(Env$l.var$log.axehor)==1) {
    if (tclvalue(Env$l.var$log.axever)==1) {
	"xy"
    } else {
	"x"
    }
  } else {
    if (tclvalue(Env$l.var$log.axever)==1) {
	"y"
    } else {
	""
    }
  }
  return(result)
}
