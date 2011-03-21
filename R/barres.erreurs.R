barres.erreurs <-
function(graphe,val,err.inf,err.sup,err.col,err.seg) {
  segments(graphe,val-err.inf,graphe,val+err.sup,col=err.col)
  if (err.seg==1) {
    segments(graphe-0.1,val-err.inf,graphe+0.1,val-err.inf,col=err.col)
    segments(graphe-0.1,val+err.sup,graphe+0.1,val+err.sup,col=err.col)
  }
}

