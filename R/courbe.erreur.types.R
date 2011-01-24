courbe.erreur.types <-
function(type,erreur.choose) {
  tkconfigure(erreur.choose,state="normal")
  tkdelete(erreur.choose,0,"end")
  if (type=="prop") {
    tkconfigure(erreur.choose,values=c(Env$vocab[118,1],Env$vocab[121,1]))
  } else if (type=="moy") {
    tkconfigure(erreur.choose,values=c(Env$vocab[118,1],Env$vocab[119,1],Env$vocab[120,1],Env$vocab[121,1]))
  }
  tkconfigure(erreur.choose,state=if (type=="som") {"disabled"} else {"readonly"})
}

