barres.type2 <-
function(fact2.choose,besideValue,erreur.lab,erreur.choose,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check) {
  tkconfigure(fact2.choose,state="readonly")
  if (as.numeric(tclvalue(besideValue))==0) {
    barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
  } else {
    barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
  }
}

