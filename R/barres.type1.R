barres.type1 <-
function(fact2.choose,erreur.choose,erreur.lab,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check) {
  tkconfigure(fact2.choose,state="readonly")
  barres.erreur.types(type="som",erreur.choose=erreur.choose)
  barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
}

