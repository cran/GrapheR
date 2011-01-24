barres.type3 <-
function(legende.lab,legende.check,nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose,besideValue,erreur.lab,erreur.choose,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check) {
  if (nchar(Env$niv.prop)>1) {
    barres.legende(act=TRUE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
    if (as.numeric(tclvalue(besideValue))==0) {
	barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
    } else {
	barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
    }
  } else {
    barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
    barres.legende(act=FALSE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  }
}

