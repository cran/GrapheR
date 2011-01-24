barres.fact2 <-
function(fact2Value,besideValue,typeValue,legende.lab,legende.check,nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose,erreur.lab,erreur.choose,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check) {
  if (tclvalue(fact2Value)==Env$vocab[48,1]) {
    assign("col.str1","white",pos=Env)
    assign("col.str2","black",pos=Env)
    assign("dens.motif",0,pos=Env)
    assign("ang.motif",0,pos=Env)
    assign("col.motif","black",pos=Env)
    barres.legende(act=FALSE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  } else {
    assign("noms2",levels(Env$datas.GrapheR[,tclvalue(fact2Value)]),pos=Env)
    assign("nb.niv2",nlevels(Env$datas.GrapheR[,tclvalue(fact2Value)]),pos=Env)
    assign("legende.titre",c(""),pos=Env)
    assign("col.str1",gray.colors(Env$nb.niv2),pos=Env)
    assign("col.str2",rep("black",Env$nb.niv2),pos=Env)
    assign("dens.motif",rep(0,Env$nb.niv2),pos=Env)
    assign("ang.motif",rep(0,Env$nb.niv2),pos=Env)
    assign("col.motif",rep("black",Env$nb.niv2),pos=Env)
    barres.legende(act=TRUE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
    if (tclvalue(besideValue)==0 & tclvalue(typeValue)!="som") {
	barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
    } else {
	barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
    }
  }
}

