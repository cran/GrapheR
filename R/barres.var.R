barres.var <-
function(varValue,fact2Value,typeValue,fact2.lab,fact2.choose,rb1,rb2,rb3,prop.but,erreur.choose,legende.lab,legende.check,nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose) {
  if (is.factor(Env$datas.GrapheR[,tclvalue(varValue)])==TRUE) {
    assign("nb.niv2",1,pos=Env)
    assign("niv.prop","0",pos=Env)
    assign("col.str1","white",pos=Env)
    assign("col.str2","black",pos=Env)
    assign("dens.motif",0,pos=Env)
    assign("ang.motif",0,pos=Env)
    assign("col.motif","black",pos=Env)
    barres.type(type="fact",typeValue=typeValue,fact2.lab=fact2.lab,fact2.choose=fact2.choose,rb1=rb1,rb2=rb2,rb3=rb3,prop.but=prop.but)
    barres.erreur.types(type="prop",erreur.choose=erreur.choose)
    barres.legende(act=FALSE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  } else {
    assign("niv.prop","0",pos=Env)
    assign("nb.niv2",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {nlevels(Env$datas.GrapheR[,tclvalue(fact2Value)])} else {0},pos=Env)
    assign("noms2",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {levels(Env$datas.GrapheR[,tclvalue(fact2Value)])} else {""},pos=Env)
    assign("col.str1",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {gray.colors(Env$nb.niv2)} else {"white"},pos=Env)
    assign("col.str2",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {rep("black",Env$nb.niv2)} else {"black"},pos=Env)
    assign("dens.motif",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {rep(0,Env$nb.niv2)} else {0},pos=Env)
    assign("ang.motif",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {rep(0,Env$nb.niv2)} else {0},pos=Env)
    assign("col.motif",if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {rep("black",Env$nb.niv2)} else {"black"},pos=Env)
    barres.type(type="num",typeValue=typeValue,fact2.lab=fact2.lab,fact2.choose=fact2.choose,rb1=rb1,rb2=rb2,rb3=rb3,prop.but=prop.but)
    barres.erreur.types(type="moy",erreur.choose=erreur.choose)
    if (nchar(tclvalue(fact2Value))==0 | tclvalue(fact2Value)==Env$vocab[48,1]) {
	barres.legende(act=FALSE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
    } else {
	barres.legende(act=TRUE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
    }
  }
}

