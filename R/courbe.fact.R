courbe.fact <-
function(factValue,niv.but,legende.lab,legende.check,nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose) {
  if (tclvalue(factValue)!=Env$vocab[48,1]) {
    assign("niv.fact","0",pos=Env)
    assign("noms1",levels(Env$datas.GrapheR[,tclvalue(factValue)])[1],pos=Env)
    assign("nb.niv1",1,pos=Env)
    assign("symb",rep(1,Env$nb.niv1),pos=Env)
    assign("col.str1",rep("black",Env$nb.niv1),pos=Env)
    assign("lignes",rep(Env$vocab[166,1],Env$nb.niv1),pos=Env)
    assign("traits",rep(Env$vocab[75,1],Env$nb.niv1),pos=Env)
    assign("ep.lignes",rep(1,Env$nb.niv1),pos=Env)
    tkconfigure(niv.but,state="normal")
    assign("legende.titre","",pos=Env)
    courbe.legende(act=TRUE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  } else {
    assign("niv.fact","",pos=Env)
    assign("noms1","",pos=Env)
    assign("nb.niv1",0,pos=Env)
    assign("symb",1,pos=Env)
    assign("col.str1","black",pos=Env)
    assign("lignes",Env$vocab[166,1],pos=Env)
    assign("traits",Env$vocab[75,1],pos=Env)
    assign("ep.lignes",1,pos=Env)
    courbe.legende(act=FALSE,legende.lab=legende.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  }
}

