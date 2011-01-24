nuage.fact <-
function(factValue,niv.but,legende.check.lab,legende.check,nom.legende.lab,nom.legende.but,legende.pos.lab,legende.pos.choose) {
  if (tclvalue(factValue)!=Env$vocab[48,1]) {
    assign("noms1",levels(Env$datas.GrapheR[,tclvalue(factValue)])[1],pos=Env)
    assign("niv.fact","0",pos=Env)
    assign("nb.niv1",1,pos=Env)
    assign("symb",rep(1,Env$nb.niv1),pos=Env)
    assign("col.str1",rep("black",Env$nb.niv1),pos=Env)
    assign("droite",rep("aucune",Env$nb.niv1),pos=Env)
    assign("regression",rep("mc",Env$nb.niv1),pos=Env)
    assign("hor",rep("",Env$nb.niv1),pos=Env)
    assign("ver",rep("",Env$nb.niv1),pos=Env)
    assign("coeffa",rep("",Env$nb.niv1),pos=Env)
    assign("coeffb",rep("",Env$nb.niv1),pos=Env)
    assign("traits",rep(Env$vocab[75,1],Env$nb.niv1),pos=Env)
    assign("ep.lignes",rep("1",Env$nb.niv1),pos=Env)
    tkconfigure(niv.but,state="normal")
    nuage.legende(act=TRUE,legende.check.lab=legende.check.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  } else {
    assign("niv.fact","",pos=Env)
    tkconfigure(niv.but,state="disabled")
    nuage.legende(act=FALSE,legende.check.lab=legende.check.lab,legende.check=legende.check,nom.legende.lab=nom.legende.lab,nom.legende.but=nom.legende.but,legende.pos.lab=legende.pos.lab,legende.pos.choose=legende.pos.choose)
  }
}

