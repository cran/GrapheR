camembert.var <-
function(varValue) {
  if (is.factor(Env$datas.GrapheR[,tclvalue(varValue)])==TRUE) {
    assign("noms0",levels(Env$datas.GrapheR[,tclvalue(varValue)]),pos=Env)
    assign("noms1",levels(Env$datas.GrapheR[,tclvalue(varValue)]),pos=Env)
    assign("niv.prop",paste(0:(nlevels(Env$datas.GrapheR[,tclvalue(varValue)])-1),collapse=" "),pos=Env)
  } else {
    noms=NULL
    for(i in 1:length(Env$datas.GrapheR[,tclvalue(varValue)])) {noms=c(noms,paste(Env$vocab[137,1],i))}
    assign("noms0",noms,pos=Env)
    assign("noms1",noms,pos=Env)
    assign("niv.prop",paste(0:(length(Env$datas.GrapheR[,tclvalue(varValue)])-1),collapse=" "),pos=Env)
  }
  assign("nb.niv0",length(strsplit(Env$niv.prop,split=" ")[[1]]),pos=Env)
  assign("nb.niv1",length(strsplit(Env$niv.prop,split=" ")[[1]]),pos=Env)
  assign("col.str1",gray.colors(Env$nb.niv1),pos=Env)
  assign("dens.motif",rep(0,Env$nb.niv1),pos=Env)
  assign("ang.motif",rep(0,Env$nb.niv1),pos=Env)
  assign("col.motif",rep("black",Env$nb.niv1),pos=Env)
}

