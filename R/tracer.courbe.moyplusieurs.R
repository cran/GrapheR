tracer.courbe.moyplusieurs <-
function() {
  varX<-Env$dataset[,tclvalue(Env$l.var$varX)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  varY<-Env$dataset[,tclvalue(Env$l.var$varY)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  facteur<-Env$dataset[,tclvalue(Env$l.var$facteur1)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  valeurs<-tapply(varY,list(facteur,varX),function(x) mean(x,na.rm=TRUE))
  erreurs<-graphe.erreurs.calculer(variable=varY,facteur1=factor(varX),facteur2=facteur)
  limites<-tracer.courbe.limites(varX=varX,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  Env$l.code$x.inf<-x.inf<-limites$xinf
  Env$l.code$x.sup<-x.sup<-limites$xsup
  Env$l.code$y.inf<-y.inf<-limites$yinf
  Env$l.code$y.sup<-y.sup<-limites$ysup
  symboles<-graphe.symboles(num=Env$l.var$symboleB)
  lignes<-type.ligne(type=Env$l.var$type.courbeB)
  traits<-type.trait(type=Env$l.var$trait2)
  plot(valeurs[1,]~as.numeric(colnames(valeurs)),axes=FALSE,ann=FALSE,xlim=c(x.inf,x.sup),ylim=c(y.inf,y.sup),log=graphe.log(),
    col=Env$l.var$couleur2B[1],pch=symboles[1],cex=Env$l.var$taille.ptsB[1],type=lignes[1],lty=traits[1],
    lwd=Env$l.var$epaisseur2[1])
  for (i in 2:length(Env$l.var$noms1)) {
    lines(as.numeric(colnames(valeurs)),valeurs[i,],col=Env$l.var$couleur2B[i],pch=symboles[i],cex=Env$l.var$taille.ptsB[i],
    type=lignes[i],lty=traits[i],lwd=Env$l.var$epaisseur2[i])
  }
  if (nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    for (i in 1:length(Env$l.var$noms1)) {
	graphe.erreurs.tracer(abscisses=as.numeric(colnames(valeurs)),valeurs=valeurs[i,],erreur.inf=erreurs$erreur.inf[i,],
	  erreur.sup=erreurs$erreur.sup[i,],couleur=Env$l.var$couleur2B[i],amplitude=x.sup-x.inf)
    }
  }
  if (tclvalue(Env$l.var$legende)==1) {
    graphe.legende(type="courbe",symboles=symboles,lignes=lignes,traits=traits)
  }
  graphe.titre()
  graphe.axes()
  graphe.box()
}

