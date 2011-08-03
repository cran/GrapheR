tracer.courbe.propplusieurs <-
function() {
  varX<-Env$dataset[,tclvalue(Env$l.var$varX.prop)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  varY<-Env$dataset[,tclvalue(Env$l.var$proportions)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  facteur<-Env$dataset[,tclvalue(Env$l.var$facteur1)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  valeurs<-matrix(0,nrow=nlevels(facteur),ncol=nlevels(factor(varX)))
  for (i in 1:nlevels(factor(varX))) {
    for (j in 1:nlevels(facteur)) {
	valeurs[j,i]<-length(varY[varY==tclvalue(Env$l.var$prop.niveaux) & varX==levels(factor(varX))[i] & facteur==levels(facteur)[j]])/length(na.omit(varY[varX==levels(factor(varX))[i] & facteur==levels(facteur)[j]]))
    }
  }
  erreurs<-graphe.erreurs.calculer2(varX=factor(varX),varY=varY,niveau=tclvalue(Env$l.var$prop.niveaux),valeurs=valeurs,facteur=facteur)
  limites<-tracer.courbe.limites(varX=varX,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  x.inf<-limites$xinf
  x.sup<-limites$xsup
  y.inf<-limites$yinf
  y.sup<-limites$ysup
  symboles<-graphe.symboles(num=Env$l.var$symboleB)
  lignes<-type.ligne(type=Env$l.var$type.courbeB)
  traits<-type.trait(type=Env$l.var$trait2)
  plot(valeurs[1,]~as.numeric(as.character(levels(factor(varX)))),axes=FALSE,ann=FALSE,xlim=c(x.inf,x.sup),ylim=c(y.inf,y.sup),log=graphe.log(),
    col=Env$l.var$couleur2B[1],pch=symboles[1],cex=Env$l.var$taille.ptsB[1],type=lignes[1],lty=traits[1],
    lwd=Env$l.var$epaisseur2[1])
  for (i in 2:length(Env$l.var$noms1)) {
    lines(as.numeric(as.character(levels(factor(varX)))),valeurs[i,],col=Env$l.var$couleur2B[i],pch=symboles[i],cex=Env$l.var$taille.ptsB[i],
    type=lignes[i],lty=traits[i],lwd=Env$l.var$epaisseur2[i])
  }
  if (nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    for (i in 1:length(Env$l.var$noms1)) {
	graphe.erreurs.tracer(abscisses=as.numeric(as.character(levels(factor(varX)))),valeurs=valeurs[i,],erreur.inf=erreurs$erreur.inf[i,],
	  erreur.sup=erreurs$erreur.sup[i,],alert=erreurs$alert,couleur=Env$l.var$couleur2B[i],amplitude=x.sup-x.inf)
    }
  }
  if (tclvalue(Env$l.var$legende)==1) {
    graphe.legende(type="courbe",symboles=symboles,lignes=lignes,traits=traits)
  }
  graphe.titre()
  graphe.axes()
  graphe.box()
}

