tracer.courbe.moyun <-
function() {
  if(nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
    varX<-Env$dataset[,tclvalue(Env$l.var$varX)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]]
    varY<-Env$dataset[,tclvalue(Env$l.var$varY)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]]
  } else {
    varX<-Env$dataset[,tclvalue(Env$l.var$varX)]
    varY<-Env$dataset[,tclvalue(Env$l.var$varY)]
  }
  valeurs<-tapply(varY,varX,function(x) mean(x,na.rm=TRUE))
  erreurs<-graphe.erreurs.calculer(variable=varY,facteur1=factor(varX))
  limites<-tracer.courbe.limites(varX=varX,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  x.inf<-limites$xinf
  x.sup<-limites$xsup
  y.inf<-limites$yinf
  y.sup<-limites$ysup
  symbole<-graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA)))
  plot(valeurs~as.numeric(names(valeurs)),axes=FALSE,ann=FALSE,xlim=c(x.inf,x.sup),ylim=c(y.inf,y.sup),log=graphe.log(),
    col=tclvalue(Env$l.var$couleur2A),pch=symbole,cex=as.numeric(tclvalue(Env$l.var$taille.ptsA)),
    type=type.ligne(type=tclvalue(Env$l.var$type.courbeA)),lty=type.trait(type=tclvalue(Env$l.var$trait1)),
    lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)))
  if (nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    graphe.erreurs.tracer(abscisses=as.numeric(names(valeurs)),valeurs=valeurs,erreur.inf=erreurs$erreur.inf,
	erreur.sup=erreurs$erreur.sup,alert=erreurs$alert,couleur=tclvalue(Env$l.var$couleur2A),amplitude=x.sup-x.inf)
  }
  graphe.titre()
  graphe.axes()
  graphe.box()
}

