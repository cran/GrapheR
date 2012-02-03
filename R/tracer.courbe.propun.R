tracer.courbe.propun <-
function() {
  if(nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
    varX<-Env$dataset[,tclvalue(Env$l.var$varX.prop)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]]
    varY<-Env$dataset[,tclvalue(Env$l.var$proportions)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]]
  } else {
    varX<-Env$dataset[,tclvalue(Env$l.var$varX.prop)]
    varY<-Env$dataset[,tclvalue(Env$l.var$proportions)]
  }
  valeurs<-integer(nlevels(factor(varX)))
  for (i in 1:nlevels(factor(varX))) {
    valeurs[i]<-length(varY[varY==tclvalue(Env$l.var$prop.niveaux) & factor(varX)==levels(factor(varX))[i]])/length(na.omit(varY[factor(varX)==levels(factor(varX))[i]]))
  }
  erreurs<-graphe.erreurs.calculer2(varX=factor(varX),varY=varY,niveau=tclvalue(Env$l.var$prop.niveaux),valeurs=valeurs)
  limites<-tracer.courbe.limites(varX=varX,valeurs=valeurs,erreur.inf=erreurs$erreur.inf,erreur.sup=erreurs$erreur.sup)
  Env$l.code$x.inf<-x.inf<-limites$xinf
  Env$l.code$x.sup<-x.sup<-limites$xsup
  Env$l.code$y.inf<-y.inf<-limites$yinf
  Env$l.code$y.sup<-y.sup<-limites$ysup
  symbole<-graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA)))
  plot(valeurs~as.numeric(as.character(levels(factor(varX)))),axes=FALSE,ann=FALSE,xlim=c(x.inf,x.sup),ylim=c(y.inf,y.sup),log=graphe.log(),
    col=tclvalue(Env$l.var$couleur2A),pch=symbole,cex=as.numeric(tclvalue(Env$l.var$taille.ptsA)),
    type=type.ligne(type=tclvalue(Env$l.var$type.courbeA)),lty=type.trait(type=tclvalue(Env$l.var$trait1)),
    lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)))
  if (nchar(tclvalue(Env$l.var$erreur))>0 & tclvalue(Env$l.var$erreur)!=Env$voc[95,1]) {
    graphe.erreurs.tracer(abscisses=as.numeric(as.character(levels(factor(varX)))),valeurs=valeurs,erreur.inf=erreurs$erreur.inf,
	erreur.sup=erreurs$erreur.sup,couleur=tclvalue(Env$l.var$couleur2A),amplitude=x.sup-x.inf)
  }
  graphe.titre()
  graphe.axes()
  graphe.box()
}
