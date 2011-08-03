tracer.nuage.plusieurs <-
function() {
  varX<-Env$dataset[,tclvalue(Env$l.var$varX)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  varY<-Env$dataset[,tclvalue(Env$l.var$varY)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  facteur<-Env$dataset[,tclvalue(Env$l.var$facteur1)][Env$dataset[,tclvalue(Env$l.var$facteur1)]%in%levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]]
  niveaux<-levels(facteur)[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]
  limites<-tracer.nuage.limites(varX=varX,varY=varY)
  x.inf<-limites$xinf
  x.sup<-limites$xsup
  y.inf<-limites$yinf
  y.sup<-limites$ysup
  symboles<-graphe.symboles(num=Env$l.var$symboleB)
  plot(varY[facteur==niveaux[1]]~varX[facteur==niveaux[1]],axes=FALSE,ann=FALSE,xlim=c(x.inf,x.sup),ylim=c(y.inf,y.sup),
    log=graphe.log(),col=Env$l.var$couleur2B[1],pch=symboles[1],cex=Env$l.var$taille.ptsB[1])
  for (i in 2:length(niveaux)) {
    points(varY[facteur==niveaux[i]]~varX[facteur==niveaux[i]],col=Env$l.var$couleur2B[i],pch=symboles[i],
	cex=Env$l.var$taille.ptsB[i])
  }
  if (any(nchar(Env$l.var$droiteB)>0 & Env$l.var$droiteB!=Env$voc[95,1])) {
    for (i in 1:length(niveaux)) {
	if (Env$l.var$droiteB[i]==Env$voc[145,1]) {
	  abline(lm(varY[facteur==niveaux[i]]~varX[facteur==niveaux[i]])$coefficients,col=Env$l.var$couleur2B[i],
	    lty=type.trait(type=Env$l.var$trait2[i]),lwd=Env$l.var$epaisseur2[i])
	} else
	if (Env$l.var$droiteB[i]==Env$voc[146,1]) {
	  b<-sd(varY[facteur==niveaux[i]],na.rm=TRUE)/sd(varX[facteur==niveaux[i]],na.rm=TRUE)*sign(cov(varX[facteur==niveaux[i]],varY[facteur==niveaux[i]],use="complete.obs"))
	  a<-mean(varY[facteur==niveaux[i]],na.rm=TRUE)-b*mean(varX[facteur==niveaux[i]],na.rm=TRUE)
	  abline(a,b,col=Env$l.var$couleur2B[i],lty=type.trait(type=Env$l.var$trait2[i]),lwd=Env$l.var$epaisseur2[i])
	} else
	if (Env$l.var$droiteB[i]==Env$voc[147,1]) {
	  x<-varX[facteur==niveaux[i]]
	  y<-varY[facteur==niveaux[i]]
	  model<-lm(y~x+I(x^2))
	  varX2<-seq(min(varX[facteur==niveaux[i]],na.rm=TRUE),max(varX[facteur==niveaux[i]],na.rm=TRUE),abs(max(varX[facteur==niveaux[i]],na.rm=TRUE)-min(varX[facteur==niveaux[i]],na.rm=TRUE))/1000)
	  varY2<-predict(model,list(x=varX2))
	  lines(varX2,varY2,col=Env$l.var$couleur2B[i],lty=type.trait(type=Env$l.var$trait2[i]),
	    lwd=Env$l.var$epaisseur2[i])
	} else
	if (Env$l.var$droiteB[i]==Env$voc[148,1]) {
	  panel.smooth(varX[facteur==niveaux[i]],varY[facteur==niveaux[i]],pch=symboles[i],cex=Env$l.var$taille.ptsB[i],
	    col=Env$l.var$couleur2B[i],col.smooth=Env$l.var$couleur2B[i],lty=type.trait(type=Env$l.var$trait2[i]),
	    lwd=Env$l.var$epaisseur2[i])
	}
    }
  }
  if (tclvalue(Env$l.var$legende)==1) {
    graphe.legende(type="nuage",symboles=symboles)
  }
  graphe.titre()
  graphe.axes()
  graphe.box()
}

