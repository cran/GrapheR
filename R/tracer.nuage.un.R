tracer.nuage.un <-
function() {
  if(nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
    varX<-Env$dataset[,tclvalue(Env$l.var$varX)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]]
    varY<-Env$dataset[,tclvalue(Env$l.var$varY)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(tclvalue(Env$l.var$niveau))+1]]
  } else {
    varX<-Env$dataset[,tclvalue(Env$l.var$varX)]
    varY<-Env$dataset[,tclvalue(Env$l.var$varY)]
  }
  limites<-tracer.nuage.limites(varX=varX,varY=varY)
  Env$l.code$x.inf<-x.inf<-limites$xinf
  Env$l.code$x.sup<-x.sup<-limites$xsup
  Env$l.code$y.inf<-y.inf<-limites$yinf
  Env$l.code$y.sup<-y.sup<-limites$ysup
  symbole<-graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA)))
  plot(varY~varX,axes=FALSE,ann=FALSE,xlim=c(x.inf,x.sup),ylim=c(y.inf,y.sup),log=graphe.log(),
    col=tclvalue(Env$l.var$couleur2A),pch=symbole,cex=as.numeric(tclvalue(Env$l.var$taille.ptsA)))
  if (nchar(tclvalue(Env$l.var$droiteA))>0 & tclvalue(Env$l.var$droiteA)!=Env$voc[95,1]) {
    if (tclvalue(Env$l.var$droiteA)==Env$voc[145,1]) {
	model<-lm(varY~varX)
	abline(model$coefficients,col=tclvalue(Env$l.var$couleur2A),
	  lty=type.trait(type=tclvalue(Env$l.var$trait1)),lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)))
	if (tclvalue(Env$l.var$intervalA)==Env$voc[261,1]) {
	  varX2<-seq(min(varX,na.rm=TRUE),max(varX,na.rm=TRUE),abs(max(varX,na.rm=TRUE)-min(varX,na.rm=TRUE))/1000)
	  varY2<-predict(model,list(varX=varX2),interval="confidence")
	  lines(varX2,varY2[,"lwr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY2[,"upr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[262,1]) {
	  varX2<-seq(min(varX,na.rm=TRUE),max(varX,na.rm=TRUE),abs(max(varX,na.rm=TRUE)-min(varX,na.rm=TRUE))/1000)
	  varY2<-predict(model,list(varX=varX2),interval="prediction")
	  lines(varX2,varY2[,"lwr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY2[,"upr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[263,1]) {
	  varX2<-seq(min(varX,na.rm=TRUE),max(varX,na.rm=TRUE),abs(max(varX,na.rm=TRUE)-min(varX,na.rm=TRUE))/1000)
	  varY2.a<-predict(model,list(varX=varX2),interval="confidence")
	  lines(varX2,varY2.a[,"lwr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY2.a[,"upr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	  varY2.b<-predict(model,list(varX=varX2),interval="prediction")
	  lines(varX2,varY2.b[,"lwr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY2.b[,"upr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	}
    } else
    if (tclvalue(Env$l.var$droiteA)==Env$voc[146,1]) {
	b<-sd(varY,na.rm=TRUE)/sd(varX,na.rm=TRUE)*sign(cov(varX,varY,use="complete.obs"))
	a<-mean(varY,na.rm=TRUE)-b*mean(varX,na.rm=TRUE)
	abline(a,b,col=tclvalue(Env$l.var$couleur2A),
	  lty=type.trait(type=tclvalue(Env$l.var$trait1)),lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)))
    } else
    if (tclvalue(Env$l.var$droiteA)==Env$voc[147,1]) {
	model<-lm(varY~varX+I(varX^2))
	varX2<-seq(min(varX,na.rm=TRUE),max(varX,na.rm=TRUE),abs(max(varX,na.rm=TRUE)-min(varX,na.rm=TRUE))/1000)
	varY2<-predict(model,list(varX=varX2))
	lines(varX2,varY2,col=tclvalue(Env$l.var$couleur2A),lty=type.trait(type=tclvalue(Env$l.var$trait1)),
	  lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)))
	if (tclvalue(Env$l.var$intervalA)==Env$voc[261,1]) {
	  varY3<-predict(model,list(varX=varX2),interval="confidence")
	  lines(varX2,varY3[,"lwr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY3[,"upr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[262,1]) {
	  varY3<-predict(model,list(varX=varX2),interval="prediction")
	  lines(varX2,varY3[,"lwr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY3[,"upr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	}
	if (tclvalue(Env$l.var$intervalA)==Env$voc[263,1]) {
	  varY3.a<-predict(model,list(varX=varX2),interval="confidence")
	  lines(varX2,varY3.a[,"lwr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY3.a[,"upr"],lty=2,col=tclvalue(Env$l.var$couleur2A))
	  varY3.b<-predict(model,list(varX=varX2),interval="prediction")
	  lines(varX2,varY3.b[,"lwr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	  lines(varX2,varY3.b[,"upr"],lty=3,col=tclvalue(Env$l.var$couleur2A))
	}
    } else
    if (tclvalue(Env$l.var$droiteA)==Env$voc[148,1]) {
	panel.smooth(varX,varY,pch=symbole,cex=as.numeric(tclvalue(Env$l.var$taille.ptsA)),
	  col=tclvalue(Env$l.var$couleur2A),col.smooth=tclvalue(Env$l.var$couleur2A),
	  lty=type.trait(type=tclvalue(Env$l.var$trait1)),lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)))
    }
  }
  if (tclvalue(Env$l.var$ptlab)==1) {
    text(varX,varY,pos=3,offset=0.4,cex=0.65*as.numeric(tclvalue(Env$l.var$taille.ptsA)),col=tclvalue(Env$l.var$couleur2A))
  }
  graphe.titre()
  graphe.axes()
  graphe.box()
}
