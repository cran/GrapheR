tracer.hist <-
function() {
  variable<-if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
    Env$dataset[,tclvalue(Env$l.var$variable)][Env$dataset[,tclvalue(Env$l.var$facteur1)]==tclvalue(Env$l.var$niveau)]
  } else {
    Env$dataset[,tclvalue(Env$l.var$variable)]
  }
  if (tclvalue(Env$l.var$hist.type)==Env$voc[40,1]) {
    total<-sum(hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),
	plot=FALSE)$counts)
    frequence<-hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),
	plot=FALSE)$counts/total
    limites<-tracer.hist.limites(variable=variable,type="freq",frequence=frequence)
    Env$l.code$x.inf<-x.inf<-limites$xinf
    Env$l.code$x.sup<-x.sup<-limites$xsup
    Env$l.code$y.sup<-y.sup<-limites$ysup
    barplot(frequence,axes=FALSE,ann=FALSE,space=0,col=tclvalue(Env$l.var$couleur1A),border=tclvalue(Env$l.var$col.borduresA),
	xlim=c(x.inf,x.sup),ylim=c(0,y.sup))
    graphe.axes(type="hist.freq",mids=hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),
	plot=FALSE)$mids,longueur=length(frequence))
  } else if (tclvalue(Env$l.var$hist.type)==Env$voc[41,1]) {
    limites<-tracer.hist.limites(variable=variable,type="eff")
    Env$l.code$x.inf<-x.inf<-limites$xinf
    Env$l.code$x.sup<-x.sup<-limites$xsup
    Env$l.code$y.sup<-y.sup<-limites$ysup
    hist(variable,axes=FALSE,ann=FALSE,freq=TRUE,col=tclvalue(Env$l.var$couleur1A),border=tclvalue(Env$l.var$col.borduresA),
	breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),
	xlim=c(x.inf,x.sup),ylim=c(0,y.sup))
    graphe.axes()
  } else if (tclvalue(Env$l.var$hist.type)==Env$voc[42,1]) {
    Env$l.var$add.seq<-seq(min(variable,na.rm=TRUE),max(variable,na.rm=TRUE),abs(max(variable,na.rm=TRUE)-min(variable,na.rm=TRUE))/1000)
    Env$l.var$add.seq2<-floor(min(variable,na.rm=TRUE)):ceiling(max(variable,na.rm=TRUE))
    limites<-tracer.hist.limites(variable=variable,type="dens")
    Env$l.code$x.inf<-x.inf<-limites$xinf
    Env$l.code$x.sup<-x.sup<-limites$xsup
    Env$l.code$y.sup<-y.sup<-limites$ysup
    hist(variable,axes=FALSE,ann=FALSE,freq=FALSE,col=tclvalue(Env$l.var$couleur1A),border=tclvalue(Env$l.var$col.borduresA),
      breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),
	xlim=c(x.inf,x.sup),ylim=c(0,y.sup))
    graphe.axes()
    if (tclvalue(Env$l.var$hist.dens)=="1") {
	lines(density(na.omit(variable)),col=tclvalue(Env$l.var$couleur2A),lwd=as.numeric(tclvalue(Env$l.var$epaisseur1)),lty=type.trait(type=tclvalue(Env$l.var$trait1)))
    }
  }
  graphe.titre()
  graphe.box()
}

