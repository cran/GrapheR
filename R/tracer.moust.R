tracer.moust <-
function() {
  variable<-Env$dataset[,tclvalue(Env$l.var$variable)]
  facteur<-if (nchar(tclvalue(Env$l.var$facteur2))>0) {
    if (tclvalue(Env$l.var$facteur2)!=Env$voc[82,1]) {
	Env$l.var$facteur.interaction
    } else {
	Env$dataset[,tclvalue(Env$l.var$facteur1)]
    }
  } else {
    Env$dataset[,tclvalue(Env$l.var$facteur1)]
  }
  orient<-ifelse (tclvalue(Env$l.var$box.orient)==Env$voc[67,1],"hor","ver")
  log.axes<-if (orient=="ver" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "y"
  } else if (orient=="hor" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "x"
  } else {""}
  Env$l.code$y.inf<-y.inf<-if (tclvalue(Env$l.var$liminf.axevaleurs)=="Auto") {
    min(na.omit(variable))
  } else {
    as.numeric(tclvalue(Env$l.var$liminf.axevaleurs))
  }
  Env$l.code$y.sup<-y.sup<-if (tclvalue(Env$l.var$limsup.axevaleurs)=="Auto") {
    max(na.omit(variable))
  } else {
    as.numeric(tclvalue(Env$l.var$limsup.axevaleurs))
  }
  boxplot(variable~facteur,axes=FALSE,ann=FALSE,horizontal=ifelse(orient=="hor",TRUE,FALSE),col=Env$l.var$couleur1B,
    boxcol=Env$l.var$col.borduresB,medcol=Env$l.var$col.borduresB,whiskcol=Env$l.var$col.borduresB,
    staplecol=Env$l.var$col.borduresB,outcol=Env$l.var$col.borduresB,whisklty=type.trait(type=tclvalue(Env$l.var$trait1)),
    outline=as.logical(as.numeric(tclvalue(Env$l.var$outliers))),outpch=ifelse(tclvalue(Env$l.var$box.symbol)==Env$voc[80,1],16,1),
    range=as.numeric(tclvalue(Env$l.var$lg.moustaches)),notch=as.logical(as.numeric(tclvalue(Env$l.var$ICmediane))),log=log.axes,
    ylim=c(y.inf,y.sup),names=Env$l.var$noms1,varwidth=ifelse(tclvalue(Env$l.var$varwidth)==1,TRUE,FALSE))
  if (tclvalue(Env$l.var$boxmoy)==1) {
    if (tclvalue(Env$l.var$box.orient)==Env$voc[67,1]) {
	points(tapply(variable,facteur,function(x) mean(x,na.rm=TRUE)),1:nlevels(facteur),cex=2,col=Env$l.var$col.borduresB,pch="+")
    } else {
	points(1:nlevels(facteur),tapply(variable,facteur,function(x) mean(x,na.rm=TRUE)),cex=2,col=Env$l.var$col.borduresB,pch="+")
    }
  }
  graphe.titre(type="moust",orient=orient)
  graphe.axes(type="moust",orient=orient)
  graphe.box()
}
