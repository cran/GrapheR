tracer.moust <-
function() {
  variable<-Env$dataset[,tclvalue(Env$l.var$variable)]
  facteur<-Env$dataset[,tclvalue(Env$l.var$facteur1)]
  orient<-ifelse (tclvalue(Env$l.var$box.orient)==Env$voc[67,1],"hor","ver")
  log.axes<-if (orient=="ver" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "y"
  } else if (orient=="hor" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "x"
  } else {""}
  y.inf<-if (tclvalue(Env$l.var$liminf.axevaleurs)=="Auto") {
    min(na.omit(variable))
  } else {
    as.numeric(tclvalue(Env$l.var$liminf.axevaleurs))
  }
  y.sup<-if (tclvalue(Env$l.var$limsup.axevaleurs)=="Auto") {
    max(na.omit(variable))
  } else {
    as.numeric(tclvalue(Env$l.var$limsup.axevaleurs))
  }
  boxplot(variable~facteur,axes=FALSE,ann=FALSE,horizontal=ifelse(orient=="hor",TRUE,FALSE),col=tclvalue(Env$l.var$couleur1A),
    boxcol=tclvalue(Env$l.var$col.borduresA),medcol=tclvalue(Env$l.var$col.borduresA),whiskcol=tclvalue(Env$l.var$couleur2A),
    staplecol=tclvalue(Env$l.var$couleur2A),outcol=tclvalue(Env$l.var$couleur3),whisklty=type.trait(type=tclvalue(Env$l.var$trait1)),
    outline=as.logical(as.numeric(tclvalue(Env$l.var$outliers))),outpch=ifelse(tclvalue(Env$l.var$box.symbol)==Env$voc[80,1],16,1),
    range=as.numeric(tclvalue(Env$l.var$lg.moustaches)),notch=as.logical(as.numeric(tclvalue(Env$l.var$ICmediane))),log=log.axes,
    ylim=c(y.inf,y.sup),names=Env$l.var$noms1)
  graphe.titre(type="moust",orient=orient)
  graphe.axes(type="moust",orient=orient)
  graphe.box()
}

