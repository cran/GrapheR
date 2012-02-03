code.graph.moust <-
function() {
  variable<-tclvalue(Env$l.var$variable)
  facteur1<-tclvalue(Env$l.var$facteur1)
  facteur2<-tclvalue(Env$l.var$facteur2)
  texte<-""
  if (nchar(facteur2)>0 & facteur2!=Env$voc[82,1]) {
    texte<-paste("boxplot(",variable," ~ interaction, axes=FALSE, ann=FALSE",sep="")
  } else {
    texte<-paste("boxplot(",variable," ~ ",facteur1,", axes=FALSE, ann=FALSE",sep="")
  }
  orient<-ifelse (tclvalue(Env$l.var$box.orient)==Env$voc[67,1],"hor","ver")
  log.axes<-if (orient=="ver" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "y"
  } else if (orient=="hor" & tclvalue(Env$l.var$log.axevaleurs)==1) {
    "x"
  } else {""}
  texte<-paste(texte,", horizontal=",ifelse(orient=="hor","TRUE","FALSE"),sep="")
  texte<-paste(texte,",\n  col=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,", boxcol=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,", medcol=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,",\n  whiskcol=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,", staplecol=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,", whisklty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
  texte<-paste(texte,",\n  outline=",ifelse(tclvalue(Env$l.var$outliers)=="1","TRUE","FALSE"),sep="")
  if (tclvalue(Env$l.var$outliers)=="1") {texte<-paste(texte,", outpch=",ifelse(tclvalue(Env$l.var$box.symbol)==Env$voc[80,1],16,1),sep="")}
  if (tclvalue(Env$l.var$outliers)=="1") {texte<-paste(texte,", outcol=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")}
  texte<-paste(texte,", range=",as.numeric(tclvalue(Env$l.var$lg.moustaches)),sep="")
  texte<-paste(texte,", notch=",ifelse(tclvalue(Env$l.var$ICmediane)=="1","TRUE","FALSE"),sep="")
  if (log.axes!="") {texte<-paste(texte,", log=",log.axes,sep="")}
  texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
  if (tclvalue(Env$l.var$varwidth)==1) {texte<-paste(texte,", varwidth=TRUE",sep="")}
  texte<-paste(texte,")\n\n",sep="")
  cat(texte)
  if (tclvalue(Env$l.var$boxmoy)==1) {
    facteur<-NULL
    if (nchar(facteur2)>0 & facteur2!=Env$voc[82,1]) {
	facteur <- factor(paste(Env$dataset[,tclvalue(Env$l.var$facteur1)],Env$dataset[,tclvalue(Env$l.var$facteur2)],sep=":"))
    } else {
	facteur <- Env$dataset[,tclvalue(Env$l.var$facteur1)]
    }
    texte<-""
    if (tclvalue(Env$l.var$box.orient)==Env$voc[67,1]) {
	texte<-paste("points(means",sep="")
	texte<-paste(texte,", 1:",nlevels(facteur),sep="")
    } else {
	texte<-paste("points(1:",nlevels(facteur),sep="")
	texte<-paste(texte,", means",sep="")
    }
    texte<-paste(texte,", cex=2",sep="")
    texte<-paste(texte,", col=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
    texte<-paste(texte,", pch=\"+\")\n\n",sep="")
    cat(texte)
  }
  code.graph.axes()
  code.graph.titre()
  if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
}
