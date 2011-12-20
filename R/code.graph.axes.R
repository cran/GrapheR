code.graph.axes <-
function() {
  texte<-""
  if (Env$l.var$ecran=="H" & tclvalue(Env$l.var$hist.type)==Env$voc[40,1]) {
    texte<-"axis(1, labels=middles, at=(0.5:(length(na.omit(frequencies))))"
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n",sep="")
  } else if (Env$l.var$ecran=="M") {
    if (tclvalue(Env$l.var$box.orient)==Env$voc[68,1]) {
	texte<-paste("axis(1, labels=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\"), at=1:",length(Env$l.var$noms1),sep="")
    } else {
	texte<-"axis(1"
    }
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n",sep="")
  } else if (Env$l.var$ecran=="B") {
    ordonnee<-if (Env$l.code$y.inf>=0 & Env$l.code$y.sup>0) {
	Env$l.code$y.inf
    } else if (Env$l.code$y.inf<0 & Env$l.code$y.sup>0){
	0
    } else if (Env$l.code$y.inf<0 & Env$l.code$y.sup<=0) {
	Env$l.code$y.sup
    }
    texte<-paste("abline(h=",ordonnee,")\n",sep="")
  } else {
    texte<-"axis(1"
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n",sep="")
  }
  if (Env$l.var$ecran=="M") {
    if (tclvalue(Env$l.var$box.orient)==Env$voc[68,1]) {
	texte<-paste(texte,"axis(2",sep="")
	if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
	if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
	texte<-paste(texte,")\n\n",sep="")
    } else {
	texte<-paste(texte,"axis(2, labels=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\"), at=1:",length(Env$l.var$noms1),sep="")
	if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
	if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
	texte<-paste(texte,")\n\n",sep="")
    }
  } else {
    texte<-paste(texte,"axis(2",sep="")
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n\n",sep="")
  }
  cat(texte)
}
