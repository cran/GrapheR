code.graph.axes <-
function() {
  texte<-ifelse(tclvalue(Env$l.var$graduations.orient)==Env$voc[246,1],"","par(las=1)\n")
  if (Env$l.var$ecran=="H" & tclvalue(Env$l.var$hist.type)==Env$voc[40,1]) {
    texte<-paste(texte,"axis(1, labels=middles, at=(0.5:(length(na.omit(frequencies))))",sep="")
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n",sep="")
  } else if (Env$l.var$ecran=="M") {
    if (tclvalue(Env$l.var$box.orient)==Env$voc[68,1]) {
	texte<-paste(texte,"axis(1, labels=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\"), at=1:",length(Env$l.var$noms1),sep="")
    } else {
	texte<-paste(texte,"axis(1",sep="")
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
    texte<-paste(texte,"abline(h=",ordonnee,")\n",sep="")
    if (tclvalue(Env$l.var$nobar)==1) {
	texte<-paste(texte,"mtext(",sep="")
	if(tclvalue(Env$l.var$moyprop)=="moy"){
	  texte<-paste(texte,"c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\")",sep="")
	} else {
	  texte<-paste(texte,"c(\"",paste(Env$l.var$nomsprop.fac,collapse="\",\""),"\")",sep="")
	}
	texte<-paste(texte,", side=1",sep="")
	if (nrow(t(Env$l.var$add.abscisses))==1) {
	  texte<-paste(texte,", at=graph",sep="")
	} else {
	  texte<-paste(texte,", at=c(",paste(round(apply(Env$l.var$add.abscisses,2,mean),1),collapse=","),")",sep="")
	}
	texte<-paste(texte,", line=1.1",sep="")
	texte<-paste(texte,",\n  cex=",tclvalue(Env$l.var$legendes.taille),sep="")
	texte<-paste(texte,", col=\"",tclvalue(Env$l.var$legendes.col),"\")\n",sep="")
    }
  } else {
    texte<-paste("axis(1",sep="")
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n",sep="")
  }
  if (Env$l.var$ecran=="M") {
    if (tclvalue(Env$l.var$box.orient)==Env$voc[68,1]) {
	texte<-paste(texte,"axis(2",sep="")
	if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
	if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
	texte<-paste(texte,")\n",sep="")
    } else {
	texte<-paste(texte,"axis(2, labels=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\"), at=1:",length(Env$l.var$noms1),sep="")
	if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
	if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
	texte<-paste(texte,")\n",sep="")
    }
  } else {
    texte<-paste(texte,"axis(2",sep="")
    if (tclvalue(Env$l.var$graduations.col)!="black" & tclvalue(Env$l.var$graduations.col)!="#000000") {texte<-paste(texte,", col.axis=\"",tclvalue(Env$l.var$graduations.col),"\"",sep="")}
    if (tclvalue(Env$l.var$graduations.taille)!="1") {texte<-paste(texte,", cex.axis=",tclvalue(Env$l.var$graduations.taille),sep="")}
    texte<-paste(texte,")\n",sep="")
  }
  texte<-paste(texte,ifelse(tclvalue(Env$l.var$graduations.orient)==Env$voc[246,1],"\n","par(las=0)\n\n"),sep="")
  cat(texte)
}
