code.graph.hist <-
function() {
  variable<-tclvalue(Env$l.var$variable)
  facteur1<-tclvalue(Env$l.var$facteur1)
  texte<-""
  if (tclvalue(Env$l.var$hist.type)==Env$voc[40,1]) {
    texte<-"barplot(frequencies, axes=FALSE, ann=FALSE, space=0"
    texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur1A),"\"",sep="")
    if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")}
    texte<-paste(texte,",\n  xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
    texte<-paste(texte,", ylim=c(0,",round(Env$l.code$y.sup,2),"))\n\n",sep="")
    cat(texte)
    code.graph.axes()
    code.graph.titre()
    if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
  } else {
    if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	texte<-paste(texte,"hist(variable, axes=FALSE, ann=FALSE",sep="")
    } else {
	texte<-paste(texte,"hist(",variable,", axes=FALSE, ann=FALSE",sep="")
    }
    texte<-paste(texte,", freq=",ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[41,1],"TRUE","FALSE"),sep="")
    texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur1A),"\"",sep="")
    if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")}
    if (tclvalue(Env$l.var$hist.barres)!="Auto") {texte<-paste(texte,", breaks=",as.numeric(tclvalue(Env$l.var$hist.barres))-1,sep="")}
    texte<-paste(texte,",\n  xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
    texte<-paste(texte,", ylim=c(0,",round(Env$l.code$y.sup,2),"))\n\n",sep="")
    cat(texte)
    code.graph.axes()
    code.graph.titre()
    if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
    if (tclvalue(Env$l.var$hist.type)==Env$voc[42,1] & tclvalue(Env$l.var$hist.dens)==1) {
	texte<-""
	cat("# Distribution curve of the data\n\n")
	if (nchar(facteur1)>0 & facteur1!=Env$voc[82,1]) {
	  texte<-paste(texte,"lines(density(na.omit(variable))",sep="")
	} else {
	  texte<-paste(texte,"lines(density(na.omit(",variable,"))",sep="")
	}
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	if (tclvalue(Env$l.var$epaisseur1)!="1") {texte<-paste(texte,", lwd=",tclvalue(Env$l.var$epaisseur1),sep="")}
	texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),")\n\n",sep="")
	cat(texte)
    }
  }
}
