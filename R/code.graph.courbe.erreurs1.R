code.graph.courbe.erreurs1 <-
function(type) {
  if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
    cat("# Error bars\n\n")
    texte<-paste("width <- 0.015 * (",Env$l.code$x.sup," - ",Env$l.code$x.inf,")\n",sep="")
    texte<-paste(texte,"segments(abscissae, ",type," - std.dev, abscissae, ",type," + std.dev",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(abscissae - width, ",type," + std.dev, abscissae + width, ",type," + std.dev",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(abscissae - width, ",type," - std.dev, abscissae + width, ",type," - std.dev",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n\n",sep="")
    cat(texte)
  } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
    cat("# Error bars\n\n")
    texte<-paste("width <- 0.015 * (",Env$l.code$x.sup," - ",Env$l.code$x.inf,")\n",sep="")
    texte<-paste(texte,"segments(abscissae, ",type," - std.err, abscissae, ",type," + std.err",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(abscissae - width, ",type," + std.err, abscissae + width, ",type," + std.err",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(abscissae - width, ",type," - std.err, abscissae + width, ",type," - std.err",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n\n",sep="")
    cat(texte)
  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
    cat("# Error bars\n\n")
    texte<-paste("width <- 0.015 * (",Env$l.code$x.sup," - ",Env$l.code$x.inf,")\n",sep="")
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	texte<-paste(texte,"segments(abscissae, ",type," - ci, abscissae, ",type," + ci",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type," + ci, abscissae + width, ",type," + ci",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type," - ci, abscissae + width, ",type," - ci",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n\n",sep="")
    } else {
	texte<-paste(texte,"segments(abscissae, ",type," - ci.inf, abscissae, ",type," + ci.sup",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type," + ci.sup, abscissae + width, ",type," + ci.sup",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type," - ci.inf, abscissae + width, ",type," - ci.inf",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n\n",sep="")
    }
    cat(texte)
  }
}
