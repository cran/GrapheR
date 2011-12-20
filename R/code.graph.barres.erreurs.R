code.graph.barres.erreurs  <-
function(type) {
  if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
    cat("# Error bars\n\n")
    texte<-""
    texte<-paste("segments(graph, ",type," - std.dev, graph, ",type," + std.dev",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(graph - 0.1, ",type," + std.dev, graph + 0.1, ",type," + std.dev",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(graph - 0.1, ",type," - std.dev, graph + 0.1, ",type," - std.dev",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n\n",sep="")
    cat(texte)
  } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
    cat("# Error bars\n\n")
    texte<-""
    texte<-paste("segments(graph, ",type," - std.err, graph, ",type," + std.err",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(graph - 0.1, ",type," + std.err, graph + 0.1, ",type," + std.err",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n",sep="")
    texte<-paste(texte,"segments(graph - 0.1, ",type," - std.err, graph + 0.1, ",type," - std.err",sep="")
    if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
    texte<-paste(texte,")\n\n",sep="")
    cat(texte)
  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
    cat("# Error bars\n\n")
    texte<-""
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	texte<-paste("segments(graph, ",type," - ci, graph, ",type," + ci",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(graph - 0.1, ",type," + ci, graph + 0.1, ",type," + ci",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(graph - 0.1, ",type," - ci, graph + 0.1, ",type," - ci",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n\n",sep="")
    } else {
	texte<-paste("segments(graph, ",type," - ci.inf, graph, ",type," + ci.sup",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(graph - 0.1, ",type," + ci.sup, graph + 0.1, ",type," + ci.sup",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(graph - 0.1, ",type," - ci.inf, graph + 0.1, ",type," - ci.inf",sep="")
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,")\n\n",sep="")
    }
    cat(texte)
  }
}
