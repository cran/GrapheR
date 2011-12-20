code.graph.courbe.erreurs2 <-
function(type) {
  if (tclvalue(Env$l.var$erreur)==Env$voc[96,1]) {
    cat("# Error bars\n\n")
    texte<-paste("width <- 0.015 * (",Env$l.code$x.sup," - ",Env$l.code$x.inf,")\n\n",sep="")
    cat(texte)
    for (i in 1:length(Env$l.var$noms1)) {
	texte<-paste("segments(abscissae, ",type,"[",i,",] - std.dev[",i,",], abscissae, ",type,"[",i,",] + std.dev[",i,",]",sep="")
	if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] + std.dev[",i,",], abscissae + width, ",type,"[",i,",] + std.dev[",i,",]",sep="")
	if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] - std.dev[",i,",], abscissae + width, ",type,"[",i,",] - std.dev[",i,",]",sep="")
	if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	texte<-paste(texte,")\n\n",sep="")
	cat(texte)
    }
  } else if (tclvalue(Env$l.var$erreur)==Env$voc[97,1]) {
    cat("# Error bars\n\n")
    texte<-paste("width <- 0.015 * (",Env$l.code$x.sup," - ",Env$l.code$x.inf,")\n\n",sep="")
    cat(texte)
    for (i in 1:length(Env$l.var$noms1)) {
	texte<-paste("segments(abscissae, ",type,"[",i,",] - std.err[",i,",], abscissae, ",type,"[",i,",] + std.err[",i,",]",sep="")
	if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] + std.err[",i,",], abscissae + width, ",type,"[",i,",] + std.err[",i,",]",sep="")
	if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	texte<-paste(texte,")\n",sep="")
	texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] - std.err[",i,",], abscissae + width, ",type,"[",i,",] - std.err[",i,",]",sep="")
	if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	texte<-paste(texte,")\n\n",sep="")
	cat(texte)
    }
  } else if (tclvalue(Env$l.var$erreur)==Env$voc[98,1]) {
    cat("# Error bars\n\n")
    texte<-paste("width <- 0.015 * (",Env$l.code$x.sup," - ",Env$l.code$x.inf,")\n\n",sep="")
    cat(texte)
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	for (i in 1:length(Env$l.var$noms1)) {
	  texte<-paste("segments(abscissae, ",type,"[",i,",] - ci[",i,",], abscissae, ",type,"[",i,",] + ci[",i,",]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] + ci[",i,",], abscissae + width, ",type,"[",i,",] + ci[",i,",]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] - ci[",i,",], abscissae + width, ",type,"[",i,",] - ci[",i,",]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,")\n\n",sep="")
	  cat(texte)
	}
    } else {
	for (i in 1:length(Env$l.var$noms1)) {
	  texte<-paste("segments(abscissae, ",type,"[",i,",] - ci.inf[",i,",], abscissae, ",type,"[",i,",] + ci.sup[",i,",]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] + ci.sup[",i,",], abscissae + width, ",type,"[",i,",] + ci.sup[",i,",]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  texte<-paste(texte,"segments(abscissae - width, ",type,"[",i,",] - ci.inf[",i,",], abscissae + width, ",type,"[",i,",] - ci.inf[",i,",]",sep="")
	  if (Env$l.var$couleur2B[i]!="black" & Env$l.var$couleur2B[i]!="#000000") {texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")}
	  texte<-paste(texte,")\n\n",sep="")
	  cat(texte)
	}
    }
  }
}
