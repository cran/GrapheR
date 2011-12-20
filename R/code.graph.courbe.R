code.graph.courbe <-
function() {
  texte<-""
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    if (tclvalue(Env$l.var$plusieurs)==0) {
	cat("abscissae <- as.numeric(names(means))\n")
	texte<-"plot(means ~ abscissae, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
	texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,",\n  pch=",graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA))),sep="")
	if (tclvalue(Env$l.var$taille.ptsA)!="1") {texte<-paste(texte,", cex=",as.numeric(tclvalue(Env$l.var$taille.ptsA)),sep="")}
	texte<-paste(texte,", type=\"",type.ligne(type=tclvalue(Env$l.var$type.courbeA)),"\"",sep="")
	if (type.ligne(type=tclvalue(Env$l.var$type.courbeA))!="p") {
	  texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	  texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	}
	cat(paste(texte,")\n\n",sep=""))
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	code.graph.courbe.erreurs1(type="means")
    } else {
	symboles<-graphe.symboles(num=Env$l.var$symboleB)
	lignes<-type.ligne(type=Env$l.var$type.courbeB)
	traits<-type.trait(type=Env$l.var$trait2)
	cat("abscissae <- as.numeric(colnames(means))\n")
	texte<-"plot(means[1,] ~ abscissae, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
	texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	texte<-paste(texte,", col=\"",Env$l.var$couleur2B[1],"\"",sep="")
	texte<-paste(texte,",\n  pch=",symboles[1],sep="")
	if (Env$l.var$taille.ptsB[1]!="1") {texte<-paste(texte,", cex=",as.numeric(Env$l.var$taille.ptsB[1]),sep="")}
	texte<-paste(texte,", type=\"",lignes[1],"\"",sep="")
	if (lignes[1]!="p") {
	  texte<-paste(texte,", lty=",traits[1],sep="")
	  texte<-paste(texte,", lwd=",as.numeric(Env$l.var$epaisseur2[1]),sep="")
	}
	cat(paste(texte,")\n",sep=""))
	for (i in 2:length(Env$l.var$noms1)) {
	  texte<-paste("lines(abscissae, means[",i,",]",sep="")
	  texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")
	  texte<-paste(texte,", pch=",symboles[i],sep="")
	  if (Env$l.var$taille.ptsB[i]!="1") {texte<-paste(texte,", cex=",as.numeric(Env$l.var$taille.ptsB[i]),sep="")}
	  texte<-paste(texte,", type=\"",lignes[i],"\"",sep="")
	  if (lignes[i]!="p") {
	    texte<-paste(texte,", lty=",traits[i],sep="")
	    texte<-paste(texte,", lwd=",as.numeric(Env$l.var$epaisseur2[i]),sep="")
	  }
	  cat(paste(texte,")\n",sep=""))
	}
	cat("\n")
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	code.graph.courbe.erreurs2(type="means")
	if (tclvalue(Env$l.var$legende)==1) {code.graph.courbe.leg()}
    }
  } else {
    if (tclvalue(Env$l.var$plusieurs)==0) {
	cat("abscissae <- as.numeric(as.character(levels(varX)))\n")
	texte<-"plot(proportions ~ abscissae, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
	texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	if (tclvalue(Env$l.var$couleur2A)!="black" & tclvalue(Env$l.var$couleur2A)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur2A),"\"",sep="")}
	texte<-paste(texte,",\n  pch=",graphe.symboles(num=as.numeric(tclvalue(Env$l.var$symboleA))),sep="")
	if (tclvalue(Env$l.var$taille.ptsA)!="1") {texte<-paste(texte,", cex=",as.numeric(tclvalue(Env$l.var$taille.ptsA)),sep="")}
	texte<-paste(texte,", type=\"",type.ligne(type=tclvalue(Env$l.var$type.courbeA)),"\"",sep="")
	if (type.ligne(type=tclvalue(Env$l.var$type.courbeA))!="p") {
	  texte<-paste(texte,", lty=",type.trait(type=tclvalue(Env$l.var$trait1)),sep="")
	  texte<-paste(texte,", lwd=",as.numeric(tclvalue(Env$l.var$epaisseur1)),sep="")
	}
	cat(paste(texte,")\n\n",sep=""))
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	code.graph.courbe.erreurs1(type="proportions")
    } else {
	symboles<-graphe.symboles(num=Env$l.var$symboleB)
	lignes<-type.ligne(type=Env$l.var$type.courbeB)
	traits<-type.trait(type=Env$l.var$trait2)
	cat("abscissae <- as.numeric(as.character(levels(varX)))\n")
	texte<-"plot(proportions[1,] ~ abscissae, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", xlim=c(",round(Env$l.code$x.inf,2),",",round(Env$l.code$x.sup,2),")",sep="")
	texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	texte<-paste(texte,", col=\"",Env$l.var$couleur2B[1],"\"",sep="")
	texte<-paste(texte,",\n  pch=",symboles[1],sep="")
	if (Env$l.var$taille.ptsB[1]!="1") {texte<-paste(texte,", cex=",as.numeric(Env$l.var$taille.ptsB[1]),sep="")}
	texte<-paste(texte,", type=\"",lignes[1],"\"",sep="")
	if (lignes[1]!="p") {
	  texte<-paste(texte,", lty=",traits[1],sep="")
	  texte<-paste(texte,", lwd=",as.numeric(Env$l.var$epaisseur2[1]),sep="")
	}
	cat(paste(texte,")\n",sep=""))
	for (i in 2:length(Env$l.var$noms1)) {
	  texte<-paste("lines(abscissae, proportions[",i,",]",sep="")
	  texte<-paste(texte,", col=\"",Env$l.var$couleur2B[i],"\"",sep="")
	  texte<-paste(texte,", pch=",symboles[i],sep="")
	  if (Env$l.var$taille.ptsB[i]!="1") {texte<-paste(texte,", cex=",as.numeric(Env$l.var$taille.ptsB[i]),sep="")}
	  texte<-paste(texte,", type=\"",lignes[i],"\"",sep="")
	  if (lignes[i]!="p") {
	    texte<-paste(texte,", lty=",traits[i],sep="")
	    texte<-paste(texte,", lwd=",as.numeric(Env$l.var$epaisseur2[i]),sep="")
	  }
	  cat(paste(texte,")\n",sep=""))
	}
	cat("\n")
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	code.graph.courbe.erreurs2(type="proportions")
	if (tclvalue(Env$l.var$legende)==1) {code.graph.courbe.leg()}
    }
  }
}
