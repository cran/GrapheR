code.graph.courbe.leg <-
function() {
  symboles<-graphe.symboles(num=Env$l.var$symboleB)
  lignes<-type.ligne(type=Env$l.var$type.courbeB)
  traits<-type.trait(type=Env$l.var$trait2)
  epaisseur<-Env$l.var$epaisseur2
  taille.pts<-Env$l.var$taille.ptsB
  if (any(lignes=="p")) {
    traits[which(lignes=="p")]<-NA
    epaisseur[which(lignes=="p")]<-NA
  }
  if (any(lignes%in%c("l","h"))) {
    symboles[which(lignes%in%c("l","h"))]<-NA
    taille.pts[which(lignes%in%c("l","h"))]<-NA
  }
  cat("# Legend\n\n")
  cat("par(xpd=TRUE)\n")
  texte<-paste("legend(\"",code.graph.posleg(),"\"",sep="")
  texte<-paste(texte,", legend=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,", col=c(\"",paste(Env$l.var$couleur2B,collapse="\",\""),"\")",sep="")
  texte<-paste(texte,",\n  pch=c(",paste(symboles,collapse=","),")",sep="")
  texte<-paste(texte,", pt.cex=c(",paste(taille.pts,collapse=","),")",sep="")
  texte<-paste(texte,", lty=c(",paste(traits,collapse=","),")",sep="")
  texte<-paste(texte,", lwd=c(",paste(epaisseur,collapse=","),")",sep="")
  if (nchar(tclvalue(Env$l.var$legende.titre))>0) {texte<-paste(texte,", title=\"",tclvalue(Env$l.var$legende.titre),"\"",sep="")}
  cat(paste(texte,")\n",sep=""))
  cat("par(xpd=FALSE)\n\n")
}
