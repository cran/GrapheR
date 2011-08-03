graphe.legende <-
function(type,symboles=NULL,lignes=NULL,traits=NULL) {
  par(xpd=TRUE)
  position<-if (tclvalue(Env$l.var$legende.pos)==Env$voc[102,1]) {"top"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[103,1]) {"topright"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[104,1]) {"left"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[105,1]) {"center"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[106,1]) {"right"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[107,1]) {"bottomleft"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[108,1]) {"bottom"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[109,1]) {"bottomright"} else
    {"topleft"}
  if (type=="bar") {
    if (nchar(tclvalue(Env$l.var$legende.titre))>0) {
	if (tclvalue(Env$l.var$moyprop)=="moy") {
	  legend(position,legend=Env$l.var$noms2,fill=Env$l.var$couleur1B,title=tclvalue(Env$l.var$legende.titre))
	} else {
	  legend(position,legend=Env$l.var$nomsprop,fill=Env$l.var$couleur1B,title=tclvalue(Env$l.var$legende.titre))
	}
    } else {
	if (tclvalue(Env$l.var$moyprop)=="moy") {
	  legend(position,legend=Env$l.var$noms2,fill=Env$l.var$couleur1B)
	} else {
	  legend(position,legend=Env$l.var$nomsprop,fill=Env$l.var$couleur1B)
	}
    }
  }
  if (type=="cam") {
    if (nchar(tclvalue(Env$l.var$legende.titre))>0) {
	legend(position,legend=Env$l.var$nomsparts,fill=Env$l.var$couleur1B,title=tclvalue(Env$l.var$legende.titre))
    } else {
	legend(position,legend=Env$l.var$nomsparts,fill=Env$l.var$couleur1B)
    }
  }
  if (type=="courbe") {
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
    if (nchar(tclvalue(Env$l.var$legende.titre))>0) {
	legend(position,legend=Env$l.var$noms1,col=Env$l.var$couleur2B,pch=symboles,pt.cex=taille.pts,
	  lty=traits,lwd=epaisseur,title=tclvalue(Env$l.var$legende.titre))
    } else {
	legend(position,legend=Env$l.var$noms1,col=Env$l.var$couleur2B,pch=symboles,pt.cex=taille.pts,
	  lty=traits,lwd=epaisseur)
    }
  }
  if (type=="nuage") {
    if (nchar(tclvalue(Env$l.var$legende.titre))>0) {
	legend(position,legend=Env$l.var$noms1,col=Env$l.var$couleur2B,pch=symboles,pt.cex=Env$l.var$taille.ptsB,
	  title=tclvalue(Env$l.var$legende.titre))
    } else {
	legend(position,legend=Env$l.var$noms1,col=Env$l.var$couleur2B,pch=symboles,pt.cex=Env$l.var$taille.ptsB)
    }
  }
  par(xpd=FALSE)
}

