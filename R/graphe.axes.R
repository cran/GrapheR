graphe.axes <-
function(type="",mids=NULL,longueur=NULL,orient=NULL,ordonnee=NULL) {
  parametres<-par()
  par(col.axis=tclvalue(Env$l.var$graduations.col),cex.axis=as.numeric(tclvalue(Env$l.var$graduations.taille)))
  if (type=="hist.freq") {
    axis(1,labels=mids,at=(0.5:(longueur-0.5)))
  } else if (type=="moust") {
    if (orient=="ver") {
	axis(1,labels=Env$l.var$noms1,at=1:length(Env$l.var$noms1))
    } else {
	axis(1)
    }
  } else if (type=="bar") {
	abline(h=ordonnee)
  } else {
    axis(1)
  }
  if (type=="moust") {
    if (orient=="ver") {
	axis(2)
    } else {
	axis(2,labels=Env$l.var$noms1,at=1:length(Env$l.var$noms1))
    }
  } else {
    axis(2)
  }
  par<-parametres
}

