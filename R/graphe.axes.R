graphe.axes <-
function(type="",mids=NULL,longueur=NULL,orient=NULL,ordonnee=NULL) {
  parametres<-par()
  par(col.axis=tclvalue(Env$l.var$graduations.col),cex.axis=as.numeric(tclvalue(Env$l.var$graduations.taille)),
    las=ifelse(tclvalue(Env$l.var$graduations.orient)==Env$voc[246,1],0,1))
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
    if (tclvalue(Env$l.var$nobar)==1) {
	if (nrow(t(Env$l.var$add.abscisses))==1) {
	  mtext(text=if(tclvalue(Env$l.var$moyprop)=="moy"){Env$l.var$noms1}else{Env$l.var$nomsprop.fac},at=Env$l.var$add.abscisses,side=1,line=1.1,
	    cex=as.numeric(tclvalue(Env$l.var$legendes.taille)),col=tclvalue(Env$l.var$legendes.col))
	} else {
	  mtext(text=if(tclvalue(Env$l.var$moyprop)=="moy"){Env$l.var$noms1}else{Env$l.var$nomsprop.fac},at=apply(Env$l.var$add.abscisses,2,mean),side=1,
	    line=1.1,cex=as.numeric(tclvalue(Env$l.var$legendes.taille)),col=tclvalue(Env$l.var$legendes.col))
	}
    }
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
  par(las=0)
}
