graphe.titre <-
function(type="",orient=NULL) {
  if (type=="moust") {
    titre.x<-if (orient=="ver") {
	tclvalue(Env$l.var$titre.axenoms)
    } else {
	tclvalue(Env$l.var$titre.axevaleurs)
    }
    titre.y<-if (orient=="ver") {
	tclvalue(Env$l.var$titre.axevaleurs)
    } else {
	tclvalue(Env$l.var$titre.axenoms)
    }
    title(main=tclvalue(Env$l.var$titre),col.main=tclvalue(Env$l.var$titre.col),cex.main=as.numeric(tclvalue(Env$l.var$titre.taille)),
	xlab=titre.x,ylab=titre.y,cex.lab=as.numeric(tclvalue(Env$l.var$legendes.taille)),col.lab=tclvalue(Env$l.var$legendes.col))
  } else {
    title(main=tclvalue(Env$l.var$titre),col.main=tclvalue(Env$l.var$titre.col),cex.main=as.numeric(tclvalue(Env$l.var$titre.taille)),
	xlab=tclvalue(Env$l.var$titre.axehor),ylab=tclvalue(Env$l.var$titre.axever),cex.lab=as.numeric(tclvalue(Env$l.var$legendes.taille)),
	col.lab=tclvalue(Env$l.var$legendes.col))
  }
}

