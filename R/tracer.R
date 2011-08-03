tracer <-
function() {
  if(!is.null(Env$dataset)) {
    if (Env$l.var$ecran=="H") {
	if (nchar(tclvalue(Env$l.var$variable))>0 & nchar(tclvalue(Env$l.var$hist.type))>0) {
	  par(mar=c(5,5,4,2),bg="white")
	  tracer.hist()
	} else {
	  msg(text=Env$voc[154,1],type="error")
	}
    } else
    if (Env$l.var$ecran=="M") {
	if (nchar(tclvalue(Env$l.var$variable))>0 & nchar(tclvalue(Env$l.var$facteur1))>0) {
	  par(mar=c(5,5,4,2),bg="white")
	  tracer.moust()
	} else {
	  msg(text=Env$voc[154,1],type="error")
	}
    } else
    if (Env$l.var$ecran=="B") {
	if (tclvalue(Env$l.var$moyprop)=="moy") {
	  if (nchar(tclvalue(Env$l.var$variable))>0 & nchar(tclvalue(Env$l.var$facteur1))>0) {
	    if (tclvalue(Env$l.var$plusieurs)==0) {
		par(mar=c(5,5,4,2),bg="white")
		tracer.barres.moyun()
	    } else {
		par(mar=c(5,5,4,2),bg="white")
		tracer.barres.moyplusieurs()
	    }
	  } else {
	    msg(text=Env$voc[154,1],type="error")
	  }
	} else {
	  if (nchar(tclvalue(Env$l.var$proportions))>0 & nchar(tclvalue(Env$l.var$prop.niveaux))>0 & nchar(tclvalue(Env$l.var$facteurprop))>0) {
	    if (tclvalue(Env$l.var$plusieurs)==0) {
		par(mar=c(5,5,4,2),bg="white")
		tracer.barres.propun()
	    } else {
		par(mar=c(5,5,4,2),bg="white")
		tracer.barres.propplusieurs()
	    }
	  } else {
	    msg(text=Env$voc[154,1],type="error")
	  }
	}
    } else
    if (Env$l.var$ecran=="Ca") {
	if (nchar(tclvalue(Env$l.var$variable))>0 & nchar(tclvalue(Env$l.var$parts.niveaux))>0) {
	  par(mar=c(5,5,4,2),bg="white")
	  tracer.cam()
	} else {
	  msg(text=Env$voc[154,1],type="error")
	}
    } else
    if (Env$l.var$ecran=="Co") {
	if (tclvalue(Env$l.var$moyprop)=="moy") {
	  if (nchar(tclvalue(Env$l.var$varX))>0 & nchar(tclvalue(Env$l.var$varY))>0) {
	    if (tclvalue(Env$l.var$plusieurs)==0) {
		if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
		  if (nchar(tclvalue(Env$l.var$niveau))>0) {
		    par(mar=c(5,5,4,2),bg="white")
		    tracer.courbe.moyun()
		  } else {
		    msg(text=Env$voc[154,1],type="error")
		  }
		} else {
		  par(mar=c(5,5,4,2),bg="white")
		  tracer.courbe.moyun()
		}
	    } else {
		par(mar=c(5,5,4,2),bg="white")
		tracer.courbe.moyplusieurs()
	    }
	  } else {
	    msg(text=Env$voc[154,1],type="error")
	  }
	} else {
	  if (nchar(tclvalue(Env$l.var$varX.prop))>0 & nchar(tclvalue(Env$l.var$proportions))>0 & nchar(tclvalue(Env$l.var$prop.niveaux))>0) {
	    if (tclvalue(Env$l.var$plusieurs)==0) {
		if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
		  if (nchar(tclvalue(Env$l.var$niveau))>0) {
		    par(mar=c(5,5,4,2),bg="white")
		    tracer.courbe.propun()
		  } else {
		    msg(text=Env$voc[154,1],type="error")
		  }
		} else {
		  par(mar=c(5,5,4,2),bg="white")
		  tracer.courbe.propun()
		}
	    } else {
		par(mar=c(5,5,4,2),bg="white")
		tracer.courbe.propplusieurs()
	    }
	  } else {
	    msg(text=Env$voc[154,1],type="error")
	  }
	}
    } else
    if (Env$l.var$ecran=="N") {
	if (nchar(tclvalue(Env$l.var$varX))>0 & nchar(tclvalue(Env$l.var$varY))>0) {
	  if (tclvalue(Env$l.var$plusieurs)==0) {
	    if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
		if (nchar(tclvalue(Env$l.var$niveau))>0) {
		  par(mar=c(5,5,4,2),bg="white")
		  tracer.nuage.un()
		} else {
		  msg(text=Env$voc[154,1],type="error")
		}
	    } else {
		par(mar=c(5,5,4,2),bg="white")
		tracer.nuage.un()
	    }
	  } else {
	    par(mar=c(5,5,4,2),bg="white")
	    tracer.nuage.plusieurs()
	  }
	} else {
	  msg(text=Env$voc[154,1],type="error")
	}
    }
  } else {
    msg(text=Env$voc[153,1],type="error")
  }
}

