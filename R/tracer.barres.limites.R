tracer.barres.limites <-
function(valeurs,erreur.inf,erreur.sup) {
  y.inf<-if(tclvalue(Env$l.var$liminf.axever)=="Auto") {
    if (any(valeurs-erreur.inf<0)) {
	if (tclvalue(Env$l.var$plusieurs)==1) {
	  if (tclvalue(Env$l.var$stack)==1) {
	    1.2*min(colSums(valeurs))
	  } else {
	    1.2*min(valeurs-erreur.inf)
	  }
	} else {
	  1.2*min(valeurs-erreur.inf)
	}
    } else {
	if (tclvalue(Env$l.var$log.axever)==1) {
	  0.01
	} else {
	  0
	}
    }
  } else {
    as.numeric(tclvalue(Env$l.var$liminf.axever))
  }
  y.sup<-if(tclvalue(Env$l.var$limsup.axever)=="Auto") {
    if (any(valeurs+erreur.sup>0)) {
	if (tclvalue(Env$l.var$plusieurs)==1) {
	  if (tclvalue(Env$l.var$stack)==1) {
	    1.2*max(colSums(valeurs))
	  } else {
	    1.2*max(valeurs+erreur.sup)
	  }
	} else {
	  1.2*max(valeurs+erreur.sup)
	}
    } else {
	if (tclvalue(Env$l.var$log.axever)==1) {
	  -0.01
	} else {
	  0
	}
    }
  } else {
    as.numeric(tclvalue(Env$l.var$limsup.axever))
  }
  ordonnee<-if (y.inf>=0 & y.sup>0) {
    y.inf
  } else if (y.inf<0 & y.sup>0){
    0
  } else if (y.inf<0 & y.sup<=0) {
    y.sup
  }
  return(list(yinf=y.inf,ysup=y.sup,ordonnee=ordonnee))
}
