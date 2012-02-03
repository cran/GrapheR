tracer.hist.limites <-
function(variable,type,frequence=NULL) {
  x.inf<-if (type=="freq") {
    0
  } else if (type=="eff" | type=="dens") {
    if (tclvalue(Env$l.var$liminf.axehor)=="Auto") {
	min(hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),plot=FALSE)$breaks)
    } else {
	as.numeric(tclvalue(Env$l.var$liminf.axehor))
    }
  }
  x.sup<-if (type=="freq") {
    length(hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),plot=FALSE)$counts)
  } else if (type=="eff" | type=="dens") {
    if (tclvalue(Env$l.var$limsup.axehor)=="Auto") {
	max(hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),plot=FALSE)$breaks)
    } else {
	as.numeric(tclvalue(Env$l.var$limsup.axehor))
    }
  }
  y.sup<-if (type=="freq") {
    if (tclvalue(Env$l.var$limsup.axever)=="Auto") {
	1.1*max(frequence)
    } else {
	as.numeric(tclvalue(Env$l.var$limsup.axever))
    }
  } else if (type=="eff") {
    if (tclvalue(Env$l.var$limsup.axever)=="Auto") {
	max(hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),plot=FALSE)$counts)
    } else {
	as.numeric(tclvalue(Env$l.var$limsup.axever))
    }
  } else if (type=="dens") {
    if (tclvalue(Env$l.var$limsup.axever)=="Auto") {
	max(hist(variable,breaks=ifelse(tclvalue(Env$l.var$hist.barres)=="Auto","Sturges",as.numeric(tclvalue(Env$l.var$hist.barres))-1),plot=FALSE)$density)
    } else {
	as.numeric(tclvalue(Env$l.var$limsup.axever))
    }
  }
  return(list(xinf=x.inf,xsup=x.sup,ysup=y.sup))
}
