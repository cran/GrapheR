tracer.nuage.limites <-
function(varX,varY) {
  x.inf<-if(tclvalue(Env$l.var$liminf.axehor)=="Auto") {
    0.8*min(varX,na.rm=TRUE)
  } else {
    as.numeric(tclvalue(Env$l.var$liminf.axehor))
  }
  x.sup<-if(tclvalue(Env$l.var$limsup.axehor)=="Auto") {
    1.1*max(varX,na.rm=TRUE)
  } else {
    as.numeric(tclvalue(Env$l.var$limsup.axehor))
  }
  y.inf<-if(tclvalue(Env$l.var$liminf.axever)=="Auto") {
    0.8*min(varY,na.rm=TRUE)
  } else {
    as.numeric(tclvalue(Env$l.var$liminf.axever))
  }
  y.sup<-if(tclvalue(Env$l.var$limsup.axever)=="Auto") {
    1.1*max(varY,na.rm=TRUE)
  } else {
    as.numeric(tclvalue(Env$l.var$limsup.axever))
  }
  return(list(xinf=x.inf,xsup=x.sup,yinf=y.inf,ysup=y.sup))
}

