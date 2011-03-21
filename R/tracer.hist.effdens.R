tracer.hist.effdens <-
function(xinf,vect,nb.barres,xsup,ysup,type,col.bar,col.bor,x.leg,y.leg,size.leg,col.leg,col.ax,
  size.ax,distrib,type.distrib,col.distrib,ep.distrib) {
  if (xinf=="Auto") {
    xinf=min(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$breaks)
  } else {
    xinf=as.numeric(xinf)
  }
  if (xsup=="Auto") {
    xsup=max(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$breaks)
  } else {
    xsup=as.numeric(xsup)
  }
  if (ysup=="Auto") {
    ysup=if (type=="eff") {
	max(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts)
    } else {
	max(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$density)
    }
  } else {
    ysup=as.numeric(ysup)
  }
  hist(vect,freq=if (type=="eff") {TRUE} else {FALSE},main="",col=col.bar,border=col.bor,xlab=x.leg,ylab=y.leg,cex.lab=size.leg,col.lab=col.leg,axes=FALSE,
    xlim=c(xinf,xsup),ylim=c(0,ysup),breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)})
  axis(1,col=col.ax,col.axis=col.ax,cex.axis=size.ax)
  if (type=="dens" & distrib==1) {
    trait=if (type.distrib==Env$vocab[75,1]) {1} else if (type.distrib==Env$vocab[76,1]) {2} else {3}
    lines(density(vect,na.rm=TRUE),col=col.distrib,lwd=ep.distrib,lty=trait)
  }
}

