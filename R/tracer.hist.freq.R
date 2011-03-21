tracer.hist.freq <-
function(vect,nb.barres,ysup,col.bar,col.bor,x.leg,y.leg,size.leg,col.leg,col.ax,size.ax) {
  eff.tot=sum(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts)
  freq=hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts/eff.tot
  if (ysup=="Auto") {
    ysup=max(freq)
  } else {
    ysup=as.numeric(ysup)
  }
  barplot(freq,space=0,main="",col=col.bar,border=col.bor,
    xlab=x.leg,ylab=y.leg,cex.lab=size.leg,col.lab=col.leg,axes=FALSE,
    xlim=c(0,length(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts)),ylim=c(0,ysup))
  axis(1,col=col.ax,col.axis=col.ax,cex.axis=size.ax,labels=hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else 
    {as.numeric(nb.barres)},plot=FALSE)$mids,at=0.5:length(freq)-0.5)
}

