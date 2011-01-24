tracer.hist <-
function(var,fact,niv,type,xinf,xsup,ysup,nb.barres,col.bar,col.bor,x.leg,y.leg,size.leg,col.leg,titre,col.titre,size.titre,
  col.ax,size.ax,distrib,col.distrib,type.distrib,ep.distrib,fen) {
  if (nchar(var)>0) {
    vect=if (nchar(fact)>0 & fact!=Env$vocab[48,1]) {Env$datas.GrapheR[,var][which(Env$datas.GrapheR[,fact]==niv)]} else {Env$datas.GrapheR[,var]}
    sequence=c(min(vect,na.rm=TRUE),max(vect,na.rm=TRUE),abs(max(vect,na.rm=TRUE)-min(vect,na.rm=TRUE))/1000)
    if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
    if (type=="eff" | type=="dens") {
	if (xinf=="Auto") {xinf=min(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$breaks)} else {xinf=as.numeric(xinf)}
	if (xsup=="Auto") {xsup=max(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$breaks)} else {xsup=as.numeric(xsup)}
	if (ysup=="Auto") {ysup=if (type=="eff") {max(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts)} else 
        {max(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$density)}} else {ysup=as.numeric(ysup)}
      hist(vect,freq=if (type=="eff") {TRUE} else {FALSE},main="",col=col.bar,border=col.bor,xlab=x.leg,ylab=y.leg,cex.lab=size.leg,col.lab=col.leg,axes=FALSE,
	  xlim=c(xinf,xsup),ylim=c(0,ysup),breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)})
	title(main=titre,col.main=col.titre,cex.main=size.titre)
	axis(1,col=col.ax,col.axis=col.ax,cex.axis=size.ax)
	axis(2,col=col.ax,col.axis=col.ax,cex.axis=size.ax)
	if (type=="dens" & distrib==1) {
	  trait=if (type.distrib==Env$vocab[75,1]) {1} else if (type.distrib==Env$vocab[76,1]) {2} else {3}
	  lines(density(vect,na.rm=TRUE),col=col.distrib,lwd=ep.distrib,lty=trait)
	}
    } else {
	eff.tot=sum(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts)
	freq=hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts/eff.tot
	if (ysup=="Auto") {ysup=max(freq)} else {ysup=as.numeric(ysup)}
	barplot(freq,space=0,main="",col=col.bar,border=col.bor,
	  xlab=x.leg,ylab=y.leg,cex.lab=size.leg,col.lab=col.leg,axes=FALSE,
	  xlim=c(0,length(hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else {as.numeric(nb.barres)},plot=FALSE)$counts)),ylim=c(0,ysup))
	title(main=titre,col.main=col.titre,cex.main=size.titre)
	axis(1,col=col.ax,col.axis=col.ax,cex.axis=size.ax,labels=hist(vect,breaks=if (nb.barres=="Auto") {"Sturges"} else 
        {as.numeric(nb.barres)},plot=FALSE)$mids,at=0.5:length(freq)-0.5)
	axis(2,col=col.ax,col.axis=col.ax,cex.axis=size.ax)
    }
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="hist",type.hist=type,sequence=sequence,log.ax="",ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],type="ok",icon="error")}
}

