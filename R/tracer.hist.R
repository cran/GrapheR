tracer.hist <-
function(var,fact,niv,type,xinf,xsup,ysup,nb.barres,col.bar,col.bor,x.leg,y.leg,size.leg,col.leg,titre,col.titre,size.titre,
  col.ax,size.ax,distrib,col.distrib,type.distrib,ep.distrib,fen) {
  if (nchar(var)>0) {
    vect=if (nchar(fact)>0 & fact!=Env$vocab[48,1]) {Env$datas.GrapheR[,var][which(Env$datas.GrapheR[,fact]==niv)]} else {Env$datas.GrapheR[,var]}
    sequence=c(min(vect,na.rm=TRUE),max(vect,na.rm=TRUE),abs(max(vect,na.rm=TRUE)-min(vect,na.rm=TRUE))/1000)
    if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
    if (type=="eff" | type=="dens") {
	tracer.hist.effdens(xinf=xinf,vect=vect,nb.barres=nb.barres,xsup=xsup,ysup=ysup,type=type,col.bar=col.bar,col.bor=col.bor,x.leg=x.leg,y.leg=y.leg,
	  size.leg=size.leg,col.leg=col.leg,col.ax=col.ax,size.ax=size.ax,distrib=distrib,type.distrib=type.distrib,col.distrib=col.distrib,ep.distrib=ep.distrib)
    } else {
	tracer.hist.freq(vect=vect,nb.barres=nb.barres,ysup=ysup,col.bar=col.bar,col.bor=col.bor,x.leg=x.leg,y.leg=y.leg,size.leg=size.leg,
	  col.leg=col.leg,col.ax=col.ax,size.ax=size.ax)
    }
    title(main=titre,col.main=col.titre,cex.main=size.titre)
    axis(2,col=col.ax,col.axis=col.ax,cex.axis=size.ax)
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="hist",type.hist=type,sequence=sequence,log.ax="",ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],type="ok",icon="error")}
}

