barres.som1 <-
function(datas.GrapheR2,var,fact2,fact1,y.inf,beside,log.ax,y.sup,x.legende,y.legende,log.axes,col.ax,taille.ax,
  col.legende,taille.legende,noms1,col.bar,col.bor,dens.motif,ang.motif,col.motif,pos.leg,leg,leg.titre,noms2) {
  sommes.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),sum)
  ht.max=sommes.mat
  yinf=barres.yinf(y.inf=y.inf,val=sommes.mat,err=NULL,beside=beside,log.ax=log.ax)
  ysup=barres.ysup(y.sup=y.sup,val=sommes.mat,err=NULL,beside=beside,log.ax=log.ax)
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  graphe=barplot(sommes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},xlab=x.legende,ylab=y.legende,ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,
    cex.axis=taille.ax,col.lab=col.legende,cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
  if (log.ax==0) {
    par(new=TRUE)
    barplot(sommes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),
	log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
  }
  tracer.legende.1(pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,labels=noms2,couleurs=col.bar)
  return(list(graphe=graphe,ht=ht.max))
}

