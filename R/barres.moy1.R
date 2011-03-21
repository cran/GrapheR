barres.moy1 <-
function(datas.GrapheR2,var,fact2,fact1,err.type,y.inf,beside,log.ax,y.sup,x.legende,y.legende,log.axes,col.ax,taille.ax,col.legende,
  taille.legende,noms1,col.bar,col.bor,dens.motif,ang.motif,col.motif,pos.leg,leg,leg.titre,noms2,err.col,err.seg) {
  moyennes.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),mean)
  list.err=calc.err1(datas.GrapheR2=datas.GrapheR2,err.type=err.type,var=var,fact2=fact2,fact1=fact1)
  barres.inf.mat=list.err$inf
  barres.sup.mat=list.err$sup
  ht.max=moyennes.mat+barres.sup.mat
  yinf=barres.yinf(y.inf=y.inf,val=moyennes.mat,err=barres.inf.mat,beside=beside,log.ax=log.ax)
  ysup=barres.ysup(y.sup=y.sup,val=moyennes.mat,err=barres.sup.mat,beside=beside,log.ax=log.ax)
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  graphe=barplot(moyennes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},xlab=x.legende,ylab=y.legende,
    ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,
    cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
  if (log.ax==0) {
    par(new=TRUE)
    barplot(moyennes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),
	log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
  }
  tracer.legende.1(pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,labels=noms2,couleurs=col.bar)
  if (any(barres.inf.mat!=0)==TRUE & beside==0) {barres.erreurs(graphe=graphe,val=moyennes.mat,err.inf=barres.inf.mat,err.sup=barres.sup.mat,err.col=err.col,err.seg=err.seg)}
  return(list(graphe=graphe,ht=ht.max))
}

