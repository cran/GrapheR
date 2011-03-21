barres.prop2B <-
function(datas.GrapheR2,y.inf,log.ax,y.sup,beside,proportions.mat,barres.sup.mat,x.legende,y.legende,log.axes,col.ax,taille.ax,col.legende,taille.legende,
  noms1,col.bar,col.bor,fact1,dens.motif,ang.motif,col.motif,pos.leg,leg,leg.titre,noms2,barres.inf.mat,err.col,err.seg) {
  yinf=if (y.inf=="Auto") {if (log.ax==1) {0.01} else {0}} else {as.numeric(y.inf)}
  ysup=if (y.sup=="Auto") {if (beside==1) {1.2*max(colSums(proportions.mat))} else {1.2*max(proportions.mat+barres.sup.mat)}} else {as.numeric(y.sup)}
  graphe=barplot(proportions.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},xlab=x.legende,ylab=y.legende,
    ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,
    cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
  if (log.ax==0) {
    par(new=TRUE)
    barplot(proportions.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),
	log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
  }
  tracer.legende.1(pos.leg=pos.leg,leg=leg,leg.titre=leg.titre,labels=noms2,couleurs=col.bar)
  if (any(barres.inf.mat!=0)==TRUE & beside==0) {barres.erreurs(graphe=graphe,val=proportions.mat,err.inf=barres.inf.mat,err.sup=barres.sup.mat,err.col=err.col,err.seg=err.seg)}
  return(graphe)
}

