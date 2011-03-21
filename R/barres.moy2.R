barres.moy2 <-
function(datas.GrapheR2,var,fact1,err.type,y.inf,y.sup,beside,log.ax,x.legende,y.legende,log.axes,col.ax,taille.ax,col.legende,taille.legende,
  noms1,col.bar,col.bor,dens.motif,ang.motif,col.motif,err.col,err.seg) {
  moyennes=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],mean)
  list.err=calc.err2(datas.GrapheR2=datas.GrapheR2,err.type=err.type,var1=var,var2=fact1)
  barres.inf=list.err$inf
  barres.sup=list.err$sup
  ht.max=moyennes+barres.sup
  yinf=barres.yinf(y.inf=y.inf,val=moyennes,err=NULL,beside=NULL,log.ax=log.ax)
  ysup=barres.ysup(y.sup=y.sup,val=moyennes,err=NULL,beside=NULL,log.ax=log.ax)
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  graphe=barplot(moyennes,main="",xlab=x.legende,ylab=y.legende,ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,cex.lab=taille.legende,
    cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
  if (log.ax==0) {
    par(new=TRUE)
    barplot(moyennes,main="",axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
  }
  if (any(barres.inf!=0)==TRUE) {barres.erreurs(graphe=graphe,val=moyennes,err.inf=barres.inf,err.sup=barres.sup,err.col=err.col,err.seg=err.seg)}
  return(list(graphe=graphe,ht=ht.max))
}

