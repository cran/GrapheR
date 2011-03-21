courbes.moy1 <-
function(datas.GrapheR2,var2,var1,err.type,x.inf,x.sup,y.inf,y.sup,lig,tra,x.legende,y.legende,log.axes,taille.legende,col.legende,
  col.symb,symbol,taille.symb,ep.lig,err.seg) {
  moyennes=tapply(datas.GrapheR2[,var2],datas.GrapheR2[,var1],mean)
  list.err=calc.err2(datas.GrapheR2=datas.GrapheR2,err.type=err.type,var1=var2,var2=var1)
  barres.inf=list.err$inf
  barres.sup=list.err$sup
  list.coord=courbes.axes(x.inf=x.inf,var1=var1,x.sup=x.sup,y.inf=y.inf,val=moyennes,err.inf=barres.inf,y.sup=y.sup,err.sup=barres.sup)
  xinf=list.coord$xinf
  xsup=list.coord$xsup
  yinf=list.coord$yinf
  ysup=list.coord$ysup
  ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
    if (lig==Env$vocab[167,1]) {"o"} else {"h"}
  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  plot(moyennes~as.numeric(names(moyennes)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
    log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
    pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
  if (nchar(err.type)>0 & err.type!=Env$vocab[118,1]) {courbes.erreurs1(val=moyennes,barres.inf=barres.inf,barres.sup=barres.sup,col.symb=col.symb,
    trait=trait,err.seg=err.seg,xsup=xsup,xinf=xinf)}
}

