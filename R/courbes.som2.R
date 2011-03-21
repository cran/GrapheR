courbes.som2 <-
function(datas.GrapheR2,var1,fact,nivx,var2,x.inf,x.sup,y.inf,y.sup,lig,tra,x.legende,y.legende,log.axes,taille.legende,
  col.legende,col.symb,symbol,taille.symb,ep.lig,nb.niv,pos.leg,leg.titre,noms,leg) {
  x=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(datas.GrapheR2[,fact])[nivx]]
  y=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(datas.GrapheR2[,fact])[nivx]]
  sommes=tapply(y,x,sum)
  list.coord=courbes.axes(x.inf=x.inf,var1=var1,x.sup=x.sup,y.inf=y.inf,val=sommes,err.inf=NULL,y.sup=y.sup,err.sup=NULL)
  xinf=list.coord$xinf
  xsup=list.coord$xsup
  yinf=list.coord$yinf
  ysup=list.coord$ysup
  ligne=if (lig==Env$vocab[164,1]) {"p"} else if (lig==Env$vocab[165,1]) {"l"} else if (lig==Env$vocab[166,1]) {"b"} else 
    if (lig==Env$vocab[167,1]) {"o"} else {"h"}
  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  plot(sommes~as.numeric(names(sommes)),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
    log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,
    pch=symbol,cex=taille.symb,type=ligne,lty=trait,lwd=ep.lig)
  if (leg==1) {tracer.legende.2(nb.niv=nb.niv,ligne=ligne,symbol=symbol,trait=trait,pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,taille.symb=taille.symb,col.symb=col.symb,ep.lig=ep.lig)}
}

