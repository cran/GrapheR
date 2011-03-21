nuage2 <-
function(nb.niv,var1,fact,niv,var2,x.inf,x.sup,y.inf,y.sup,x.legende,y.legende,log.axes,taille.legende,col.legende,col.symb,symbol,taille.symb,
  leg,pos.leg,leg.titre,noms,drt,tra,horiz,vertic,ep.lig,coefb,coefa,x.log,y.log,reg) {
  varx=NULL
  vary=NULL
  for (i in 1:nb.niv) {
    varx=c(varx,Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])
    vary=c(vary,Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])
  }
  xinf=if (x.inf=="Auto") {0.8*min(varx,na.rm=TRUE)} else {as.numeric(x.inf)}
  xsup=if (x.sup=="Auto") {1.1*max(varx,na.rm=TRUE)} else {as.numeric(x.sup)}
  yinf=if (y.inf=="Auto") {0.8*min(vary,na.rm=TRUE)} else {as.numeric(y.inf)}
  ysup=if (y.sup=="Auto") {1.1*max(vary,na.rm=TRUE)} else {as.numeric(y.sup)}
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  plot(Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][1])+1]]~Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][1])+1]],
    main="",xlab=x.legende,ylab=y.legende,log=log.axes,xlim=c(xinf,xsup),ylim=c(yinf,ysup),cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[1],pch=symbol[1],
    cex=taille.symb)
  if (nb.niv>1) {
    for (i in 2:nb.niv) {
	par(new=TRUE)
	plot(Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]~Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]],
	  main="",xlab="",ylab="",log=log.axes,xlim=c(xinf,xsup),ylim=c(yinf,ysup),xaxs="i",yaxs="i",axes=FALSE,col=col.symb[i],pch=symbol[i],cex=taille.symb)
    }
  }
  if (leg==1) {tracer.legende.3(pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,symbol=symbol,taille.symb=taille.symb,col.symb=col.symb)}
  if (any(drt!="aucune")==TRUE) {ajouter.droite2(nb.niv=nb.niv,tra=tra,drt=drt,horiz=horiz,col.symb=col.symb,ep.lig=ep.lig,vertic=vertic,coefb=coefb,
    coefa=coefa,x.log=x.log,y.log=y.log,reg=reg,var1=var1,fact=fact,niv=niv,var2=var2)}
}

