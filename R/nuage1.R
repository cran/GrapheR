nuage1 <-
function(x.inf,var1,x.sup,y.inf,var2,y.sup,x.legende,y.legende,log.axes,taille.legende,col.legende,col.symb,symbol,taille.symb,
  drt,tra,horiz,vertic,ep.lig,coefb,coefa,x.log,y.log,reg) {
  xinf=if (x.inf=="Auto") {0.8*min(Env$datas.GrapheR[,var1],na.rm=TRUE)} else {as.numeric(x.inf)}
  xsup=if (x.sup=="Auto") {1.1*max(Env$datas.GrapheR[,var1],na.rm=TRUE)} else {as.numeric(x.sup)}
  yinf=if (y.inf=="Auto") {0.8*min(Env$datas.GrapheR[,var2],na.rm=TRUE)} else {as.numeric(y.inf)}
  ysup=if (y.sup=="Auto") {1.1*max(Env$datas.GrapheR[,var2],na.rm=TRUE)} else {as.numeric(y.sup)}
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  plot(Env$datas.GrapheR[,var2]~Env$datas.GrapheR[,var1],main="",xlab=x.legende,ylab=y.legende,log=log.axes,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
    cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,pch=symbol,cex=taille.symb)
  if (drt!="aucune") {ajouter.droite1(tra=tra,drt=drt,horiz=horiz,col.symb=col.symb,ep.lig=ep.lig,vertic=vertic,coefb=coefb,coefa=coefa,x.log=x.log,
    y.log=y.log,reg=reg,var1=var1,var2=var2)}
}

