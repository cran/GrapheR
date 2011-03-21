courbes.som3 <-
function(datas.GrapheR2,nb.niv,var1,fact,nivx,var2,lig,tra,x.inf,x.sup,y.inf,y.sup,x.legende,y.legende,log.axes,taille.legende,
  col.legende,col.symb,symbol,taille.symb,ep.lig,pos.leg,leg.titre,noms,leg) {
  ligne=integer(nb.niv)
  trait=integer(nb.niv)
  liste.sommes=list()
  for(i in 1:nb.niv) {
    x=datas.GrapheR2[,var1][datas.GrapheR2[,fact]==levels(datas.GrapheR2[,fact])[nivx[i]]]
    y=datas.GrapheR2[,var2][datas.GrapheR2[,fact]==levels(datas.GrapheR2[,fact])[nivx[i]]]		
    liste.sommes[[i]]=tapply(y,x,sum)
    ligne[i]=if (lig[i]==Env$vocab[164,1]) {"p"} else if (lig[i]==Env$vocab[165,1]) {"l"} else if (lig[i]==Env$vocab[166,1]) {"b"} else 
	if (lig[i]==Env$vocab[167,1]) {"o"} else {"h"}
    trait[i]=if (tra[i]==Env$vocab[75,1]) {1} else if (tra[i]==Env$vocab[76,1]) {2} else {3}
  }
  max=integer(liste.sommes)
  min=integer(liste.sommes)
  for (i in 1:length(liste.sommes)) {
    max[i]=max(liste.sommes[[i]])
    min[i]=min(liste.sommes[[i]])
  }
  xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
  xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
  yinf=if (y.inf=="Auto") {0.8*min(min)} else {as.numeric(y.inf)}
  ysup=if (y.sup=="Auto") {1.1*max(max)} else {as.numeric(y.sup)}
  if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
  plot(liste.sommes[[1]]~as.numeric(names(liste.sommes[[1]])),main="",xlab=x.legende,ylab=y.legende,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
    log=log.axes,cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[1],
    pch=symbol[1],cex=taille.symb,type=ligne[1],lty=trait[1],lwd=ep.lig[1])
  for (i in 2:nb.niv) {
    par(new=TRUE)
    plot(liste.sommes[[i]]~as.numeric(names(liste.sommes[[i]])),main="",xlab="",ylab="",xlim=c(xinf,xsup),ylim=c(yinf,ysup),
    log=log.axes,xaxs="i",yaxs="i",axes=FALSE,col=col.symb[i],pch=symbol[i],cex=taille.symb,type=ligne[i],lty=trait[i],lwd=ep.lig[i])
  }
  if (leg==1) {tracer.legende.2(nb.niv=nb.niv,ligne=ligne,symbol=symbol,trait=trait,pos.leg=pos.leg,leg.titre=leg.titre,noms=noms,taille.symb=taille.symb,col.symb=col.symb,ep.lig=ep.lig)}
}

