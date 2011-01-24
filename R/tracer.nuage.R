tracer.nuage <-
function(var1,var2,fact,nb.niv,niv,encadre,titre,col.titre,taille.titre,col.ax,taille.ax,col.legende,taille.legende,x.legende,y.legende,x.inf,x.sup,
  y.inf,y.sup,x.log,y.log,symbol,col.symb,taille.symb,drt,reg,horiz,vertic,coefa,coefb,tra,ep.lig,leg,pos.leg,noms,leg.titre,fen) {
  if (nchar(var1)>0 & nchar(var2)>0) {
    log.axes=if (x.log==1) {if (y.log==1) {"xy"} else {"x"}} else {if (y.log==1) {"y"} else {""}}
    if (nchar(fact)==0 | fact==Env$vocab[48,1]) {
	xinf=if (x.inf=="Auto") {0.8*min(na.omit(Env$datas.GrapheR[,var1]))} else {as.numeric(x.inf)}
	xsup=if (x.sup=="Auto") {1.1*max(na.omit(Env$datas.GrapheR[,var1]))} else {as.numeric(x.sup)}
	yinf=if (y.inf=="Auto") {0.8*min(na.omit(Env$datas.GrapheR[,var2]))} else {as.numeric(y.inf)}
	ysup=if (y.sup=="Auto") {1.1*max(na.omit(Env$datas.GrapheR[,var2]))} else {as.numeric(y.sup)}
      if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	plot(Env$datas.GrapheR[,var2]~Env$datas.GrapheR[,var1],main="",xlab=x.legende,ylab=y.legende,log=log.axes,xlim=c(xinf,xsup),ylim=c(yinf,ysup),
	  cex.lab=taille.legende,col.lab=col.legende,xaxs="i",yaxs="i",axes=FALSE,col=col.symb,pch=symbol,cex=taille.symb)
	title(main=titre,col.main=col.titre,cex.main=taille.titre)
	axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	if (encadre==1) {box()}
	if (drt!="aucune") {
	  trait=if (tra==Env$vocab[75,1]) {1} else if (tra==Env$vocab[76,1]) {2} else {3}
	  if (drt=="hor") {abline(h=horiz,col=col.symb,lty=trait,lwd=ep.lig)}
	  if (drt=="ver") {abline(v=vertic,col=col.symb,lty=trait,lwd=ep.lig)}
	  if (drt=="affine") {
	    abline(coefb,coefa,col=col.symb,lty=trait,lwd=ep.lig,untf=if (x.log==1 | y.log==1) {TRUE} else {FALSE})
	  }
	  if (drt=="reg" & reg=="mc") {
	    x=if (x.log==1) {log10(Env$datas.GrapheR[,var1])} else {Env$datas.GrapheR[,var1]}
	    y=if (y.log==1) {log10(Env$datas.GrapheR[,var2])} else {Env$datas.GrapheR[,var2]}
	    abline(lm(y~x),col=col.symb,lty=trait,lwd=ep.lig)
	  }
	  if (drt=="reg" & reg=="mr") {
	    x=if (x.log==1) {log10(Env$datas.GrapheR[,var1])} else {Env$datas.GrapheR[,var1]}
	    y=if (y.log==1) {log10(Env$datas.GrapheR[,var2])} else {Env$datas.GrapheR[,var2]}
	    a=sqrt(var(y,na.rm=TRUE)/var(x,na.rm=TRUE))*sign(cov(x,y,use="complete.obs"))
	    b=mean(y,na.rm=TRUE)-a*mean(x,na.rm=TRUE)
	    abline(b,a,col=col.symb,lty=trait,lwd=ep.lig)
	  }
	  if (drt=="quadra") {
	    x=Env$datas.GrapheR[,var1]
	    y=Env$datas.GrapheR[,var2]
	    regress=lm(y~x+I(x^2))
	    x2=seq(min(na.omit(x)),max(na.omit(x)),0.01)
	    y2=predict(regress,list(x=x2))
	    lines(x2,y2,col=col.symb,lty=trait,lwd=ep.lig)
	  }
	  if (drt=="gam") {
	    x=Env$datas.GrapheR[,var1]
	    y=Env$datas.GrapheR[,var2]
	    model=gam(y~s(x))
	    x2=seq(min(na.omit(x)),max(na.omit(x)),0.01)
	    y2=predict(model,list(x=x2))
	    lines(x2,y2,col=col.symb,lty=trait,lwd=ep.lig)
	  }
	}
    } else {
	varx=NULL
	vary=NULL
	for (i in 1:nb.niv) {
	  varx=c(varx,Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])
	  vary=c(vary,Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])
	}
	xinf=if (x.inf=="Auto") {0.8*min(na.omit(varx))} else {as.numeric(x.inf)}
	xsup=if (x.sup=="Auto") {1.1*max(na.omit(varx))} else {as.numeric(x.sup)}
	yinf=if (y.inf=="Auto") {0.8*min(na.omit(vary))} else {as.numeric(y.inf)}
	ysup=if (y.sup=="Auto") {1.1*max(na.omit(vary))} else {as.numeric(y.sup)}
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
	title(main=titre,col.main=col.titre,cex.main=taille.titre)
	axis(1,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	axis(2,col=col.ax,col.axis=col.ax,cex.axis=taille.ax)
	if (encadre==1) {box()}
	if (leg==1) {
	  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
	  if (nchar(leg.titre)==0) {legend(position.legende,legend=noms,pch=symbol,pt.cex=taille.symb,col=col.symb)} else {
	    legend(position.legende,legend=noms,pch=symbol,pt.cex=taille.symb,col=col.symb,title=leg.titre)}
	}
	if (any(drt!="aucune")==TRUE) {
	  for (i in 1:nb.niv) {
	    trait=if (tra[i]==Env$vocab[75,1]) {1} else if (tra[i]==Env$vocab[76,1]) {2} else {3}
	    if (drt[i]=="hor") {abline(h=horiz[i],col=col.symb[i],lty=trait,lwd=ep.lig[i])}
	    if (drt[i]=="ver") {abline(v=vertic[i],col=col.symb[i],lty=trait,lwd=ep.lig[i])}
	    if (drt[i]=="affine") {abline(coefb[i],coefa[i],col=col.symb[i],lty=trait,lwd=ep.lig[i],untf=if (x.log==1 | y.log==1) {TRUE} else {FALSE})}
	    if (drt[i]=="reg" & reg[i]=="mc") {
	      x=if (x.log==1) {log10(Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	      y=if (y.log==1) {log10(Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
		abline(lm(y~x),col=col.symb[i],lty=trait,lwd=ep.lig[i])}
	    if (drt[i]=="reg" & reg[i]=="mr") {
	      x=if (x.log==1) {log10(Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	      y=if (y.log==1) {log10(Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]])} else {Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]}
	      a=sqrt(var(y,na.rm=TRUE)/var(x,na.rm=TRUE))*sign(cov(x,y,use="complete.obs"))
	      b=mean(y,na.rm=TRUE)-a*mean(x,na.rm=TRUE)
	      abline(b,a,col=col.symb[i],lty=trait,lwd=ep.lig[i])
	    }
	    if (drt[i]=="quadra") {
	      x=Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	      y=Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	      regress=lm(y~x+I(x^2))
	      x2=seq(min(na.omit(x)),max(na.omit(x)),0.01)
	      y2=predict(regress,list(x=x2))
	      lines(x2,y2,col=col.symb[i],lty=trait,lwd=ep.lig[i])
	    }
	    if (drt[i]=="gam") {
	      x=Env$datas.GrapheR[,var1][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	      y=Env$datas.GrapheR[,var2][Env$datas.GrapheR[,fact]==levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(niv,split=" ")[[1]][i])+1]]
	      model=gam(y~s(x))
	      x2=seq(min(na.omit(x)),max(na.omit(x)),0.01)
	      y2=predict(model,list(x=x2))
	      lines(x2,y2,col=col.symb[i],lty=trait,lwd=ep.lig[i])
	    }
	  }
	}
    }
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="nuage",type.hist="",sequence="",log.ax=log.axes,ht="",abs="")
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[157,1],icon="error",type="ok")}
}

