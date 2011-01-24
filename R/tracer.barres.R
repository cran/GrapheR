tracer.barres <-
function(var,fact1,fact2,type,niv.prop,y.inf,y.sup,beside,x.legende,y.legende,col.ax,taille.ax,log.ax,col.legende,taille.legende,noms1,col.bar,col.bor,dens.motif,ang.motif,
  col.motif,titre,col.titre,taille.titre,leg,leg.titre,pos.leg,noms2,err.type,err.seg,err.col,fen) {
  if (nchar(var)>0 & nchar(fact1)>0) {
    log.axes=if (log.ax==1) {"y"} else {""}
    ht.max=NULL
    graphe=NULL
    na=which(is.na(Env$datas.GrapheR[,var])==TRUE)
    datas.GrapheR2=if (length(na)!=0) {Env$datas.GrapheR[-na,]} else {Env$datas.GrapheR}
    if (type=="som") {
      if (nchar(fact2)>0 & fact2!=Env$vocab[48,1]) {
	  sommes.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),sum)
	  ht.max=sommes.mat
	  yinf=if (y.inf=="Auto") {
	      if (any(sommes.mat<0)==TRUE) {
		  if (beside==0) {1.2*min(sommes.mat)} else {1.2*min(colSums(sommes.mat))}
		} else {
		  if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {
		if (any(sommes.mat>0)==TRUE) {
		  if (beside==1) {1.2*max(colSums(sommes.mat))} else {1.2*max(sommes.mat)}
		} else {
		  if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.sup)}
	  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  graphe=barplot(sommes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},xlab=x.legende,ylab=y.legende,ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,
	    cex.axis=taille.ax,col.lab=col.legende,cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
	  if (log.ax==0) {
	    par(new=TRUE)
	    barplot(sommes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),
	      log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  if (leg==1) {
	    if (nchar(leg.titre)==0) {legend(position.legende,legend=noms2,fill=col.bar)} else {
	      legend(position.legende,legend=noms2,fill=col.bar,title=leg.titre)}
	  }
      } else {
	  sommes=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],sum)
	  ht.max=sommes
	  yinf=if (y.inf=="Auto") {
	      if (any(sommes<0)==TRUE) {1.2*min(sommes)} else {
		  if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {
		if (any(sommes>0)==TRUE) {1.2*max(sommes)} else {
		  if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.sup)}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  graphe=barplot(sommes,main="",xlab=x.legende,ylab=y.legende,ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,
	    cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
	  if (log.ax==0) {
	    par(new=TRUE)
	    barplot(sommes,main="",legend=FALSE,axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),log=log.axes,
	      density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	}
    } else if (type=="moy") {
      if (nchar(fact2)>0 & fact2!=Env$vocab[48,1]) {
	  moyennes.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),mean)
	  if (err.type==Env$vocab[119,1]) {
	    barres.inf.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),sd)
	    barres.sup.mat=barres.inf.mat
	  } else if (err.type==Env$vocab[120,1]) {
	    barres.inf.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),function(x) sd(x)/sqrt(length(x)))
	    barres.sup.mat=barres.inf.mat
	  } else if (err.type==Env$vocab[121,1]) {
	    if (any(tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),length)<30)==TRUE) {
	      barres.inf.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),function(x) mean(x)-wilcox.test(x,conf.int=TRUE)$conf.int[1])
	      barres.sup.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2]-mean(x))
	    } else {
	      barres.inf.mat=tapply(datas.GrapheR2[,var],list(datas.GrapheR2[,fact2],datas.GrapheR2[,fact1]),function(x) mean(x)-t.test(x)$conf.int[1])
	      barres.sup.mat=barres.inf.mat
	    }
	  } else {
	    barres.inf.mat=matrix(rep(0,nlevels(datas.GrapheR2[,fact1])*nlevels(datas.GrapheR2[,fact2])),nrow=nlevels(datas.GrapheR2[,fact2]),dimnames=list(levels(datas.GrapheR2[,fact2]),levels(datas.GrapheR2[,fact1])))
	    barres.sup.mat=barres.inf.mat
	  }
	  ht.max=moyennes.mat+barres.sup.mat
	  yinf=if (y.inf=="Auto") {
	      if (any(moyennes.mat<0)==TRUE) {
		  if (beside==1) {1.2*min(colSums(moyennes.mat))} else {1.2*min(moyennes.mat-barres.inf.mat)}
		} else {
		  if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {
		if (any(moyennes.mat>0)==TRUE) {
		  if (beside==1) {1.2*max(colSums(moyennes.mat))} else {1.2*max(moyennes.mat+barres.sup.mat)}
		} else {
		  if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.sup)}
	  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  graphe=barplot(moyennes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},xlab=x.legende,ylab=y.legende,
	    ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,
	    cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
	  if (log.ax==0) {
	    par(new=TRUE)
	    barplot(moyennes.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),
	      log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  if (leg==1) {
	    if (nchar(leg.titre)==0) {legend(position.legende,legend=noms2,fill=col.bar)} else {
	      legend(position.legende,legend=noms2,fill=col.bar,title=leg.titre)}
	  }
	  if (any(barres.inf.mat!=0)==TRUE & beside==0) {
	    segments(graphe,moyennes.mat-barres.inf.mat,graphe,moyennes.mat+barres.sup.mat,col=err.col)
	    if (err.seg==1) {
		segments(graphe-0.1,moyennes.mat-barres.inf.mat,graphe+0.1,moyennes.mat-barres.inf.mat,col=err.col)
		segments(graphe-0.1,moyennes.mat+barres.sup.mat,graphe+0.1,moyennes.mat+barres.sup.mat,col=err.col)
	    }
	  }
	} else {
	  moyennes=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],mean)
	  if (err.type==Env$vocab[119,1]) {
	    barres.inf=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],sd)
	    barres.sup=barres.inf
	  } else if (err.type==Env$vocab[120,1]) {
	    barres.inf=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],function(x) sd(x)/sqrt(length(x)))
	    barres.sup=barres.inf
	  } else if (err.type==Env$vocab[121,1]) {
	    if (any(tapply(Env$datas.GrapheR[,var],Env$datas.GrapheR[,fact1],length)<30)==TRUE) {
	      barres.inf=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],function(x) mean(x)-wilcox.test(x,conf.int=TRUE)$conf.int[1])
	      barres.sup=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],function(x) wilcox.test(x,conf.int=TRUE)$conf.int[2]-mean(x))
	    } else {
	      barres.inf=tapply(datas.GrapheR2[,var],datas.GrapheR2[,fact1],function(x) mean(x)-t.test(x)$conf.int[1])
	      barres.sup=barres.inf
	    }
	  } else {barres.inf=rep(0,nlevels(Env$datas.GrapheR[,fact1]));barres.sup=barres.inf}
	  ht.max=moyennes+barres.sup
	  yinf=if (y.inf=="Auto") {
	      if (any(moyennes<0)==TRUE) {1.2*min(moyennes-barres.inf)} else {if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {
		if (any(moyennes>0)==TRUE) {1.2*max(moyennes+barres.sup)} else {if (log.ax==1) {0.01} else {0}}
	    } else {as.numeric(y.sup)}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  graphe=barplot(moyennes,main="",xlab=x.legende,ylab=y.legende,ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,cex.lab=taille.legende,
	    cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
	  if (log.ax==0) {
	    par(new=TRUE)
	    barplot(moyennes,main="",axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  if (any(barres.inf!=0)==TRUE) {
	    segments(graphe,moyennes-barres.inf,graphe,moyennes+barres.sup,col=err.col)
	    if (err.seg==1) {
		segments(graphe-0.1,moyennes-barres.inf,graphe+0.1,moyennes-barres.inf,col=err.col)
		segments(graphe-0.1,moyennes+barres.sup,graphe+0.1,moyennes+barres.sup,col=err.col)
	    }
	  }
	}
    } else if (type=="prop") {
	niv.prop2=as.numeric(strsplit(niv.prop,split=" ")[[1]])
	proportions=NULL
	for (i in 1:nlevels(datas.GrapheR2[,fact1])) {
	  for (j in 1:length(niv.prop2)) {proportions=c(proportions,length(datas.GrapheR2[,var][datas.GrapheR2[,var]==levels(datas.GrapheR2[,var])[niv.prop2[j]+1] & 
	    datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]])/length(datas.GrapheR2[,var][datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]))}
	}
	proportions.mat=matrix(proportions,nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
	barres.inf=NULL
	barres.sup=NULL
	if (err.type==Env$vocab[121,1]) {
	  for (i in 1:nlevels(datas.GrapheR2[,fact1])) {
	    for (j in 1:length(niv.prop2)) {
	      barres.inf=c(barres.inf,proportions.mat[j,i]-binom.test(length(datas.GrapheR2[,var][datas.GrapheR2[,var]==levels(datas.GrapheR2[,var])[niv.prop2[j]+1] & 
	        datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]),length(datas.GrapheR2[,var][datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]))$conf.int[1])
	      barres.sup=c(barres.sup,binom.test(length(datas.GrapheR2[,var][datas.GrapheR2[,var]==levels(datas.GrapheR2[,var])[niv.prop2[j]+1] & 
	        datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]),length(datas.GrapheR2[,var][datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]))$conf.int[2]-proportions.mat[j,i])
	    }
	  }
	barres.inf.mat=matrix(barres.inf,nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
	barres.sup.mat=matrix(barres.sup,nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
	} else {
	barres.inf.mat=matrix(rep(0,length(niv.prop2)*nlevels(datas.GrapheR2[,fact1])),nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
	barres.sup.mat=barres.inf.mat
	}
	ht.max=proportions.mat+barres.sup.mat
      if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	if (nchar(niv.prop)==1) {
	  yinf=if (y.inf=="Auto") {if (log.ax==1) {0.01} else {0}} else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {1.2*max(proportions.mat+barres.sup.mat)} else {as.numeric(y.sup)}
	  graphe=barplot(proportions,main="",xlab=x.legende,ylab=y.legende,ylim=c(yinf,ysup),log=log.axes,
	    col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,cex.lab=taille.legende,cex.names=taille.ax,
	    names.arg=noms1,col=col.bar,border=col.bor)
	  if (log.ax==0) {
	    par(new=TRUE)
	    barplot(proportions,main="",axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),log=log.axes,
	      density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  if (any(barres.inf!=0)==TRUE) {
	    segments(graphe,proportions.mat-barres.inf.mat,graphe,proportions.mat+barres.sup.mat,col=err.col)
	    if (err.seg==1) {
		segments(graphe-0.1,proportions.mat-barres.inf.mat,graphe+0.1,proportions.mat-barres.inf.mat,col=err.col)
		segments(graphe-0.1,proportions.mat+barres.sup.mat,graphe+0.1,proportions.mat+barres.sup.mat,col=err.col)
	    }
	  }
	} else {
	  yinf=if (y.inf=="Auto") {if (log.ax==1) {0.01} else {0}} else {as.numeric(y.inf)}
	  ysup=if (y.sup=="Auto") {if (beside==1) {1.2*max(colSums(proportions.mat))} else {1.2*max(proportions.mat+barres.sup.mat)}} else {as.numeric(y.sup)}
	  position.legende=if (pos.leg==Env$vocab[126,1]) {"topleft"} else if (pos.leg==Env$vocab[127,1]) {"top"} else if (pos.leg==Env$vocab[128,1]) {"topright"} else
	    if (pos.leg==Env$vocab[129,1]) {"left"} else if (pos.leg==Env$vocab[130,1]) {"center"} else if (pos.leg==Env$vocab[131,1]) {"right"} else
	    if (pos.leg==Env$vocab[132,1]) {"bottomleft"} else if (pos.leg==Env$vocab[133,1]) {"bottom"} else if (pos.leg==Env$vocab[134,1]) {"bottomright"}
        if (is.numeric(dev.list()[1])==FALSE) {par(bg="white",mar=c(5,6,4,2))}
	  graphe=barplot(proportions.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},xlab=x.legende,ylab=y.legende,
	    ylim=c(yinf,ysup),log=log.axes,col.axis=col.ax,cex.axis=taille.ax,col.lab=col.legende,
	    cex.lab=taille.legende,cex.names=taille.ax,names.arg=noms1,col=col.bar,border=col.bor)
	  if (log.ax==0) {
	    par(new=TRUE)
	    barplot(proportions.mat,main="",legend=FALSE,beside=if (beside==0) {TRUE} else {FALSE},axes=FALSE,names.arg=rep('',nlevels(datas.GrapheR2[,fact1])),ylim=c(yinf,ysup),
	      log=log.axes,density=dens.motif,angle=ang.motif,col=col.motif,border=col.bor)
	  }
	  title(main=titre,col.main=col.titre,cex.main=taille.titre)
	  if (leg==1) {
	    if (nchar(leg.titre)==0) {legend(position.legende,legend=noms2,fill=col.bar)} else {
	      legend(position.legende,legend=noms2,fill=col.bar,title=leg.titre)}
	  }
	  if (any(barres.inf!=0)==TRUE & beside==0) {
	    segments(graphe,proportions.mat-barres.inf.mat,graphe,proportions.mat+barres.sup.mat,col=err.col)
	    if (err.seg==1) {
		segments(graphe-0.1,proportions.mat-barres.inf.mat,graphe+0.1,proportions.mat-barres.inf.mat,col=err.col)
		segments(graphe-0.1,proportions.mat+barres.sup.mat,graphe+0.1,proportions.mat+barres.sup.mat,col=err.col)
	    }
	  }
	}
    }
    if (Env$toolbar.GrapheR==1) {tkdestroy(Env$Toolbar)}
    tkgrab.release(fen)
    toolbar(type="barres",type.hist="",sequence="",log.ax=log.axes,ht=ht.max,abs=graphe)
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[235,1],icon="error",type="ok")}
}

